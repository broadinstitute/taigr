#' taigr.
#'
#' interface with taiga including a cache functionality
#'
#' When loading data from taiga, it can look for a saved version
#' of the data object. If it doesn't find one, will load regularly from taiga
#' and write the object to disk.
#'
#' @name taigr
#' @docType package
#'
NULL



#' Visit Taiga page in web browser
#'
#' @param data.id The dataset ID in taiga.
#' @param data.name The dataset name in taiga.
#' @param data.version The dataset version number in taiga.
#' @param taiga.url Where is taiga?
#' @return the full URL
#' @export
visit.taiga.page <- function(data.id = NULL,
                            data.name = NULL,
                            data.version = NULL,
                            taiga.url = "http://datasci-dev:8999") {

    if (is.null(data.id) && is.null(data.name)) {
        stop("Error: must supply either data.id or data.name")
    }

    if (is.null(data.id)) {
        data.id <- get.data.id(taiga.url = taiga.url,
                    data.name = data.name, data.version = data.version)
    }

    data.url <- paste(taiga.url, "/dataset/show/", data.id, sep="")

    browseURL(data.url)

    return(data.url)
}

#' Prettily print taiga dataset info
#' @usage pretting.print.taiga.info(info)
#' @param info named list of arguments to load.from.taiga
#' @return NULL
#' @importFrom stringr str_replace
#' @export pretty.print.taiga.info
pretty.print.taiga.info <- function(info) {
    info <- capture.output(str(info, no.list=T))
    info <- str_replace(info, ":List of .*", "")
    info <- str_replace(info, ": (num|chr|logi)",": ")
    cat(info, sep="\n")
}

#' Load multiple datasets from taiga
#'
#' @param info named list of arguments to load.from.taiga
#' @param ... extra arguments passed to load.from.taiga
#' @return named list of datasets
#'
#' @examples
#' datasets.info <- list(
#'     cnv = list(
#'         data.name = "ccle-copy-number-variants",
#'         data.version = 1),
#'     rpkm = list(
#'         data.name="ccle-rnaseq-gene-expression-rpkm-for-analysis-in-manuscripts-protein-coding-genes-only-hgnc-mapped",
#'         data.version = 3))
#' datasets <- load.all.from.taiga(datasets.info, transpose=TRUE)
#' @importFrom plyr llply
#' @export
load.all.from.taiga <- function(info, ...) {

    info <- llply(info, function(i) {
        c(i, list(...))
    })

    dataset.list <- llply(info, function(i) {
        do.call(load.from.taiga, i, )
    })

    return(dataset.list)
}

#' Load data from taiga
#'
#' @param data.id The dataset ID in taiga.
#' @param data.name The dataset name in taiga.
#' @param data.version The dataset version number in taiga.
#' @param transpose transpose the data before returning it. the cached
#'  version will not be transposed.
#' @param data.dir Where to look for and save cached version of the data.
#' @param force.taiga Force function to re-download data from taiga.
#' @param taiga.url Where is taiga?
#' @param cache.id use <id>.RData for filename instead of
#'  <name>_<version>.RData
#' @param no.save Do not save dataset to cache.
#' @param quiet Do not print messages.
#' @return The dataset loaded into your R session.
#' @export
load.from.taiga <- function(data.id = NULL,
                            data.name = NULL,
                            data.version = NULL,
                            transpose = FALSE,
                            data.dir = "~/.taiga",
                            force.taiga = FALSE,
                            taiga.url = "http://datasci-dev:8999",
                            cache.id = FALSE,
                            no.save = FALSE,
                            quiet = FALSE) {

    if (is.null(data.id) && is.null(data.name)) {
        stop("Error: must supply either data.id or data.name")
    }

    if (! (force.taiga && no.save)) {
        if (! file.exists(data.dir)) {
            dir.create(data.dir)
        }
    }

    if (is.null(data.name)) {
        # data.name is not supplied
        data <- load.using.id(data.dir, data.id, taiga.url, force.taiga, quiet)

        if (!no.save) {
            save.using.id(data, data.dir, data.id, quiet)
        }
    } else {
        data.id <- NULL
        if (is.null(data.version)) {
            if (!cache.id) {
                warning(paste("Warning: will only cache using id",
                              "unless version number is supplied"))

                cache.id <- TRUE
            }
            data.id <- get.data.id(taiga.url, data.name, data.version)
            data <- load.using.id(data.dir, data.id,
                                  taiga.url, force.taiga, quiet)
        } else {
            data <- load.using.name(data.dir, data.name, data.version,
                                    taiga.url, force.taiga, quiet)
        }

        if (!no.save) {
            if (cache.id) {
                if (is.null(data.id)) {
                    data.id <- get.data.id(taiga.url, data.name, data.version)
                }
                save.using.id(data, data.dir, data.id, quiet)
            } else {
                save.using.name(data, data.dir, data.name, data.version, quiet)
            }
        }
    }

    if (transpose) {
        data <- t(data)
    }

    return(data)
}



load.using.id <- function(data.dir, data.id, taiga.url, force.taiga, quiet) {

    data.file <- make.id.file(data.dir, data.id)
    data.source <- make.id.source(taiga.url, data.id)

    if (!force.taiga && file.exists(data.file)) {
        if (!quiet) message("Loading from disk\n",data.file)
        load(data.file)
    } else {
        if (!quiet) message("Loading from Taiga\n",data.source)
        load(url(data.source))
    }
    return(data)
}


load.using.name <- function(data.dir, data.name, data.version,
                            taiga.url, force.taiga, quiet) {

    data.file <- make.name.file(data.dir, data.name, data.version)
    data.source <- make.name.source(taiga.url, data.name, data.version)

    if (!force.taiga) {
        if (file.exists(data.file)) {
            if (!quiet) message("Loading from disk\n",data.file)
            load(data.file)
            return(data)
        } else {
            # get data.id and check if that file exists
            data.id <- get.data.id(taiga.url, data.name, data.version)
            data.file.id <- make.id.file(data.dir, data.id)
            if (file.exists(data.file.id)) {
                # if it does, load it
                if (!quiet) message("Loading from disk\n",data.file.id)
                load(data.file.id)
                return(data)
            }
        }
    }

    if (!quiet) message("Loading from Taiga\n",data.source)
    load(url(data.source))

    return(data)
}


get.data.id <- function (taiga.url, data.name, data.version) {
    source <- paste(taiga.url,
                    "/rest/v0/namedDataset?fetch=id&format=rdata&name=",
                    data.name,sep='');
    if(!is.null(data.version)) {
        source <- paste(source,"&version=",data.version,sep='')
    }
    data.id <- scan(source, what=character(), quiet=TRUE)
}

save.using.id <- function(data, data.dir, data.id, quiet) {
    data.file <- make.id.file(data.dir, data.id)
    if (!file.exists(data.file)) {
        if (!quiet) message("Saving to disk",data.file)
        save(data, file=data.file)
    }
}

save.using.name <- function(data, data.dir, data.name, data.version, quiet) {
    data.file <- make.name.file(data.dir, data.name, data.version)
    if (!file.exists(data.file)) {
        if (!quiet) message("Saving to disk\n",data.file)
        save(data, file=data.file)
    }
}

make.name.file <- function(data.dir, data.name, data.version) {
    return(file.path(data.dir,
                     paste(data.name,"_",data.version,".Rdata",sep="")))
}

make.name.source <- function(taiga.url, data.name, data.version) {
    return(paste(taiga.url,
                 "/rest/v0/namedDataset?fetch=content&format=rdata",
                 "&name=", data.name,
                 "&version=", data.version,sep=""))
}

make.id.file <- function(data.dir, data.id) {
    return(file.path(data.dir,paste(data.id,".Rdata",sep="")))
}

make.id.source <- function(taiga.url, data.id) {
    return(paste(taiga.url,
                 "/rest/v0/datasets/",data.id,"?format=rdata", sep=''))
}
