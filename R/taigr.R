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

######## TAIGA1 methods

#' Visit Taiga page in web browser
#'
#' @param data.id The dataset ID in taiga.
#' @param data.name The dataset name in taiga.
#' @param data.version The dataset version number in taiga.
#' @param taiga.url Where is taiga?
#' @return the full URL
#' @export
#'
#'

visit.taiga.page <- function(data.id = NULL,
                            data.name = NULL,
                            data.version = NULL,
                            taiga.url = getOption("default.taiga.url",
                                "https://cds.team/taiga"),
                            taiga.api.version=getOption("default.taiga.api.version", 2)
                            ) {

    if (is.null(data.id) && is.null(data.name)) {
        stop("Error: must supply either data.id or data.name")
    }

    if(taiga.api.version == 1) {
        if (is.null(data.id)) {
            data.id <- get.data.id(taiga.url = taiga.url,
                                   data.name = data.name, data.version = data.version)
        }

        data.url <- paste(taiga.url, "/dataset/show/", data.id, sep="")
    } else {
        stopifnot(taiga.api.version == 2)

        if (is.null(data.id)) {
            data.url <- paste0(taiga.url, "/dataset/", data.name, "/", data.version )
        } else {
            data.url <- paste0(taiga.url, "/dataset/x/", data.id )
        }
    }

    cat("opening", data.url)
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
#' @param data.file file to load from within the dataset
#' @return The dataset loaded into your R session.
#' @export
load.from.taiga <- function(data.id = NULL,
                            data.name = NULL,
                            data.version = NULL,
                            transpose = FALSE,
                            data.dir = "~/.taiga",
                            force.taiga = FALSE,
                            taiga.url = getOption("default.taiga.url",
                                "https://cds.team/taiga"),
                            cache.id = FALSE,
                            no.save = FALSE,
                            quiet = FALSE,
                            taiga.api.version=getOption("default.taiga.api.version", 2),
                            force.convert=F,
                            data.file=NULL) {

    if(taiga.api.version!=1) {
        return(load.from.taiga2(data.id=data.id, data.name=data.name, data.version=data.version, transpose=transpose, data.dir=data.dir, force.taiga=force.taiga, taiga.url=taiga.url, cache.id=cache.id, no.save=no.save, quiet=quiet, force=force.convert, data.file=data.file))
    }

    if(!is.null(data.file)) {
        stop("Taiga1 does not support a non-null data.file parameter")
    }

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

#### TAIGA2 methods below

have_access_to_taiga <- function(){
    have_access <- FALSE
    tryCatch(
        {
            response <- httr::GET("https://cds.team/taiga", httr::timeout(1))
            if(response$status_code != 503){
                have_access <- TRUE
            }
        },
        error = function(e){
            have_access <- FALSE
        }
    )
    return(have_access)
}

taiga2.get.datafile <- function(taiga.url, data.id, data.name, data.version, data.file, force, format, token) {
    stopifnot(length(token) == 1)

    url <- paste0(taiga.url, "/api/datafile?format=", format)
    if(! is.null(data.id)) {
        url <- paste0(url, "&dataset_version_id=", data.id)
    } else {
        url <- paste0(url, "&dataset_permaname=", RCurl::curlEscape(data.name))
        if(!is.null(data.version)) {
            url <- paste0(url, "&version=", RCurl::curlEscape(data.version))
        }
    }
    if(!is.null(data.file)) {
        url <- paste0(url, "&datafile_name=", RCurl::curlEscape(data.file))
    }

    if(force) {
        url <- paste0(url, "&force=Y")
    }

    fetch.json(url, token)
}

fetch.json <- function(url, token) {
    cat("Fetching", url, "\n")
    resp <- httr::GET(url, httr::add_headers(Authorization=paste0("Bearer ",token)))
    status <- resp$status_code
    cat("Status", status,"\n")
    if(status == 500) {
        stop("internal server error")
    }
    if(status != 404) {
       response <- jsonlite::fromJSON(httr::content(resp, as="text"))
    } else {
       response <- list()
    }

#    h = RCurl::basicTextGatherer()
#    response.json <- RCurl::getURL(url, headerfunction = h$update, httpheader = c(Authorization=paste0("Bearer ", token)), .mapUnicode=F)
#    status_line <- h$value(NULL)[1]
#    status <- as.integer(strsplit(status_line, " ")[[1]][2])
#    cat("Status", status,"\n")
#    if(status == 500) {
#        stop("internal server error")
#    }
#
#    response <- jsonlite::fromJSON(response.json)

    response$http_status <- status

    response
}

request.files.from.taiga2 <- function(data.id, data.name, data.version, data.file, taiga.url, force,
token, format="rds")
{
    first.attempt <- T
    prev.status <- NULL
    delay.between.polls <- 1
    waiting.for.conversion <- T
    while(waiting.for.conversion) {
        response <- taiga2.get.datafile(taiga.url, data.id, data.name, data.version, data.file, force,
format, token)
        force <- F

        if(is.null(response$urls)) {
            if(first.attempt) {
                cat("Taiga needs to convert data to rds before we can fetch it.  Waiting...\n")
                cat(response$status, "\n")
            } else {
                if(prev.status != response$status) {
                    cat(response$status, "\n")
                }
            }
            prev.status <- response$status

            first.attempt <- F
            Sys.sleep(delay.between.polls)
            delay.between.polls <- min(delay.between.polls * 1.5, 10)
        } else {
            waiting.for.conversion <- F
        }
    }

    filenames <- sapply(response$urls, function(url) {
        message(paste0("Downloading ", url," ..."))
        dest <- tempfile()
        httr::GET(url, httr::write_disk(dest, overwrite=TRUE))
        # leaving off method results in 403 error (??)
        #download.file(url, dest, method='curl')
        dest
    } )

    list(filenames=filenames, datafile.name=response$datafile_name, data.id=response$dataset_version_id, data.name=response$dataset_permaname, data.version=response$dataset_version)
}

load.from.multiple.rds <- function(filenames) {
    combined <- do.call(rbind, lapply(filenames, readRDS))
    if(is.data.frame(combined)) {
        rownames(combined) <- NULL
    }
    combined
}

save.to.taiga2.cache <- function(data.id, data.name, data.version, datafile.name, data.dir, data) {
    # mkdir for rds files
    # move from temp location to rds location
    # write out file called data.name+data.version + ".idx" and one called data.id + ".idx"

    stopifnot(!is.null(data.id))
    stopifnot(!is.null(data.name))
    stopifnot(!is.null(data.version))
    stopifnot(!is.null(data.dir))
    stopifnot(!is.null(datafile.name))
    stopifnot(!is.null(data))

    if(!dir.exists(data.dir)) {
        dir.create(data.dir)
    }

    normalized.datafile.name <- normalize.name(datafile.name)

    data.file = paste0(data.id, "_", normalized.datafile.name, ".rds")

    saveRDS(data, paste0(data.dir, "/", data.file))
    message(paste0("Saved to cache as ", data.file))


    index.file.names <- c(
        paste0(data.dir, '/', data.id, "_", normalized.datafile.name, ".idx"),
        paste0(data.dir, '/', data.name, "_", normalized.datafile.name, "_", data.version, ".idx")
    )

    cat("writing", index.file.names, "\n")
    for(fn in index.file.names) {
        writeLines(data.file, fn)
    }
}

normalize.name <- function(x) {
    orig <- x
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "-", x)
    x <- gsub("-+", "-", x)
    x
}

load.from.taiga2.cache <- function(data.id, data.name, data.version, datafile.name, data.dir) {
    stopifnot(!is.null(datafile.name))
    normalized.datafile.name <- normalize.name(datafile.name)

    # with the addition of some objects are broken into multiple rds files, we now have an
    # extra layer of indirection.  We look up a ".idx" file which contains a list of rds filenames
# that should be rbind'ed load the datafile.
    if(!is.null(data.id)) {
        idx.file <- paste0(data.dir, '/', data.id, "_", normalized.datafile.name, ".idx")
    } else {
        stopifnot(!is.null(data.name) & !is.null(data.version))
        idx.file <- paste0(data.dir, '/', data.name, "_", normalized.datafile.name, "_", data.version, ".idx")
    }

    if(file.exists(idx.file)) {
        # the combining is presently happening before addition to the cache, so I don't think
        # handling multiple lines in file is necessary.
        filenames <- readLines(idx.file)
        filenames <- sapply(filenames, function(x) paste0(data.dir, '/', x))

        message(paste0("Loading from cached file ", filenames))
        return(load.from.multiple.rds(filenames))
    } else {
        return (NULL)
    }
}

taiga2.get.dataset.version <- function(taiga.url, data.id, data.name, data.version, token) {
    if (!is.null(data.id)) {
        url <- paste0(taiga.url, '/api/dataset/x/', data.id)
    } else {
        url <- paste0(taiga.url, '/api/dataset/', data.name, '/', data.version)
    }
    #cat("taiga2.get.dataset.version: ", url, "\n")
    r <- fetch.json(url, token)
    #str(r)
    r
}

taiga2.get.cached.dataset.version <- function(data.dir, data.id, data.name, data.version) {
    if(!is.null(data.id)) {
        fn <- paste0(data.dir,"/",data.id,".toc")
    } else if(!is.null(data.name)) {
        stopifnot(!is.null(data.version))
        fn <- paste0(data.dir, "/", data.name, "_", data.version, ".toc")
    }
    response <- NULL
#    cat("checking for ", fn, "\n")
    if(file.exists(fn)) {
        cat("loading cached data version from ", fn, "\n")
        response <- readRDS(fn)
    }
    response
}

taiga2.cache.dataset.version <- function(data.dir, data.id, data.name, data.version, response) {
    for(fn in c(paste0(data.dir,"/",data.id,".toc"), paste0(data.dir, "/", data.name, "_", data.version, ".toc"))) {
        saveRDS(response, fn)
    }
}

read.token.file <- function(data.dir) {
    find.first.token <- function() {
        possibilities <- c(".taiga-token", paste0(data.dir, "/token"))
        for (token.filename in possibilities) {
            if(file.exists(token.filename)) {
                return(token.filename)
            }
        }
        stop(paste0("Could not find token to use for authentication!  Please put your user token into one of: ", paste(collapse=possibilities)))
    }

    token.filename <- find.first.token()
    token <- readLines(token.filename)
    # only keep first line in case there's extra whitespace
    token <- token[1]
}

taiga2.resolve.id <- function(data.id, data.name, data.version, data.dir,
force.taiga, taiga.url, cache.id, quiet, data.file, force.convert, no.save, token) {
    # make sure only data.id or data.name is provided
    if(!is.null(data.id)) {
        dataset.description <- data.id
        stopifnot(is.null(data.version) & is.null(data.name))

        # does data.id include a filename?
        index.of.slash <- regexpr("/", data.id)
        if(index.of.slash >= 1) {
            # if so, we want to split off the filename from the data.id
            stopifnot(is.null(data.file))
            data.file <- substring(data.id, index.of.slash+1)
            data.id <- substring(data.id, 1, index.of.slash-1)
        }

        # now, data.id may be a real id, or it may be a permaname + "." + version number
        if(length(grep("[^.]+\\.\\d+", data.id)) == 1) {
            id.parts <- strsplit(data.id, "\\.")[[1]]
            data.id <- NULL
            data.name <- id.parts[1]
            data.version <- as.numeric(id.parts[2])
        }

        # maybe put in a warning here if data.id looks like it is actually a permaname?
    }

    if(!is.null(data.name)) {
        if(!is.null(data.version)) {
            dataset.description <- paste0(data.name, " v", data.version, sep="")
        } else {
            dataset.description <- data.name
        }
        stopifnot(is.null(data.id))
    }

    if((!is.null(data.name) && !is.null(data.version)) || !is.null(data.id)) {
        # if it's possible to rely on the cache go that route.  (Only possible when we're asking for a specific version, not latest version)
        response <- NULL
        if(!force.taiga && !force.convert) {
            # We update the dataset version metadata if we have access to internet
            response_from_website <- NULL
            if(have_access_to_taiga()){
                response_from_website <- taiga2.get.dataset.version(taiga.url, data.id, data.name, data.version, token)
                if(response_from_website$http_status == "404") {
                    warning("No such dataset, load.from.taiga returning NULL")
                    return(NULL)
                } else if(response_from_website$http_status != "200") {
                    stop(paste0("Request for dataset failed, status: ", response_from_website$status))
                }
            }
            else{
                warning("You are in offline mode, please be aware that you might be out of sync with the state of the dataset version (deprecatio")
            }
            response <- taiga2.get.cached.dataset.version(data.dir, data.id, data.name, data.version)
        }

        # if could not get from cache, contact taiga
        if(is.null(response)) {
            response <- taiga2.get.dataset.version(taiga.url, data.id, data.name, data.version, token)
            if(response$http_status == "404") {
                warning("No such dataset, load.from.taiga returning NULL")
                return(NULL)
            } else if(response$http_status != "200") {
                stop(paste0("Request for dataset failed, status: ", response$status))
            }

            # if we allow caching, now save it
            if(!no.save) {
                taiga2.cache.dataset.version(data.dir, data.id, data.name, data.version, response)
            }
        } else {
            # Update dataset version state based on website if not NULL
            if(!is.null(response_from_website)){
                response$datasetVersion$state <- response_from_website$datasetVersion$state
                response$datasetVersion$reason_state <- response_from_website$datasetVersion$reason_state
            }
        }

        data.name <- response$dataset$permanames[1]
        data.id <- response$datasetVersion$id
        data.version <- response$datasetVersion$version

        # Get the state of the datasetVersion
        data.state <- response$datasetVersion$state
        data.reason_state <- response$datasetVersion$reason_state

        if(data.state == 'deprecated'){
            message = paste("This dataset version is deprecated. Please use with caution. Reason for deprecation:",
                            data.reason_state,
                            sep = "\n\t")
            warning(message)
        }

        # now look for the file within the selected version
        if(is.null(data.file)) {
            data.file <- response$datasetVersion$datafiles$name[1]
        } else {
            found <- FALSE
            #print("response")
            #print(response)
            for (dfname in response$datasetVersion$datafiles$name) {
                if(dfname == data.file) {
                    found <- TRUE
                }
            }
            if(!found) {
                stop(paste0("No data file named ", data.file, " in dataset"))
            }
        }
    } else {
        response <- taiga2.get.datafile(taiga.url, data.id, data.name, data.version, data.file, force.convert, "metadata", token)
        if(response$http_status == "404") {
            warning("No such datafile, load.from.taiga returning NULL")
            return(NULL)
        } else if(response$http_status != "200") {
            stop(paste0("Request for metadata failed, status: ", response$status))
        }

        data.id <- response$dataset_version_id
        data.name <- response$dataset_permaname
        data.version <- response$dataset_version
        data.file <- response$datafile_name
    }

    return( list(data.id=data.id, data.name=data.name,
        data.version=data.version, data.file=data.file))
}

#' Download a "raw" file to cache directory.
#' @return the file path to the downloaded file
#' @export download.raw.from.taiga
download.raw.from.taiga <- function(data.id = NULL,
                            data.name = NULL,
                            data.version = NULL,
                            data.dir = "~/.taiga",
                            force.taiga = FALSE,
                            taiga.url = getOption("default.taiga.url",
                                "https://cds.team/taiga"),
                            quiet = FALSE,
                            data.file = NULL) {

    token <- read.token.file(data.dir)

    result <- taiga2.resolve.id(data.id, data.name, data.version, data.dir,
force.taiga, taiga.url, cache.id, quiet, data.file, FALSE, FALSE, token)
    if(is.null(result)) {
        return(NULL)
    }

    data.id <- result$data.id
    data.name <- result$data.name
    data.version <- result$data.version
    data.file <- result$data.file

    stopifnot(!is.null(data.id))
    stopifnot(!is.null(data.name))
    stopifnot(!is.null(data.version))
    stopifnot(!is.null(data.file))

    normalized.datafile.name <- normalize.name(data.file)
    if(!is.null(data.id)) {
        cached.file <- paste0(data.dir, '/', data.id, "_", normalized.datafile.name,".raw")
    } else {
        stopifnot(!is.null(data.name) & !is.null(data.version))
        cached.file <- paste0(data.dir, '/', data.name, "_", normalized.datafile.name, "_", data.version, ".raw")
    }

    cat("Checking for cached file", cached.file, "\n")
    if(!file.exists(cached.file)) {
        print(token)
        result <- request.files.from.taiga2(data.id = data.id, data.name=data.name, data.version=data.version, data.file=data.file, taiga.url=taiga.url, force=FALSE,
        token=token, format="raw")

        stopifnot(length(result$filenames) == 1)
        file.rename(result$filenames[[1]], cached.file)
    }

    return (cached.file)
}

load.from.taiga2 <- function(data.id = NULL,
                            data.name = NULL,
                            data.version = NULL,
                            transpose = FALSE,
                            data.dir = "~/.taiga",
                            force.taiga = FALSE,
                            taiga.url = "http://datasci-dev:8999",
                            cache.id = FALSE,
                            no.save = FALSE,
                            quiet = FALSE,
                            data.file = NULL,
                            force.convert=F) {
    if(!is.null(data.id)) {
        dataset.description <- data.id
    }
    if(!is.null(data.name)) {
        if(!is.null(data.version)) {
            dataset.description <- paste0(data.name, " v", data.version, sep="")
        } else {
            dataset.description <- data.name
        }
    }

    token <- read.token.file(data.dir)
    result <- taiga2.resolve.id(data.id, data.name, data.version, data.dir,
force.taiga, taiga.url, cache.id, quiet, data.file, force.convert, no.save,
token)
    if(is.null(result)) {
        return(NULL)
    }

    data.id <- result$data.id
    data.name <- result$data.name
    data.version <- result$data.version
    data.file <- result$data.file

    stopifnot(!is.null(data.id))
    stopifnot(!is.null(data.name))
    stopifnot(!is.null(data.version))
    stopifnot(!is.null(data.file))

    # first check cache
    data <- NULL
    if(!force.taiga && !force.convert) {
        data <- load.from.taiga2.cache(data.id = data.id, data.name=data.name, data.version=data.version, data.dir=data.dir, datafile.name = data.file)
    }

    if(is.null(data)) {
        # if not in cache, pull and optionally store in cache
        cat("Could not find", dataset.description, "in cache, requesting from taiga...\n")
        result <- request.files.from.taiga2(data.id = data.id, data.name=data.name, data.version=data.version, data.file=data.file, taiga.url=taiga.url, force=force.convert, token=token)

        data <- load.from.multiple.rds(result$filenames)

        if(!no.save) {
            cat("Saving", dataset.description, "in cache (", result$data.id, result$datafile.name, ")...\n")
#            print(result)
#            cat("result ",is.null(result), "\n")
            save.to.taiga2.cache(data.id=result$data.id,
                                 data.name=result$data.name,
                                 data.version=result$data.version,
                                 datafile.name=result$datafile.name,
                                 data.dir=data.dir,
                                 data=data)
        }
    }

    stopifnot(!is.null(data))

    if(transpose) {
        t(data)
    } else {
        data
    }
}

