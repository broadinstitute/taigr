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
        do.call(load.from.taiga, i)
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
                            data.dir = path.expand("~/.taiga"),
                            force.taiga = FALSE,
                            taiga.url = getOption("default.taiga.url",
                                "https://cds.team/taiga"),
                            cache.id = FALSE,
                            no.save = FALSE,
                            quiet = FALSE,
                            taiga.api.version=getOption("default.taiga.api.version", 2),
                            force.convert=F,
                            data.file=NULL) {

    stopifnot(taiga.api.version!=1);

    return(load.from.taiga2(data.id=data.id, data.name=data.name, data.version=data.version, transpose=transpose, data.dir=data.dir, force.taiga=force.taiga, taiga.url=taiga.url, cache.id=cache.id, no.save=no.save, quiet=quiet, force=force.convert, data.file=data.file))
}

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
        message(paste0("Downloading ", data.name, "/v", data.version, "/", data.file," ..."))
        dest <- tempfile()
        httr::GET(url, httr::write_disk(dest, overwrite=TRUE), httr::progress())
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
        possibilities <- c(file.path(getwd(), ".taiga-token"), file.path(data.dir, "token"))
        for (token.filename in possibilities) {
            if(file.exists(token.filename)) {
                return(token.filename)
            }
        }
        stop(paste0("Could not find token to use for authentication!  Please put your user token into one of: ", paste(possibilities, collapse=", ")))
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
                warning("You are in offline mode, please be aware that you might be out of sync with the state of the dataset version (deprecation)")
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
                stopifnot(!is.null(response_from_website$datasetVersion$state))
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

        stopifnot(!is.null(data.state))
        if(data.state == 'deprecated'){
            message = paste("This dataset version is deprecated. Please use with caution. Reason for deprecation:",
                            data.reason_state,
                            sep = "\n\t")
            warning(message)
        } else if (data.state == 'deleted') {
            # Not show warnings
            oldw <- getOption("warn")
            options(warn = -1)

            # Removing data from cache
            # TODO: Currently removing all data from the dataset. Should only remove data from the specific dataset version
            # Remove data.name based
            pattern_remove <- paste0(data.name, '_', '*')
            to_delete <- dir(path=paste0(data.dir, '/'), pattern=pattern_remove)

            path_file_remove <- paste(data.dir, to_delete, sep='/')
            file.remove(path_file_remove)

            # Remove data.id based
            pattern_remove <- paste0(data.id, '_', '*')
            to_delete <- dir(path=paste0(data.dir, '/'), pattern=pattern_remove)

            path_file_remove <- paste(data.dir, to_delete, sep='/')
            file.remove(path_file_remove)

            # Restore warnings
            options(warn = oldw)

            # Stop program and notify user
            stop("This version of the dataset has been deleted. Contact its maintainer for more information.")
        }

        # now look for the file within the selected version
        if(is.null(data.file)) {
            warning("No filename passed, getting the first one by default")
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

    if (! (force.taiga && no.save)) {
        if (! file.exists(data.dir)) {
            print("B")
            dir.create(data.dir)
        }
        stopifnot(file.exists(data.dir))
    }

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
            save.to.taiga2.cache(data.id=data.id,
                                 data.name=data.name,
                                 data.version=data.version,
                                 datafile.name=data.file,
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

#' Get all type of data and set them by `filename` in the hash passed in parameter
#' `get_agnostic_data` doesn't return anything, the hash is mutated
#' @param data.file Datafile object received from taiga (not the data)
#' @param datasetVersion.id Id of the datasetVersion
#' @param dataset.name Name of the dataset
#' @param dataset.version Version number of the datasetVersion
#' @param taiga.url Url to contact Taiga
#' @param force Boolean to ask Taiga if it needs to do the conversion again (in case of failure or interruption)
#' @param token Path of the location of the token file containing the Taiga token of the user
#' @param dict_filenames_data Hash which is going to contain the data in the form `filename`: `data`
#' @return This function does not return anything, but mutate a hash object
#' @import hash
get_agnostic_data <- function(data.file,
                              datasetVersion.id,
                              dataset.name,
                              dataset.version,
                              taiga.url,
                              force=FALSE,
                              token=token,
                              dict_filenames_data) {
    filename <- data.file$name
    if (data.file$allowed_conversion_type[1]  == 'raw') {
        warning(paste(filename, 'is raw. You will have a local path as data'))

        data <- download.raw.from.taiga(data.name = dataset.name,
                                        data.version = dataset.version,
                                        force.taiga = force,
                                        taiga.url = taiga.url,
                                        data.file = filename)
    } else {
        result <- request.files.from.taiga2(data.id=datasetVersion.id,
                                            data.name = dataset.name,
                                            data.version=dataset.version,
                                            data.file=filename,
                                            taiga.url=taiga.url,
                                            force=force,
                                            token=token)
        data <- load.from.multiple.rds(result$filenames)
    }
    .set(dict_filenames_data, keys=c(filename), values=data)
}

#' Function to retrieve all the data available in a specific datasetVersion
#' @param datasetVersion.id DatasetVersion id
#' @param dataset.name Permaname of the dataset we want to retrieve data from
#' @param dataset.version Version of the dataset
#' @param transpose Will transpose all the matrices
#' @param cache.dir Path to the directory where to put your data in. !Not recommended to modify this
#' @param force.taiga Boolean to force the download of the data from Taiga instead of using the cache (if you modified the data for example)
#' @param taiga.url Url to taiga. !Not recommended to change this
#' @param cache.id Id of the cache used to store the file
#' @param no.save Boolean to not put the file in cache
#' @param taiga.api.version Version of the Taiga api to use. !Not receommended to change this
#' @param force.convert Boolean to force a new conversion of the data. Might be useful if the conversion has been interrupted
#' @return Hash table of the form `filename`: `data`
#' @importFrom plyr llply
#' @import hash
#' @export
load.all.datafiles.from.taiga <- function(datasetVersion.id = NULL,
                                          dataset.name = NULL,
                                          dataset.version = NULL,
                                          transpose = FALSE,
                                          cache.dir = "~/.taiga",
                                          force.taiga = FALSE,
                                          taiga.url = getOption("default.taiga.url", "https://cds.team/taiga"),
                                          cache.id = FALSE,
                                          no.save = FALSE,
                                          taiga.api.version=getOption("default.taiga.api.version", 2),
                                          force.convert=F) {
    # TODO: Use the cache, currently it redownload everytime
    # For each files found, use load.from.taiga2 if not raw (or download.raw.from.taiga if options is asked)
    if(!is.null(datasetVersion.id)) {
        dataset.description <-datasetVersion.id
    }
    if(!is.null(dataset.name)) {
        if(!is.null(dataset.version)) {
            dataset.description <- paste0(dataset.name, " v", dataset.version, sep="")
        } else {
            dataset.description <- dataset.name
        }
    }

    token <- read.token.file(cache.dir)

    # resolve.id chunks
    if(!is.null(datasetVersion.id)) {
        dataset.description <- datasetVersion.id
        stopifnot(is.null(dataset.version) & is.null(dataset.name))

        # does data.id include a filename?
        index.of.slash <- regexpr("/", datasetVersion.id)
        if(index.of.slash >= 1) {
            # if so, we want to split off the filename from the datasetVersion.id
            stopifnot(is.null(data.file))
            data.file <- substring(datasetVersion.id, index.of.slash+1)
            datasetVersion.id <- substring(datasetVersion.id, 1, index.of.slash-1)
        }

        # now, data.id may be a real id, or it may be a permaname + "." + version number
        if(length(grep("[^.]+\\.\\d+", datasetVersion.id)) == 1) {
            id.parts <- strsplit(datasetVersion.id, "\\.")[[1]]
            datasetVersion.id <- NULL
            dataset.name <- id.parts[1]
            dataset.version <- as.numeric(id.parts[2])
        }

        # maybe put in a warning here if data.id looks like it is actually a permaname?
    }

    if(!is.null(dataset.name)) {
        if(!is.null(dataset.version)) {
            dataset.description <- paste0(dataset.name, " v", dataset.version, sep="")
        } else {
            dataset.description <- dataset.name
        }
        stopifnot(is.null(datasetVersion.id))
    }

    response <- taiga2.get.dataset.version(taiga.url, datasetVersion.id, dataset.name, dataset.version, token)
    if(!no.save) {
        taiga2.cache.dataset.version(cache.dir, datasetVersion.id, dataset.name, dataset.version, response)
    }

    dataset.name <- response$dataset$permanames[1]
    datasetVersion.id <- response$datasetVersion$id
    dataset.version <- response$datasetVersion$version
    data.files <- response$datasetVersion$datafiles

    # Get the state of the datasetVersion
    data.state <- response$datasetVersion$state
    data.reason_state <- response$datasetVersion$reason_state

    # TODO: Add deprecation and deletion management
    # now look for the file within the selected version
    dict_filenames_data <- hash()
    # TODO: get_agnostic_data is mutating dict_filename_data. Might be cleaner to just return all_data and attribute to keys
    all_data <- apply(data.files, 1, function(data.file){
        get_agnostic_data(data.file=data.file,
                          datasetVersion.id=datasetVersion.id,
                          dataset.name=dataset.name,
                          dataset.version=dataset.version,
                          taiga.url=taiga.url,
                          token=token,
                          dict_filenames_data=dict_filenames_data)
    })
    # Use hash here
    dict_filenames_data
}
