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


#' Load data from taiga
#'
#' @param data.id The dataset ID in taiga.
#' @param data.name The dataset name in taiga.
#' @param data.version The dataset version number in taiga.
#' @param data.dir Where to look for and save cached version of the data.
#' @param force.taiga Force function to re-download data from taiga.
#' @param taiga.url Where is taiga?
#' @param no.save Do not save dataset to cache.
#' @param quiet Do not print messages.
#' @return The dataset loaded into your R session.
load.from.taiga <- function(data.id = NULL,
                            data.name = NULL,
                            data.version = NULL,
                            data.dir = "~/.taiga",
                            force.taiga = FALSE,
                            taiga.url = "http://datasci-dev:8999",
                            no.save = FALSE,
                            quiet = FALSE) {

    if (is.null(data.id) && is.null(data.name)) {
        stop("Error: must supply either data.id or data.name")
    }

    if (is.null(data.id)) {
        data.id <- get.data.id(taiga.url, data.name, data.version, quiet)
    }


    data.file <- file.path(data.dir,paste(data.id,".Rdata",sep=""))
    if (!force.taiga && file.exists(data.file)) {
        if (!quiet) cat("Loading from disk\n",data.file,"\n",sep="")
        load(data.file)
    } else {
        source <- paste(taiga.url,
                        "/rest/v0/datasets/",data.id,"?format=rdata", sep='');
        if (!quiet) cat("Loading from Taiga\n",source,"\n",sep="")
        load(url(source))
        if (!no.save) {
            if (!file.exists(data.dir)) dir.create(data.dir)
            save(data,file=data.file)
        }
    }

    return(data)
}


get.data.id <- function (taiga.url, data.name, data.version, quiet) {
    source <- paste(taiga.url,
                    "/rest/v0/namedDataset?fetch=id&format=rdata&name=",
                    data.name,sep='');
    if(!is.null(data.version)) {
        source <- paste(source,"&version=",data.version,sep='')
    }
    data.id <- scan(source, what=character(), quiet=quiet)
}
