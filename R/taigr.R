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

get.taiga.client.fetch.command <- function(
    data.id,
    data.name,
    data.version,
    data.dir,
    taiga.url,
    data.file,
    cache.format,
    tmpf
) {
    cmd <- paste(
        "taigaclient --taiga-url=",
        taiga.url,
        " --data-dir=",
        data.dir,
        " fetch --format=",
        cache.format,
        " ",
        sep=""
    )

    if (!is.null(data.id)) {
        cmd <- paste(cmd, data.id, " ", sep="")
    } else {
        cmd <- paste(
            cmd,
            "--name=",
            data.name,
            " ",
            sep=""
        )
    }

    if (!is.null(data.version)) {
        cmd <- paste(
            cmd,
            " --version=",
            data.version,
            " ",
            sep=""
        )
    }

    if (!is.null(data.file)) {
        cmd <- paste(
            cmd,
            " --file=",
            data.file,
            " ",
            sep=""
        )
    }

    cmd <- paste(cmd, "--write-filename=", tmpf, sep="")

    return(cmd)
}

#' Load data from taiga
#'
#' @param data.id The dataset ID in taiga.
#' @param data.name The dataset name in taiga.
#' @param data.version The dataset version number in taiga.
#' @param transpose transpose the data before returning it. the cached
#'  version will not be transposed.
#' @param data.dir Where to look for and save cached version of the data.
#' @param taiga.url Where is taiga?
#' @param data.file file to load from within the dataset
#' @return The dataset loaded into your R session.
#' @export
load.from.taiga <- function(data.id = NULL,
                            data.name = NULL,
                            data.version = NULL,
                            transpose = FALSE,
                            data.dir = path.expand("~/.taiga"),
                            taiga.url = getOption("default.taiga.url",
                                "https://cds.team/taiga"),
                            data.file=NULL) {

    tmpf <- tempfile(fileext = ".json")
    cmd <- get.taiga.client.fetch.command(
        data.id,
        data.name,
        data.version,
        data.dir,
        taiga.url,
        data.file,
        "feather",
        tmpf
    )
    system(cmd)
    download.info <- jsonlite::fromJSON(tmpf)
    if (download.info$error) {
        return(NULL)
    }
    df <- arrow::read_feather(download.info$filename)
    datafile_type <- download.info$datafile_type

    if (datafile_type == "HDF5") {
        df <- tibble::column_to_rownames(df, var=colnames(df)[[1]])
        df <- as.matrix(df)
    } else if (datafile_type == "Columnar") {
        df <- as.data.frame(df)
    }

    if (transpose) {
        df <- t(df)
    }

    return(df)
}


#' Download a "raw" file to cache directory.
#' @return the file path to the downloaded file
#' @export download.raw.from.taiga
download.raw.from.taiga <- function(data.id = NULL,
                            data.name = NULL,
                            data.version = NULL,
                            data.dir = "~/.taiga",
                            taiga.url = getOption("default.taiga.url",
                                "https://cds.team/taiga"),
                            data.file = NULL) {
    tmpf <- tempfile(fileext = ".json")
    cmd <- get.taiga.client.fetch.command(
        data.id,
        data.name,
        data.version,
        data.dir,
        taiga.url,
        data.file,
        "raw",
        tmpf
    )
    system(cmd)
    download.info <- jsonlite::fromJSON(tmpf)
    return(download.info$filename)
}


get.taiga.client.metadata.command <- function(
    datasetVersion.id,
    data.name,
    data.version,
    data.dir,
    taiga.url,
    data.file,
    tmpf
) {
    cmd <- paste(
        "taigaclient --taiga-url=",
        taiga.url,
        " --data-dir=",
        data.dir,
        " dataset-meta ",
        sep=""
    )

    if (!is.null(datasetVersion.id)) {
        cmd <- paste(
            cmd,
            "--version-id=",
            datasetVersion.id,
            " ",
            sep="")
    } else {
        cmd <- paste(
            cmd,
            data.name,
            " ",
            sep=""
        )
        if (!is.null(data.version)) {
            cmd <- paste(
                cmd,
                " --version=",
                data.version,
                " ",
                sep=""
            )
        }
    }


    if (!is.null(data.file)) {
        cmd <- paste(
            cmd,
            " --file=",
            data.file,
            " ",
            sep=""
        )
    }

    cmd <- paste(cmd, "--write-filename=", tmpf, sep="")

    return(cmd)
}

#' Function to retrieve all the data available in a specific datasetVersion
#' @param datasetVersion.id DatasetVersion id
#' @param dataset.name Permaname of the dataset we want to retrieve data from
#' @param dataset.version Version of the dataset
#' @param transpose Will transpose all the matrices
#' @param cache.dir Path to the directory where to put your data in. !Not recommended to modify this
#' @param taiga.url Url to taiga. !Not recommended to change this
#' @return Hash table of the form `filename`: `data`
#' @importFrom plyr llply
#' @import hash
#' @export
load.all.datafiles.from.taiga <- function(datasetVersion.id = NULL,
                                          dataset.name = NULL,
                                          dataset.version = NULL,
                                          transpose = FALSE,
                                          cache.dir = "~/.taiga",
                                          taiga.url = getOption("default.taiga.url", "https://cds.team/taiga")) {
    tmpf <- tempfile(fileext = ".json")
    cmd <- get.taiga.client.metadata.command(
        datasetVersion.id,
        dataset.name,
        dataset.version,
        cache.dir,
        taiga.url,
        NULL,
        tmpf
    ) 
    system(cmd)

    download.info <- tryCatch({
        jsonlite::fromJSON(tmpf)
    }, error = function(e) {
        print(e)
        list()
    })

    if (!is.null(download.info$datasetVersion)) {
        dataset.version.info <- download.info$datasetVersion
    } else if (!is.null(download.info$datafiles)) {
        dataset.version.info <- download.info
    } else {
        return(hash())
    }

    datafiles <- dataset.version.info$datafiles

    # TODO: Add deprecation and deletion management
    # now look for the file within the selected version
    dict_filenames_data <- hash()
    for (i in seq_along(datafiles$name)) {
        if (datafiles$type[i] == "Raw") {
            download.raw.from.taiga(
                data.id=dataset.version.info$id,
                data.dir=cache.dir,
                taiga.url=taiga.url,
                data.file=datafiles$name[i],
            )
        } else {
            hash::.set(
                dict_filenames_data,
                keys=c(datafiles$name[i]),
                values=load.from.taiga(
                    data.id=dataset.version.info$id,
                    transpose=transpose,
                    data.dir=cache.dir,
                    taiga.url=taiga.url,
                    data.file=datafiles$name[i]
                )
            )
        }
    }
    dict_filenames_data
}
