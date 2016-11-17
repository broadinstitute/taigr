taiga.url <- "http://taiga.broadinstitute.org"
data.dir <- "~/.taiga"
data.id <- "80833526-448b-4b9f-a661-a9407a67e778"
data.name <- "ccle-sample-info"
data.version <- 1

context("Exception handling")

test_that("missing name and id throws error", {
    expect_error(load.from.taiga(),
                 "Error: must supply either data.id or data.name")
    expect_error(load.from.taiga(data.version=1),
                 "Error: must supply either data.id or data.name")
    expect_error(load.from.taiga(quiet=T),
                 "Error: must supply either data.id or data.name")
})

test_that("name with missing version throws warning", {
    data.name <- "ccle-sample-info"
    expect_warning(load.from.taiga(data.name="ccle-sample-info",quiet=T),
                   "Warning: will only cache using id")
})


context("Cache")

test_that("data.dir is created if it doesn't exist", {
    data.dir <- "~/.taiga.tests"
    if (file.exists(data.dir)) unlink(data.dir, recursive=TRUE)
    data <- load.from.taiga(data.name=data.name,
                            data.version=data.version,
                            data.dir=data.dir,
                            quiet=T)
    expect_true(file.exists(data.dir))
    unlink(data.dir, recursive=TRUE)
})


context("File and Source naming")

test_that("naming source/file by name and version works", {
    data.source <- paste(taiga.url, "/rest/v0/namedDataset?",
                         "fetch=content&format=rdata&name=ccle-sample-info",
                         "&version=1",sep="")
    data.file <- paste("~/.taiga/ccle-sample-info_1.Rdata")
    expect_equal(make.name.source(taiga.url, data.name, data.version),
                 data.source)
    expect_equal(make.name.file(data.dir, data.name, data.version),
                 data.file)
})

test_that("naming source/file by id works", {
    data.source <- paste(taiga.url, "/rest/v0/datasets/",
                         "80833526-448b-4b9f-a661-a9407a67e778",
                         "?format=rdata",sep="")
    data.file <- paste("~/.taiga/80833526-448b-4b9f-a661-a9407a67e778.Rdata")
    expect_equal(make.id.source(taiga.url, data.id),
                 data.source)
    expect_equal(make.id.file(data.dir, data.id),
                 data.file)
})

context("Low-level functions fetching data")

test_that("retrive id from dataset name", {
    expect_equal(get.data.id(taiga.url,data.name,data.version),
                 data.id)
})

test_that("loading by name works", {
    expect_is(load.using.name(data.dir,data.name,data.version,
                              taiga.url,force.taiga=TRUE,quiet=TRUE),
              "matrix")
    expect_is(load.using.name(data.dir,data.name,data.version,
                              taiga.url,force.taiga=FALSE,quiet=TRUE),
              "matrix")
})

test_that("loading by id works", {
    expect_is(load.using.id(data.dir,data.id,
                            taiga.url,force.taiga=TRUE,quiet=TRUE),
              "matrix")
    expect_is(load.using.id(data.dir,data.id,
                            taiga.url,force.taiga=FALSE,quiet=TRUE),
              "matrix")
})

context("Low-level functions saving data")

test_that("save by name works", {
    data.file <- make.name.file(data.dir,data.name,data.version)
    if (file.exists(data.file)) file.remove(data.file)
    data <- load.using.name(data.dir,data.name,data.version,
                            taiga.url,force.taiga=TRUE,quiet=TRUE)
    save.using.name(data, data.dir,data.name,data.version,quiet=TRUE)
    expect_true(file.exists(data.file))
    expect_gt(file.info(data.file)$size,0)
})

test_that("save by id works", {
    data.file <- make.id.file(data.dir,data.id)
    if (file.exists(data.file)) file.remove(data.file)
    data <- load.using.id(data.dir,data.id,
                          taiga.url,force.taiga=TRUE,quiet=TRUE)
    save.using.id(data, data.dir,data.id,quiet=TRUE)
    expect_true(file.exists(data.file))
    expect_gt(file.info(data.file)$size,0)
})

context("load.from.taiga pulls/puts data from/to all the right places")

data.name.file <- make.name.file(data.dir,data.name,data.version)
data.id.file <- make.id.file(data.dir,data.id)
data.name.source <- make.name.source(taiga.url,data.name,data.version)
data.id.source <- make.id.source(taiga.url,data.id)

test_that("loads from right spot", {
    expect_message(load.from.taiga(data.name=data.name,
                                data.version=data.version, force.taiga=TRUE),
                   data.name.source, fixed=TRUE)

    expect_message(load.from.taiga(data.id=data.id, force.taiga=TRUE),
                   data.id.source, fixed=TRUE)

    expect_message(load.from.taiga(data.name=data.name, force.taiga=TRUE),
                   data.id.source, fixed=TRUE)

    expect_message(load.from.taiga(data.name=data.name,
                                   data.version=data.version, no.save=TRUE),
                   data.name.file, fixed=TRUE)

    expect_message(load.from.taiga(data.id=data.id, no.save=TRUE),
                   data.id.file, fixed=TRUE)

    expect_message(load.from.taiga(data.name=data.name, no.save=TRUE),
                   data.id.file, fixed=TRUE)

    file.remove(data.name.file)

    expect_message(load.from.taiga(data.name=data.name,
                                   data.version=data.version,no.save=TRUE),
                   data.id.file, fixed=TRUE)

})

test_that("saves to the right spot", {

    if (file.exists(data.name.file)) file.remove(data.name.file)

    expect_message(load.from.taiga(data.name=data.name,
                                   data.version=data.version),
                   data.name.file, fixed=TRUE)
    expect_true(file.exists(data.name.file))

    if (file.exists(data.id.file)) file.remove(data.id.file)

    expect_message(load.from.taiga(data.name=data.name,
                                   data.version=data.version, cache.id=TRUE),
                   data.id.file, fixed=TRUE)
    expect_true(file.exists(data.id.file))

    file.remove(data.id.file)

    expect_message(load.from.taiga(data.id=data.id),
                   data.id.file, fixed=TRUE)
    expect_true(file.exists(data.id.file))

    file.remove(data.id.file)

    expect_message(load.from.taiga(data.name=data.name),
                   data.id.file, fixed=TRUE)
    expect_true(file.exists(data.id.file))
})
