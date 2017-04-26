taiga.url <- "https://cds.team/taiga"
mat.data.id <- "b9a6c877-37cb-4ebb-8c05-3385ff9a5ec7"
mat.data.name <- "depcon-binary-context-matrix"
df.data.id <- "0b8e280b-472c-42d4-b047-42cba9ed8e4c"
df.data.name <- "lineage-colors"
data.version <- 1

options(default.taiga.api.version=2)
options(default.taiga.url=taiga.url)

test_that("loading by id works", {
    expect_is(load.from.taiga(mat.data.id),
              "matrix")

    expect_is(load.from.taiga(df.data.id),
              "data.frame")
})

test_that("loading a specific file works", {
    expect_is(load.from.taiga(mat.data.id, data.file="data"),
              "matrix")
})

test_that("loading by name works", {
    expect_is(load.from.taiga(data.name=df.data.name),
              "data.frame")

    expect_is(load.from.taiga(data.name=df.data.name, data.version=1),
              "data.frame")

    expect_is(load.from.taiga(data.name=mat.data.name),
              "matrix")

    expect_is(load.from.taiga(data.name=mat.data.name, data.version=1),
              "matrix")
})

test_that("loading missing results in null", {
    suppressWarnings( {
    expect_null(load.from.taiga(data.id="invalid"))

    expect_null(load.from.taiga(data.name="invalid"))

    expect_null(load.from.taiga(data.name=df.data.name, data.version=100))

    expect_null(load.from.taiga(mat.data.id, data.file="invalid"))
    } )
})
