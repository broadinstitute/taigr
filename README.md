# taigr

Light weight R package for loading datasets from taiga. Conveniently caches data to your hard disk.

## Token set up

First, you need to get your authorization token so the client library can make requests on your behalf. Go to https://cds.team/taiga/token/ and click on the "Copy" button to copy your token. Paste your token in a file at `~/.taiga/token`.

```
mkdir ~/.taiga/
echo YOUR_TOKEN_HERE > ~/.taiga/token
```

## Installation

```
library(devtools)
devtools::install_github("https://github.com/broadinstitute/taigr")
```

To install in a docker container:
```
RUN curl -L -o taigr.zip https://github.com/broadinstitute/taigr/archive/master.zip && \
    unzip taigr.zip && \
    cd taigr-master && \
    R CMD INSTALL .
```



## Quick start

```
library(taigr)
demeter <- load.from.taiga(
	data.name = "demeter-2-data-20123df",
	data.version = 1)
```

## Open taiga URL in web browser

Useful if you want to see if there is an updated version of this dataset

```
visit.taiga.page(data.name = "demeter-2-data-20123df")
```

or if you have the ID, but don't know what dataset it is

```
visit.taiga.page(data.id = "3f6bc24c-1679-43c5-324112-3122")
```


## Package documentation

```
package?taigr

?load.from.taiga
```
