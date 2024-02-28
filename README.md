# taigr

Light weight R package for loading datasets from taiga. Conveniently caches data to your hard disk.

## Token set up

First, you need to get your authorization token so the client library can make requests on your behalf. Go to https://cds.team/taiga/token/ and click on the "Copy" button to copy your token. Paste your token in a file at `~/.taiga/token`.

```sh
mkdir ~/.taiga/
echo YOUR_TOKEN_HERE > ~/.taiga/token
```

## Installation

```R
library(devtools)
devtools::install_github("https://github.com/broadinstitute/taigr")
```

To install in a docker container:
```sh
RUN curl -L -o taigr.zip https://github.com/broadinstitute/taigr/archive/master.zip && \
    unzip taigr.zip && \
    cd taigr-master && \
    R CMD INSTALL .
```



## Quick start

```R
library(taigr)
demeter <- load.from.taiga(
	data.name = "demeter-2-data-20123df",
	data.version = 1)
```


## Package documentation

```R
package?taigr

?load.from.taiga
```

## Development

To test your changes, run the following line in R
```R
devtools::install(pkg=".")
```