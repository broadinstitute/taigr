# taigr

Light weight R package for loading datasets from taiga. Conveniently caches data to your hard disk.

## Token set up

First, you need to get your authorization token so the client library can make requests on your behalf. Go to https://cds.team/taiga/token/ and click on the "Copy" button to copy your token. Paste your token in a file at `~/.taiga/token`.

```sh
mkdir ~/.taiga/
echo YOUR_TOKEN_HERE > ~/.taiga/token
```

## Installation
Within the Broad, to install the last 'released' version:

```R
options(repos = c(
	"https://iwww.broadinstitute.org/~datasci/R-packages",
	"https://cran.cnr.berkeley.edu"))
install.packages('taigr')
```

To install a development version from git:

```
git clone https://github.com/broadinstitute/taigr.git
R CMD INSTALL .
```

Alternatively if you have a working git2r package installed (many people don't) you can install via:

```R
library(devtools)
install_git(url="https://github.com/broadinstitute/taigr.git")
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