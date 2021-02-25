# taigr

Light weight R package for loading datasets from taiga. Conveniently caches data to your hard disk.

## Token set up

First, you need to get your authorization token so the client library can make requests on your behalf. Go to https://cds.team/taiga/token/ and click on the "Copy" button to copy your token. Paste your token in a file at `~/.taiga/token`.

```
mkdir ~/.taiga/
echo YOUR_TOKEN_HERE > ~/.taiga/token
```

## Installation
Within the Broad, to install the last 'released' version:

```
options(repos = c(
	"https://iwww.broadinstitute.org/~datasci/R-packages",
	"https://cran.cnr.berkeley.edu"))
# For R version >4.0.0 instead use
options(repos = c(
      "https://iwww.broadinstitute.org/~datasci/R-packages",
      "https://cran.rstudio.com/"))
install.packages('taigr')
```

To install a development version from git:

```
```

```
$ git clone https://github.com/broadinstitute/taigr.git
$ R
> library(devtools)
> install_git(url="https://github.com/broadinstitute/taigr.git")
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
