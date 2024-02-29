# taigr

Light weight R package for loading datasets from taiga. Conveniently caches data to your hard disk.

## Token set up

First, you need to get your authorization token so the client library can make requests on your behalf. Go to https://cds.team/taiga/token/ and click on the "Copy" button to copy your token. Paste your token in a file at `~/.taiga/token`.

```
mkdir ~/.taiga/
echo YOUR_TOKEN_HERE > ~/.taiga/token
```

## Installation

Even though taigr is written for R, it relies on a command line tool written
in python. You'll first need to install that before taigr will work.

Make sure you have git installed https://git-scm.com/download/win

1. Install miniconda ( https://docs.anaconda.com/free/miniconda/miniconda-install/ )
2. conda create -n taigapy python=3.9
3. conda activate taigapy
4. pip install git+https://github.com/broadinstitute/taigapy.git

Make sure `devtools` is installed in your R session and then:

```
library(devtools)
devtools::install_github("https://github.com/broadinstitute/taigr")
```

5. add to top of your script `options(taigaclient.path=...)`

On windows, I used: `options(taigaclient.path=file.path(Sys.getenv("USERPROFILE"), "miniconda3/envs/taigapy/Scripts/taigaclient"))`

Install the token via:
dir.create(file.path(Sys.getenv("HOME"), ".taiga"))
writeLines("...token...", con=file.path(Sys.getenv("HOME"), ".taiga/token"))

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
