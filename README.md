# taigr

Light weight R package for loading datasets from taiga. Conveniently caches data to your hard disk.

The latest version of this library is a thin wrapper around `taigaclient` which is part of [taigapy](https://github.org/broadinstitute/taigapy). As a result the first step is to install taigapy into a python environment. 

Prerequistes: 
1. conda: You can download miniconda from https://docs.anaconda.com/free/miniconda/miniconda-install/ and the instructions below will use the `conda` command to set up the python environment in which we'll install taigapy.
2. git: Can be downloaded from https://git-scm.com/downloads and is used by some of the steps below to pull code from github.
3. The R devtools package: Can be installed via `install.packages('devtools')`

## Installation

### 1. Create a python environment and installing taigapy into it:

```
conda create -n taigapy python=3.9
conda activate taigapy
pip install git+https://github.com/broadinstitute/taigapy.git
```

### 2. Install taigr in an R session

```R
library(devtools)
devtools::install_github("https://github.com/broadinstitute/taigr")
```

### 3. Set up token

You need to get your authorization token so the client library can make requests on your behalf. Go to https://cds.team/taiga/token/ and click on the "Copy" button to copy your token. Paste your token in a file at `~/.taiga/token`. You can do this in R via:

On MacOS:

```R
dir.create(path.expand("~/.taiga"))
write("YOUR_TOKEN_HERE", file=path.expand("~/.taiga/token"))
```

On Windows (write it to two locations to be safest):
```
token <- "YOUR_TOKEN_HERE"
home.dir <- Sys.getenv("USERPROFILE")
dir.create(file.path(home.dir, ".taiga"))
write(token, file=file.path(home.dir, ".taiga", "token"))
home.dir <- Sys.getenv("HOME")
dir.create(file.path(home.dir, ".taiga"))
write(token, file=file.path(home.dir, ".taiga", "token"))
```


### 4. Set up path to taigaclient

`taigr` needs to know where to file the `taigaclient` executable that got installed when `taigapy` was installed. You can tell it by setting the `taigaclient.path` option in R. 

In MacOS, this would typically be:
```
options(taigaclient.path=path.expand("~/miniconda3/envs/taigapy/bin/taigaclient"))
```

Or in Windows, this would typically be: 
```
options(taigaclient.path=file.path(Sys.getenv("USERPROFILE"), "miniconda3/envs/taigapy/Scripts/taigaclient"))
```

( You may want to put this command in your ".Rprofile" so that it automatically executes each time R starts up as described in https://www.statmethods.net/interface/customizing.html ) 

At this point taigr should be successfully set up and you can test it out by running:

```
taigr::load.from.taiga("taigr-data-40f2.7/tiny_matrix")
```

( If you get an error like `The following command reported an error...` the first thing to check is whether the command it's reporting failed does actually exist at that location. )

If successful, it should have retreived the 2x3 matrix which was listed at https://cds.team/taiga/dataset/taigr-data-40f2/7


## Installation in a docker container

Similarly, install taigapy and then install taigr from a source bundle to avoid needing to install devtools

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