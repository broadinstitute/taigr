# taigr

Light weight R package for loading datasets from taiga. Conveniently caches data to your hard disk.


## Installation

```
library(devtools)
install_git(url="ssh://git@stash.broadinstitute.org:7999/cpds/taigr.git")
```


## Quick start

```
library(taigr)
demeter <- load.from.taiga(
	data.name = "achilles-v2-20-1-demeter-z-scores-ignoring-expression",
	data.version = 1)
```

## Open taiga URL in web browser

Useful if you want to see if there is an updated version of this dataset

```
visit.taiga.page(
	data.name = "achilles-v2-20-1-demeter-z-scores-ignoring-expression")
```

## Load many datasets at once

```
datasets.info <- list(
    CNV = list(
        data.name = "ccle-copy-number-variants",
        data.version = 1,
        transpose = T),
    RPKM = list(
        data.name="ccle-rnaseq-gene-expression-rpkm-for-analysis-in-manuscripts-protein-coding-genes-only-hgnc-mapped",
        data.version = 3,
        transpose = T),
    Demeter = list(
        data.name="achilles-v2-20-1-demeter-z-scores-ignoring-expression",
        data.version=1)
)

datasets <- load.all.from.taiga(datasets.info)
```

## Package documentation

```
package?taigr

?load.from.taiga
```
