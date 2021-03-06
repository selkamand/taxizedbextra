
<!-- README.md is generated from README.Rmd. Please edit that file -->

# taxizedbextra

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/selkamand/taxizedbextra/branch/master/graph/badge.svg)](https://app.codecov.io/gh/selkamand/taxizedbextra?branch=master)

<!-- badges: end -->

taxizedbextra wraps provides some convenience wrappers and extra utility
for the [taxizedb package](https://github.com/ropensci/taxizedb). It
assumes all taxids (taxonomyIDs) are from the ncbi taxonomy database.

Functions provided are optimized for long vectors with lots of repeated
elements. They also allow users to input taxids \<=0 so long as they
specify what they’re ‘scientific names’ should be.

## Installation

You can install the development version of taxizedbextra from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("selkamand/taxizedbextra")
```

## Usage

### Download ncbi taxonomic database

``` r
library(taxizedbextra)

## download ncbi taxonomy database (just over 2gb) (only run this once)
db_download_ncbi()

# Check where database was downloaded
locate_taxonomy_cache()
```

### Use taxizedbextra functions

``` r
library(taxizedbextra)

# convert taxids to names
taxid2name(562)
#>                562 
#> "Escherichia coli"

# convert taxids to names with custom taxid name specification for negative taxids
taxid2name(c(562, -1), special_taxid_names = c("Unknown Microbe" = -1))
#>                562                 -1 
#> "Escherichia coli"  "Unknown Microbe"

# Get lineage string
taxid2lineage(taxids = c(562))
#>                                                                                                                               562 
#> "cellular organisms>Bacteria>Proteobacteria>Gammaproteobacteria>Enterobacterales>Enterobacteriaceae>Escherichia>Escherichia coli"

# Get lineage string with ranks shown
taxid2lineage(taxids = c(562), show_ranks = TRUE)
#>                                                                                                                                                                                                            562 
#> "cellular organisms (no rank)>Bacteria (superkingdom)>Proteobacteria (phylum)>Gammaproteobacteria (class)>Enterobacterales (order)>Enterobacteriaceae (family)>Escherichia (genus)>Escherichia coli (species)"

# Customise which rank levels are included in lineage
taxid2lineage(taxids = c(562), ranks_to_include = c("superkingdom","genus", "species"))
#>                                     562 
#> "Bacteria>Escherichia>Escherichia coli"
```
