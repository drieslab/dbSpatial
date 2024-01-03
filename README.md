
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbSpatial

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dbSpatial)](https://CRAN.R-project.org/package=dbSpatial)
<!-- badges: end -->

The goal of `{dbSpatial}` is to provide convenience functions in R for
the [DuckDB spatial
extension](https://duckdb.org/docs/extensions/spatial.html) as well as
compatibility with [`{terra}`](https://github.com/rspatial/terra) and
other R spatial objects with a DuckDB backend.

## Installation

You can install the development version of dbSpatial from Github like
so:

``` r
# install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
pak::pak("ed2uiz/dbSpatial")
```

## Usage

The DuckDB spatial extension can be used with wrapper functions in this
package directly (`ST_*()`), through dplyr verbs containing `ST_*()`
functions, or SQL queries to a DuckDB database connection containing
`ST_*()` functions.

``` r
library(dbSpatial)
#> 
#> Attaching package: 'dbSpatial'
#> The following object is masked from 'package:base':
#> 
#>     nrow

# create duckdb db in memory
duckdb_conn = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# install and load duckdb spatial extension
loadSpatial(conn = duckdb_conn)
#> Duckdb spatial extension installed and loaded

# use ST_*() functions directly in R
# TODO

# through dplyr verbs
# TODO

# through SQL queries
# TODO
```
