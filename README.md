
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbSpatial

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dbSpatial)](https://CRAN.R-project.org/package=dbSpatial)
<!-- badges: end -->

The goal of `{dbSpatial}` is to provide larger-than-memory spatial
operations for various spatial data sources. The package largely relies
on the [DuckDB spatial
extension](https://duckdb.org/docs/extensions/spatial.html).

## Installation

You can install the development version of dbSpatial from Github like
so:

``` r
# install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
pak::pak("drieslab/dbSpatial")
```

## Usage

The DuckDB spatial extension can be used with wrapper functions in this
package directly (`ST_*()`), through dplyr verbs containing `ST_*()`
functions, or SQL queries to a DuckDB database connection containing
`ST_*()` functions.

``` r
library(dbSpatial)

# create duckdb db in memory
duckdb_conn = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# test data
test_data = data.frame(x = 1:10, y = 1:10, id = 1:10)

points <- dbSpatial(conn = duckdb_conn,
                    name = "test_points",
                    value = test_data,
                    x_colName = "x",
                    y_colName = "y",
                    overwrite = TRUE)

points
#> # Class:    dbSpatial 
#> # Source:   table<test_points> [10 x 4]
#> # Database: DuckDB v1.0.0 [root@Darwin 23.5.0:R 4.4.1/:memory:]
#>        x     y    id geom      
#>    <int> <int> <int> <list>    
#>  1     1     1     1 <raw [32]>
#>  2     2     2     2 <raw [32]>
#>  3     3     3     3 <raw [32]>
#>  4     4     4     4 <raw [32]>
#>  5     5     5     5 <raw [32]>
#>  6     6     6     6 <raw [32]>
#>  7     7     7     7 <raw [32]>
#>  8     8     8     8 <raw [32]>
#>  9     9     9     9 <raw [32]>
#> 10    10    10    10 <raw [32]>
```