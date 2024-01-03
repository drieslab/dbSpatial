# Getting Started

``` r
library(dbSpatial)
```

## Introduction

This vignette demonstrates how to use the `{dbSpatial}` package to
create a duckdb database with spatial points and polygons starting from
various data sources.

## Creating dbSpatPoints

Few words about dbSpatPoints class

### From data.frames

``` r
library(DBI)

# create db connection in memory
duckdb_conn = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# test data
test_data = data.frame(x = 1:10, y = 1:10, id = 1:10)

# df, tbl
createDbSpatPoints(conn = duckdb_conn,
                   tbl = "test_points",
                   data = test_data,
                   overwrite = TRUE)
#> Duckdb spatial extension installed and loaded
#> # Source:   table<test_points> [10 x 4]
#> # Database: DuckDB 0.9.0 [root@Darwin 22.4.0:R 4.3.0/:memory:]
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

### From .csv file

``` r
# test data
test_data = data.frame(x = 1:10, y = 1:10, id = 1:10)

# write to file
write.csv(test_data, "test_data.csv", row.names = FALSE)

# load file in db
createDbSpatPoints(conn = duckdb_conn,
                   tbl = "test_points",
                   data = 'test_data.csv',
                   overwrite = TRUE)
#> Duckdb spatial extension installed and loaded
#> # Source:   table<test_points> [10 x 4]
#> # Database: DuckDB 0.9.0 [root@Darwin 22.4.0:R 4.3.0/:memory:]
#>        x     y    id geom      
#>    <dbl> <dbl> <dbl> <list>    
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

### From {terra} objects: SpatVector

``` r
# load terra package
library(terra)

duckdb_conn = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create a data.frame with x and y coordinates and attributes
coordinates <- data.frame(x = c(100, 200, 300), y = c(500, 600, 700))
attributes <- data.frame(id = 1:3, name = c("A", "B", "C"))

# Combine the coordinates and attributes
dummy_data <- cbind(coordinates, attributes)

# Create a SpatVector from the data.frame
dummy_spatvector <- terra::vect(dummy_data, geom = c("x", "y"))

# Load SpatVector in db
createDbSpatPoints(conn = duckdb_conn,
                   tbl = "spatVector_proxy",
                   data = dummy_spatvector,
                   overwrite = TRUE)
```

### From {terra} objects: SpatRaster

``` r
# TODO
```

## Creating dbSpatPolygons

Few words about dbSpatPolygons class

``` r
#TODO
```
