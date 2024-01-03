# Spatial Operations with {terra} Comparisons

``` r
# load libs
library(dbSpatial)
library(terra)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:terra':
#> 
#>     intersect, union
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(magrittr) # remove once import %>%
#> 
#> Attaching package: 'magrittr'
#> The following objects are masked from 'package:terra':
#> 
#>     extract, inset
```

## Create test data

``` r
# Create a data.frame with x and y coordinates and attributes
coordinates <- data.frame(x = c(100, 200, 300), y = c(500, 600, 700))
attributes <- data.frame(id = 1:3, name = c("A", "B", "C"))

# Combine the coordinates and attributes
dummy_data <- cbind(coordinates, attributes)

# Create a SpatVector from the data.frame
dummy_spatvector <- vect(dummy_data, geom = c("x", "y"))

# DuckDB
duckdb_conn = DBI::dbConnect(duckdb::duckdb(), ":memory:")

db_points = createDbSpatPoints(conn = duckdb_conn,
                               tbl = "spatVector_proxy",
                               data = dummy_spatvector,
                               overwrite = TRUE)
#> Duckdb spatial extension installed and loaded
```

### Extent

``` r
# terra
terra::ext(dummy_spatvector)
#> SpatExtent : 100, 300, 500, 700 (xmin, xmax, ymin, ymax)

# dbSpatial
dbSpatial::ST_Extent(tbl = db_points)
#>   min_x max_x min_y max_y
#> 1   100   300   500   700
```

### Shift

``` r
# terra
terra::geom(dummy_spatvector)
#>      geom part   x   y hole
#> [1,]    1    1 100 500    0
#> [2,]    2    1 200 600    0
#> [3,]    3    1 300 700    0

shifted_spatVector = terra::shift(dummy_spatvector, dx = 10, dy = 10)

terra::geom(shifted_spatVector)
#>      geom part   x   y hole
#> [1,]    1    1 110 510    0
#> [2,]    2    1 210 610    0
#> [3,]    3    1 310 710    0

# dbSpatial
dbSpatial::ST_Translate(tbl = db_points, dx = 10, dy = 10) %>%
  mutate(points = ST_AsText(geom)) %>%
  select(points)
#> # Source:   SQL [3 x 1]
#> # Database: DuckDB 0.9.0 [root@Darwin 22.4.0:R 4.3.0/:memory:]
#>   points         
#>   <chr>          
#> 1 POINT (110 510)
#> 2 POINT (210 610)
#> 3 POINT (310 710)
```

### Flip

``` r
# terra
terra::geom(dummy_spatvector)
#>      geom part   x   y hole
#> [1,]    1    1 100 500    0
#> [2,]    2    1 200 600    0
#> [3,]    3    1 300 700    0

flipped_spatVector = terra::flip(dummy_spatvector, direction = "vertical")

terra::geom(flipped_spatVector)
#>      geom part   x   y hole
#> [1,]    1    1 100 500    0
#> [2,]    2    1 200 400    0
#> [3,]    3    1 300 300    0

# <----- not equivalent to terra::flip ------>
# dbSpatial
# db_points %>%
#   mutate(points = ST_AsText(geom))
# 
# db_points %>%
#   mutate(flipped_points = ST_AsText(ST_FlipCoordinates(geom)))
```

### Is.Valid

``` r
# terra
terra::geom(dummy_spatvector)
#>      geom part   x   y hole
#> [1,]    1    1 100 500    0
#> [2,]    2    1 200 600    0
#> [3,]    3    1 300 700    0

terra::is.valid(dummy_spatvector)
#> [1] TRUE TRUE TRUE

# dbSpatial
db_points %>%
  mutate(points = ST_AsText(geom)) %>%
  select(points)
#> # Source:   SQL [3 x 1]
#> # Database: DuckDB 0.9.0 [root@Darwin 22.4.0:R 4.3.0/:memory:]
#>   points         
#>   <chr>          
#> 1 POINT (100 500)
#> 2 POINT (200 600)
#> 3 POINT (300 700)

dbSpatial::ST_IsValid(tbl = db_points)
#> [1] TRUE TRUE TRUE
```

### Y, X Max

``` r
# terra
terra::geom(dummy_spatvector)
#>      geom part   x   y hole
#> [1,]    1    1 100 500    0
#> [2,]    2    1 200 600    0
#> [3,]    3    1 300 700    0

terra::ymax(dummy_spatvector)
#> [1] 700

terra::xmax(dummy_spatvector)
#> [1] 300

# dbSpatial
dbSpatial::ST_YMax(db_points)
#> [1] 700
dbSpatial::ST_XMax(db_points)
#> [1] 300
```

### nrow

``` r
# terra
terra::nrow(dummy_spatvector)
#> [1] 3

# dbSpatial
dbSpatial::nrow(db_points)
#> [1] 3
```

### centroids

``` r
# terra
terra::geom(dummy_spatvector)
#>      geom part   x   y hole
#> [1,]    1    1 100 500    0
#> [2,]    2    1 200 600    0
#> [3,]    3    1 300 700    0

terra::centroids(dummy_spatvector)
#>  class       : SpatVector 
#>  geometry    : points 
#>  dimensions  : 3, 2  (geometries, attributes)
#>  extent      : 100, 300, 500, 700  (xmin, xmax, ymin, ymax)
#>  coord. ref. :  
#>  names       :    id  name
#>  type        : <int> <chr>
#>  values      :     1     A
#>                    2     B
#>                    3     C
```
