# dbData ####

#' @name dbData
#' @title dbData
#' @description Base class for all db objects
#' @slot value dplyr tbl that represents the database data
#' @slot name name of table within database that contains the data
#' @slot init logical. Whether the object is fully initialized
#' @noRd
setClass(
  Class = 'dbData',
  contains = c('VIRTUAL'),
  slots = list(
    value = 'ANY',
    name = 'character',
    init = 'logical' #TODO
  ),
  prototype = list(
    value = NULL,
    name = NA_character_,
    init = FALSE
  )
)

### dbSpatial ####

#' @title S4 dbSpatial class
#' @description
#' Representation of spatial points and polygons using an on-disk database. Each object
#' is used as a connection to a single table in an existing database.
#' @export
dbSpatial = setClass(
  Class = 'dbSpatial',
  contains = c('dbData', 'VIRTUAL')
)


#### dbSpatialPoints ####
#' @title S4 Class for dbSpatialPoints
#'
#' @description Representation of spatial points using an on-disk database.
#' Inherits from dbSpatial
#'
#' @export
dbSpatialPoints = setClass(
  Class = "dbSpatialPoints",
  contains = "dbSpatial"
)

#### dbSpatialPolygons ####
#' @title S4 Class for dbSpatialPolygons
#'
#' @description Representation of spatial polygons using an on-disk database.
#' Inherits from dbSpatial
#'
#' @export
dbSpatialPolygons = setClass(
  Class = "dbSpatialPolygons",
  contains = "dbSpatial"
)