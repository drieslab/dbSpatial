#' @title Create a duckdb table with spatial points
#'
#' @param data Data to be added to database. Can be a data.frame, tbl,
#' file path, or {terra} spatVector/spatRaster. Experimental support for
#' lists of files is supported through {duckdbfs}.
#' @param conn Connection to duckdb database.
#' @param tbl Name of table to add dbSpatPoints to.
#' @param overwrite Overwrite existing table (default = FALSE)
#' @param ... Additional arguments to be passed
#'
#' @details For full list of supported file types see ST_Read() in duckdb.
#'
#' @return A duckdb table with spatial points.
#' @export
#'
#' @keywords constructor
#'
#' @examples
#' # create in-memory duckdb db
#' duckdb_conn = DBI::dbConnect(duckdb::duckdb(), ":memory:")
#'
#' # test data
#' test_data = data.frame(x = 1:10, y = 1:10, id = 1:10)
#'
#' write.csv(test_data, "test_data.csv", row.names = FALSE)
#'
#' # df, tbl
#' createDbSpatPoints(conn = duckdb_conn,
#'                    tbl = "test_points",
#'                    data = test_data,
#'                    overwrite = TRUE)
#'
#' # file
#' createDbSpatPoints(conn = duckdb_conn,
#'                    tbl = "test_points",
#'                    data = 'test_data.csv',
#'                    overwrite = TRUE)
#'

createDbSpatPoints <- function(data,
                               conn,
                               tbl,
                               overwrite = FALSE,
                               ...) {
  # check inputs
  if (missing(conn)) {
    stop("Please provide a connection")
  }

  if (!DBI::dbIsValid(conn)) {
    stop("Invalid connection, check your db connection.")
  }

  if (is.null(data)) {
    stop("Please provide data source")
  }

  # Check if tbl exists if it does drop it
  if (overwrite) {
    if (DBI::dbExistsTable(conn, tbl)) {
      DBI::dbRemoveTable(conn, tbl)
    }
  }

  # Load spatial extension
  loadSpatial(conn)

  # Ingest data
  if (inherits(data, "data.frame") | inherits(data, "tbl")) {
    DBI::dbWriteTable(conn, tbl, data)
  } else if (is.character(data)) { # read single file using duckdb
    if (file.exists(data)) {
      if(tools::file_ext(data) == "csv"){
        # read csv
        ingest_query = paste0("CREATE TABLE ", tbl, " AS ",
                              "SELECT * FROM '", data, "';")
        invisible(DBI::dbExecute(conn, ingest_query))
      } else{
        # spatial files
        ingest_query = paste0("CREATE TABLE ", tbl, " AS ",
                              "SELECT * FROM ST_Read(\"", data, "\");")
        invisible(DBI::dbExecute(conn, ingest_query))
      }
    }
    else {
      stop("Invalid file. Check path exists.")
    }
  } else if(is.list(data)){
    ducdkbfs::open_dataset(data)
    message("experimental feature")
  } else if(inherits(data, "SpatVector") | inherits(data, "SpatRaster")){
    # write out data to temporary .geojson file
    tmp_geojson = tempfile(fileext = ".geojson")

    if (inherits(data, "SpatRaster")) {
      terra::writeRaster(data, tmp_geojson, filetype = "geojson", overwrite = TRUE)
    } else if (inherits(data, "SpatVector")) {
      terra::writeVector(data, tmp_geojson, filetype = "geojson", overwrite = TRUE)
    }

    # load data into duckdb
    ingest_query = paste0("CREATE TABLE ", tbl, " AS ",
                          " SELECT * FROM ST_Read(\"", tmp_geojson, "\");")
    invisible(DBI::dbExecute(conn, ingest_query))

  } else{
    stop("Invalid data source. Please provide a data.frame, tbl, or file path.")
  }

  # if there is no geom column in the tbl add one
  if(!"geom" %in% DBI::dbListFields(conn, tbl)){

    sql = paste0("ALTER TABLE ", tbl, " ADD COLUMN geom GEOMETRY;",
                 "UPDATE ", tbl, " SET geom = ST_Point(x, y);")
    invisible(DBI::dbExecute(conn, sql))
  }

  # Show preview
  dplyr::tbl(conn, tbl)

}

#' Create a duckdb table with spatial polygons
#'
#' @param data Data to be added. Can be a terra::spatRaster, terra::spatVector, or file path.
#' @param conn Connection to duckdb database.
#' @param tbl Name of table to add dbSpatPolygons to.
#' @param overwrite Overwrite existing table (default = FALSE)
#' @param ... Additional arguments to be passed.
#'
#' @return A duckdb table with spatial polygons.
#' @export
#' @keywords constructor
#' @examples
#' TODO
createDbSpatPolygons <- function(data,
                                 conn,
                                 tbl,
                                 overwrite = FALSE,
                                 ...){
  stop(TODO)
}

#' Get extent of a geometry
#'
#' @param tbl name of a table in a duckdb database
#'
#' @return data.frame of extent of geom column in tbl
#' @export
#' @keywords geo_construction
#' @examples
#'
#' # Create a data.frame with x and y coordinates and attributes
#' coordinates <- data.frame(x = c(100, 200, 300), y = c(500, 600, 700))
#' attributes <- data.frame(id = 1:3, name = c("A", "B", "C"))
#'
#' # Combine the coordinates and attributes
#' dummy_data <- cbind(coordinates, attributes)
#'
#' # Create a SpatVector from the data.frame
#' dummy_spatvector <- terra::vect(dummy_data, geom = c("x", "y"))
#'
#' # Create a duckdb connection
#' duckdb_conn = DBI::dbConnect(duckdb::duckdb(), ":memory:")
#'
#' # Create a duckdb table with spatial points
#' db_points = createDbSpatPoints(conn = duckdb_conn,
#'                                tbl = "spatVector_proxy",
#'                                data = dummy_spatvector,
#'                                overwrite = TRUE)
#' # Get extent of the table
#' ext(db_points)
ST_Extent <- function(tbl){
  # check inputs
  if (missing(tbl)) {
    stop("Please provide a table name")
  }

  # check that tbl is from a duckdb connection
  if (!inherits(tbl, "tbl_duckdb_connection")) {
    stop("Please provide a duckdb table")
  }

  res <- tbl %>%
    dplyr::mutate(extent = ST_Extent(geom)) %>%
    dplyr::pull(extent) %>%
    dplyr::summarize(
      min_x = min(min_x),
      max_x = max(max_x),
      min_y = min(min_y),
      max_y = max(max_y)
    )

  res
}

#' Determine if geometry is valid
#'
#' @param tbl name of a table in a duckdb database
#'
#' @return boolean vector of whether each geometry in tbl is valid
#' @export
#' @keywords spatial_prop
#' @examples
#' # Create a data.frame with x and y coordinates and attributes
#' coordinates <- data.frame(x = c(100, 200, 300), y = c(500, 600, 700))
#' attributes <- data.frame(id = 1:3, name = c("A", "B", "C"))
#'
#' # Combine the coordinates and attributes
#' dummy_data <- cbind(coordinates, attributes)
#'
#' # Create a SpatVector from the data.frame
#' dummy_spatvector <- terra::vect(dummy_data, geom = c("x", "y"))
#'
#' # Create a duckdb connection
#' duckdb_conn = DBI::dbConnect(duckdb::duckdb(), ":memory:")
#'
#' # Create a duckdb table with spatial points
#' db_points = createDbSpatPoints(conn = duckdb_conn,
#'                                tbl = "spatVector_proxy",
#'                                data = dummy_spatvector,
#'                                overwrite = TRUE)
#' # Get extent of the table
#' ST_IsValid(db_points)
ST_IsValid <- function(tbl){
  # check inputs
  if (missing(tbl)) {
    stop("Please provide a table name")
  }

  # check that tbl is from a duckdb connection
  if (!inherits(tbl, "tbl_duckdb_connection")) {
    stop("Please provide a duckdb table")
  }

  res <- tbl %>%
    dplyr::mutate(is_valid = ST_IsValid(geom)) %>%
    dplyr::pull(is_valid)

  res
}

#' Translate x, y coordinates by delta x, delta y
#'
#' @param tbl name of a table in a duckdb database
#' @param dx delta x
#' @param dy delta y
#'
#' @return a duckdb table with translated geometries
#' @export
#' @keywords geo_construction
#' @examples
#' TODO
ST_Translate <- function(tbl, dx, dy) {
  # check inputs
  if (missing(tbl)) {
    stop("Please provide a table name")
  }

  # check that tbl is from a duckdb connection
  if (!inherits(tbl, "tbl_duckdb_connection")) {
    stop("Please provide a duckdb table")
  }

  res <- tbl %>%
    mutate(x = ST_X(geom) + dx, y = ST_Y(geom) + dy) %>%
    mutate(geom = ST_Point(x, y)) %>%
    select(-x, -y)

  res

}


#' Get maximum x coordinate
#'
#' @param tbl name of a table in a duckdb database
#'
#' @return maximum x coordinate
#' @export
#' @keywords spatial_prop
#' @examples
#' TODO
ST_XMax <- function(tbl) {
  # check inputs
  if (missing(tbl)) {
    stop("Please provide a table name")
  }

  # check that tbl is from a duckdb connection
  if (!inherits(tbl, "tbl_duckdb_connection")) {
    stop("Please provide a duckdb table")
  }

  res = tbl %>%
    mutate(x = ST_X(geom)) %>%
    pull(x) %>%
    max()

  res
}

#' Get maximum y coordinate
#'
#' @param tbl name of a table in a duckdb database
#'
#' @return maximum y coordinate
#' @export
#' @keywords spatial_prop
#' @examples
#' TODO
ST_YMax <- function(tbl) {
  # check inputs
  if (missing(tbl)) {
    stop("Please provide a table name")
  }

  # check that tbl is from a duckdb connection
  if (!inherits(tbl, "tbl_duckdb_connection")) {
    stop("Please provide a duckdb table")
  }

  res = tbl %>%
    mutate(y = ST_Y(geom)) %>%
    pull(y) %>%
    max()

  res
}

#' Count number of rows in a duckdb table
#'
#' @param tbl name of a table in a duckdb database
#'
#' @return number of rows in tbl (integer)
#' @export
#' @keywords spatial_prop
#' @examples
#' TODO
nrow <- function(tbl){
  # check inputs
  if (missing(tbl)) {
    stop("Please provide a table name")
  }

  # check that tbl is from a duckdb connection
  if (!inherits(tbl, "tbl_duckdb_connection")) {
    stop("Please provide a duckdb table")
  }

  res = tbl %>%
    count() %>%
    pull(n)

  res
}

