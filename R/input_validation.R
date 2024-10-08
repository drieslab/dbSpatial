#' Input validation for conn arg
#' @keywords internal
#' @noRd
.check_con <- function(conn){
  if(missing(conn)) {
    stop("Please provide a db connection.")
  }

  if(!DBI::dbIsValid(conn)) {
    stop("Stale connection. Reconnect your db connection.")
  }

  if(!inherits(conn, "duckdb_connection")) {
    stop("conn must be a duckdb connection. Use duckdb drv in DBI::dbConnect().")
  }
}


#' Input validation for data arg
#' @keywords internal
#' @noRd
.check_value <- function(value){
  is_tbl_dbi = inherits(value, "tbl_dbi")
  is_df = is.data.frame(value)
  is_terra = inherits(value, "SpatVector") || inherits(value, "SpatRaster")
  if(is.character(value)){
    is_valid_file = file.exists(value)
  } else {
    is_valid_file = FALSE
  }
  
  if(!(is_tbl_dbi || is_df || is_terra || is_valid_file)) {
    stop(
      'Invalid "value" input passed.'
    )
  }
}

#' Input validation for tbl arg
#' @keywords internal
#' @noRd
.check_tbl <- function(tbl){
  if (missing(tbl)) {
    stop("Please provide a table name.")
  }
  
  # check that tbl is from a duckdb connection
  if (!inherits(tbl, "tbl_duckdb_connection")) {
    stop("Please provide a table in a DuckDB database.")
  }
}

#' Input validation for name arg
#' @keywords internal
#' @noRd
.check_name <- function(name){

  if(missing(name)) {
    stop("Please provide a table name.")
  }

  # if(is.null(name)) {
  #   stop("name cannot be NULL. Did you forget to dplyr::compute()?")
  # }

  if(!is.character(name)) {
    stop("name must be a character string.")
  }

  # if name starts with a number, add warning
  if(grepl("^[0-9]", name)) {
    stop("Table names should not start with a number.")
  }

  # reserved name check
  reserved_names = c("intersect", "union", "except",
                     "select", "from", "where", "group", "by", "limit",
                     "create", "table", "insert")
  if(name %in% reserved_names){
    stop("Table name cannot be a RESERVED word. Try another name.")
  }
}

#' Input validation for geomName arg
#' @keywords internal
#' @noRd
.check_geomName <- function(value, geomName){
  
  if(is.null(geomName)) {
    stop("geomName cannot be NULL. Please provide a geomName.")
  }
  
  if(!(geomName %in% colnames(value))) {
    stop("geomName not found in 'value'.")
  }
  
  .check_name(name = geomName)
}

#' Input validation for overwrite arg
#' @keywords internal
#' @details
#' set overwrite = "PASS" token for handling lazy tables in database
#' that are not computed and have no name
#' 
#' @noRd
.check_overwrite <- function(conn, overwrite, name){
  if(overwrite == 'PASS') {
    return()
  }
  
  if(!is.logical(overwrite)) {
    stop("overwrite must be logical")
  }
  
  if(!overwrite & DBI::dbExistsTable(conn, name)){
    stop("Table already exists. Set overwrite = TRUE to overwrite the table.")
  }
  
  object_exists <- DBI::dbExistsTable(conn, name)
  
  if (overwrite && object_exists) {
    # Determine if the object is a view
    is_view <- DBI::dbGetQuery(conn, glue::glue("
      SELECT COUNT(*) > 0 AS is_view
      FROM duckdb_views()
      WHERE view_name = '{name}'
    "))$is_view
    
    arrow_tables <- duckdb::duckdb_list_arrow(conn = conn)
    is_arrow_table <- name %in% arrow_tables
    
    if (is_view) {
      DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {name}"))
    } else if (is_arrow_table) {
      duckdb::duckdb_unregister_arrow(conn = conn, name = name)
    } else {
      DBI::dbRemoveTable(conn, name)
    }
  }
  
  return()
}

#' Input validation for spatial_join functions
#' @keywords internal
#' @details
#' ensure two dbSpatial objects are in the same connection
#' 
#' @noRd
.check_con_shared <- function(conn1, conn2){
  # TODO: check if g1 and g2 share the same connection
  # How to extract absolute path of the con object from g1, g2?
  if(!all.equal(conn1, conn2)){
    stop("conn1 and conn2 must be in the same database connection.")
  }
}

#' Input validation for spatial_join functions
#' @keywords internal
#' @details
#' ensure two dbSpatial objects are in the same connection
#' 
#' @noRd
.check_cols_keep <- function(tbl, cols_keep){
  # check that cols_keep is a valid character vector
  if(!identical(cols_keep, "all")) {
    if(!is.character(cols_keep)) {
      stop("cols_keep must be a character or character vector.")
    }
    
    if(all(cols_keep == "none")){
      return()
    }
    
    if(!all(cols_keep %in% colnames(tbl))) {
      stop("cols_keep must be a subset of column names in tbl.")
    }
  }
}