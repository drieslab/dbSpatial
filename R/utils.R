#' Install and/or load duckdb spatial extension
#'
#' @param conn duckdb connection
#'
#' @return NULL
#' @export
#' @keywords duckdb-ext
#' @examples
#' duckdb_conn = DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' loadSpatial(conn = duckdb_conn)
loadSpatial <- function(conn){
  # v0.9.0 spatial extension auto-loading
  # does not seem to work

  # Install spatial extension
  query = "INSTALL spatial; LOAD spatial;"
  invisible(DBI::dbExecute(conn, query))

  message("Duckdb spatial extension installed and loaded")
}
