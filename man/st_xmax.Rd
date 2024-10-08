% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/st_xmax.R
\name{st_xmax}
\alias{st_xmax}
\alias{st_xmax,dbSpatial-method}
\title{Get maximum x coordinate}
\usage{
st_xmax(dbSpatial, geomName = "geom", ...)

\S4method{st_xmax}{dbSpatial}(dbSpatial, geomName = "geom", ...)
}
\arguments{
\item{geomName}{\code{character string}. The geometry column name in the  \code{\link{dbSpatial}}  object. Default: \code{"geom"}.}

\item{...}{additional arguments passed to methods}

\item{\code{\link{dbSpatial}}}{object}
}
\value{
numerical column vector in database
}
\description{
This function returns the maximum x coordinate in each \code{geometry} in
the specified \code{\link{dbSpatial}} object.
}
\section{Functions}{
\itemize{
\item \code{st_xmax(dbSpatial)}: Method for \code{dbSpatial} object

}}
\examples{
# Create a data.frame with x and y coordinates and attributes
coordinates <- data.frame(x = c(100, 200, 300), y = c(500, 600, 700))
attributes <- data.frame(id = 1:3, name = c("A", "B", "C"))

# Combine the coordinates and attributes
dummy_data <- cbind(coordinates, attributes)

# Create a duckdb connection
con = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create a duckdb table with spatial points
db_points = dbSpatial(conn = con,
                      value = dummy_data,
                      x_colName = "x",
                      y_colName = "y",
                      name = "foo",
                      overwrite = TRUE)

st_extent(dbSpatial = db_points)
                      
st_xmax(dbSpatial = db_points)
}
\seealso{
Other geom_summary: 
\code{\link{st_extent}()},
\code{\link{st_ymax}()}
}
\concept{geom_summary}
