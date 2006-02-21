"print.asc" <-
function(x, ...)
{
  if (!inherits(x, "asc")) stop("Non convenient data")
  cat("Raster map of class \"asc\":\n")
  cat("Cell size: ", attr(x, "cellsize"), "\n")
  cat("Number of rows: ", ncol(x), "\n")
  cat("Number of columns: ", nrow(x), "\n")
  cat("Type: ", attr(x, "type"), "\n")
}

