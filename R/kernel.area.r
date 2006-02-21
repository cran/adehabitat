"kernel.area" <-
function (xy, id, h = "href", grid = 40,
                         same4all = FALSE, hlim = c(0.1, 1.5),kern = "bivnorm",
                         levels = seq(20, 95, by = 5), unin = c("m", "km"), 
                         unout = c("ha", "km2", "m2")) 
{
  unin <- match.arg(unin)
  unout <- match.arg(unout)
  x <- kernelUD(xy, id, h, grid, same4all, hlim, kern)
  x <- getvolumeUD(x)
  area <- list()
  contours <- list()
  for (j in names(x)) {
    tmpsurf <- rep(0, length(levels))
    for (i in 1:length(levels)) {
      asc <- x[[j]]$UD
      tmp <- asc < levels[i]
      cs <- attr(asc, "cellsize")
            tmpsurf[i] <- sum(as.numeric(tmp)) * cs * cs
    }
    area[[j]] <- tmpsurf
  }
  area <- data.frame(area)
  row.names(area) <- levels
  names(area) <- names(x)
  if (unin == "m") {
    if (unout == "ha") 
      area <- area/10000
    if (unout == "km2") 
      area <- area/1e+06
  }
  if (unin == "km") {
    if (unout == "ha") 
      area <- area * 100
    if (unout == "m2") 
      area <- area * 1e+06
  }
  class(area) <- c("hrsize", "data.frame")
  attr(area, "units") <- unout
  return(area)
}

