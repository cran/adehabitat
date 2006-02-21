"traj2spdf" <-
function(tr)
  {
    if (!inherits(tr, "traj"))
      stop("tr should be of class \"traj\"")
    class(tr) <- "data.frame"
    xy <- tr[,c("x","y")]
    tr$y <- tr$x <- NULL
    res <- SpatialPointsDataFrame(xy, tr)
    return(res)
  }

