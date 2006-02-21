"area2spol" <-
function(ar)
  {
    if (!inherits(ar, "area"))
      stop("ka should be of class \"area\"")
    if (!require(sp))
      stop("the package sp is required for this function")
    class(ar) <- "data.frame"
    li <- split(ar[,2:3],ar[,1])
    res <- lapply(li, function(x) {
      if (!all(unlist(x[1,]==x[nrow(x),])))
        x <- rbind(x,x[1,])
      x <- as.matrix(x)
      y <- Polygon(x, hole=FALSE)
      if (y@ringDir<0)
        y <- Polygon(x[nrow(x):1,], hole=FALSE)
      return(y)
    })
    resb <- SpatialPolygons(lapply(1:length(res),
                               function(i) Polygons(list(res[[i]]),
                                                    names(res)[i])))
    return(resb)
  }

