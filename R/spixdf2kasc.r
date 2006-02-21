"spixdf2kasc" <-
function(sg)
  {
    if (!require(sp))
      stop("the package sp is required for this function")
    if (inherits(sg, "SpatialPixelsDataFrame"))
      sg <- as(sg, "SpatialGridDataFrame")
    if (!inherits(sg, "SpatialGridDataFrame"))
      stop(paste("sg should be of class \"SpatialPixelsDataFrame\"",
                 "\nor \"SpatialGridDataFrame\""))
    gr <- gridparameters(sg)
    if (nrow(gr)>2)
      stop("sg should be defined in two dimensions")
    if (gr[1,2]!=gr[2,2])
      stop("the cellsize should be the same in x and y directions")
    fullgrid(sg) <- TRUE
    xy <- coordinates(sg)
    ka <- as.data.frame(attr(sg@data, "att"))
    ka <- ka[order(xy[,1]),]
    xy <- xy[order(xy[,1]),]
    ka <- ka[order(xy[,2]),]
    xy <- xy[order(xy[,2]),]
    nxy <- colnames(xy)
    attr(ka, "cellsize") <- gr[2,2]
    attr(ka, "xll") <- gr[1,1]
    attr(ka, "yll") <- gr[2,1]
    attr(ka,"ncol") <- gr[1,3]
    attr(ka,"nrow") <- gr[2,3]
    class(ka) <- c("kasc", "data.frame")
    if (ncol(ka)==1) {
      v <- ka[,1]
      if ((is.numeric(v)) | (is.logical(v))) {
        e <- matrix(v, ncol = attr(ka, "nrow"))
        attr(e, "type") <- "numeric"
      }
      else {
        tc2 <- levels(v)
        v <- as.numeric(v)
        e <- matrix(v, ncol = attr(ka, "nrow"))
        attr(e, "type") <- "factor"
        attr(e, "levels") <- tc2
      }
      attr(e, "cellsize") <- attr(ka, "cellsize")
      attr(e, "xll") <- attr(ka, "xll")
      attr(e, "yll") <- attr(ka, "yll")
      class(e) <- "asc"
      ka <- e
    }
    return(ka)
  }

