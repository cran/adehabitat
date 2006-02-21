"attpol2area" <-
function(srdf)
  {
    if (!inherits(srdf, "SpatialPolygonsDataFrame"))
      stop("sr should be of class \"SpatialPolygonsDataFrame\"")
    dat <- srdf@data
    sr <- polygons(srdf)
    
    res <- lapply(1:length(sr@polygons), function(i) {
      x <- sr@polygons[[i]]
      y <- x@Polygons
      nom <- x@ID
      ll <- length(y)
      hh <- unlist(lapply(y, function(o) o@hole))
      hol <- sum(hh)
      ll <- ll-hol
      if (ll == 1) {
        re <- data.frame(nom=nom,dat[i,])
      }
      if (ll > 1) {
        nom <- paste(nom, 1:ll, sep=".")
        re <- data.frame(nom=nom, dat[rep(i,ll),])
      }
      return(re)
    })
    res <- do.call("rbind.data.frame", res)
    row.names(res) <- 1:nrow(res)
    return(res)
  }

