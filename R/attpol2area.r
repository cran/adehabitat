"attpol2area" <- function(srdf)
  {
      ## Verifications
      if (!inherits(srdf, "SpatialPolygonsDataFrame"))
          stop("sr should be of class \"SpatialPolygonsDataFrame\"")

      ## Gets the attributes and the polygons
      dat <- srdf@data
      sr <- polygons(srdf)

      ## Gets the contour of the polygons for each polygon
      res <- lapply(1:length(sr@polygons), function(i) {

          ## gets the polygon
          x <- sr@polygons[[i]]
          y <- x@Polygons

          ## The ID
          nom <- x@ID

          ## we delete the holes
          ll <- length(y)
          hh <- unlist(lapply(y, function(o) o@hole))
          hol <- sum(hh)
          ll <- ll-hol

          ## the output
          if (ll == 1) {
              re <- data.frame(nom=nom,dat[i,])
          }
          if (ll > 1) {
              nom <- paste(nom, 1:ll, sep=".")
              re <- data.frame(nom=nom, dat[rep(i,ll),])
          }
          return(re)
      })

      ## general ouput
      res <- do.call("rbind.data.frame", res)
      row.names(res) <- 1:nrow(res)
      return(res)
  }

