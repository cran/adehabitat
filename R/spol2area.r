"spol2area" <-
function(sr)
  {
    if (!require(sp))
      stop("the package sp is required for this function")
    if (inherits(sr, "SpatialPolygonsDataFrame"))
      sr <- polygons(sr)
    if (!inherits(sr, "SpatialPolygons"))
      stop("sr should be of class \"SpatialPolygons\" or \"SpatialPolygonsDataFrame\"")
    pol <- sr@polygons
    warh <- 0
    warh2 <- 0
    warz <- 0
    res <- lapply(pol, function(x) {
      y <- x@Polygons
      nom <- x@ID
      ll <- length(y)
      hh <- unlist(lapply(y, function(o) o@hole))
      hol <- sum(hh)
      ll <- ll-hol
      if (ll == 1) {
        if (hol == 0) {
          re <- as.data.frame(y[[1]]@coords)
          re <- data.frame( fac = factor(rep(nom,length(re[,1]))), re)
          names(re) <- c("fac", "x", "y")
        }
        if (hol != 0) {
          warh <- warh+hol
          warh2 <- warh2+1
          re <- as.data.frame(y[!hh][[1]]@coords)
          re <- data.frame( fac = factor(rep(nom,length(re[,1]))), re)
          names(re) <- c("fac", "x", "y")
        }
      }
      if (ll > 1) {
        warz <- warz+1
        if (hol == 0) {
          nom <- paste(nom, 1:ll, sep=".")
          re1 <- lapply(y, function(o) as.data.frame(o@coords))
          re <- do.call("rbind.data.frame", lapply(1:length(re1), function(i) {
            u <- data.frame(fac=factor(rep(nom[i], length(re1[[i]][,1]))), re1[[i]])
            names(u) <- c("fac", "x", "y")
            return(u)
          }))
        }
        if (hol!=0) {
          warh <- warh+hol
          warh2 <- warh2+1
          nom <- paste(nom, 1:ll, sep=".")
          y <- y[!hh]
          re1 <- lapply(y, function(o) as.data.frame(o@coords))
          re <- do.call("rbind.data.frame", lapply(1:length(re1), function(i) {
            u <- data.frame(fac=factor(rep(nom[i], length(re1[[i]][,1]))), re1[[i]])
            names(u) <- c("fac", "x", "y")
            return(u)
          }))
        }
      }
      return(list(re,warh2, warh, warz))
    })
    warh2 <- sum(unlist(lapply(res, function(x) x[[2]])))
    warh <- sum(unlist(lapply(res, function(x) x[[3]])))
    warz <- sum(unlist(lapply(res, function(x) x[[4]])))
    res <- lapply(res, function(x) x[[1]])
    res <- do.call("rbind.data.frame", res)
    res <- as.area(res)
    if (warh2>0) {
      warning(paste("Area objects do not take into account holes in polygon.\n",
                    warh, "holes have been deleted from the data, belonging to\n",
                    warh2, "polygons"))
    }
##    if (warz>0) {
##      warning(paste("Some spatial rings contained several polygons.\n",
##                    "Labels have therefore been changed for", warz, "objects"))
##    }
    return(res)
  }

