"neighNNCH" <-
function(xy, id=NULL, rangek, percent=95,
                      unin = c("m", "km"),
                      unout = c("ha", "km2", "m2"))
  {
    if (max(rangek)>=nrow(xy))
      stop("too large number of neighbors")
    if (ncol(xy)!=2)
      stop("xy should have two columns")
    kk <- do.call("rbind", lapply(rangek, function(x) {
      unlist(NNCH.area(NNCH(xy=xy, id=id, k=x, unin, unout), percent)[1,])
    }))
    rownames(kk) <- rangek
    colnames(kk) <- levels(id)
    class(kk) <- c("ngNNCH")
    return(kk)
  }

