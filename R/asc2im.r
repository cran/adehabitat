"asc2im" <-
function(x)
  {
    if (!inherits(x, "asc"))
      stop("should be an object of class \"asc\"")
    if (attr(x, "type")=="factor")
      stop("function not yet implemented for factors")
    if (!require(spatstat))
      stop("the package spatstat should be available for this function")
    xy<-getXYcoords(x)
    sorties<-im(t(unclass(x)), xy$x,xy$y)
    return(sorties)
  }

