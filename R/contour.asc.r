"contour.asc" <-
function(x, ...)
  {
    if (!inherits(x, "asc")) stop("not an \"asc\" object")
    if (attr(x, "type")=="factor")
      stop("function contour cannot be used with factors")
    z<-x
    xy<-getXYcoords(z)
    x<-xy$x
    y<-xy$y
    contour(x=x, y=y, z,  ...)
  }

