"persp.asc" <-
function(x, ...)
  {
    if (!inherits(x, "asc")) stop("not an \"asc\" object")
    if (attr(x, "type")=="factor")
      stop("function persp cannot be used with factors")
    z<-x
    xy<-getXYcoords(z)
    x<-xy$x
    y<-xy$y
    persp(x=x, y=y, z, ...)
  }

