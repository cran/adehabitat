"subsetmap.asc" <-
function(x, xlim=NULL, ylim=NULL, ...)
  {
    if (!inherits(x, "asc"))
      stop("x should be of class asc")
    if ((is.null(xlim))|(is.null(ylim))) {
      image(x, main="select the boundaries of the subset")
      ii<-locator(2)
      xlim<-ii$x
      ylim<-ii$y
    }
    xy<-getXYcoords(x)
    xlim<-xlim[order(xlim)]
    ylim<-ylim[order(ylim)]
    xll<-attr(x, "xll")
    yll<-attr(x, "yll")
    cs<-attr(x, "cellsize")
    posli1<-round((xlim[1]-xll)/cs, 0)+1
    posco1<-round((ylim[1]-yll)/cs, 0)+1
    posli2<-round((xlim[2]-xll)/cs, 0)+1
    posco2<-round((ylim[2]-yll)/cs, 0)+1
    o<-x[posli1:posli2,posco1:posco2]
    attr(o, "xll")<-xy$x[posli1]
    attr(o, "yll")<-xy$y[posco1]
    attr(o, "cellsize")<-cs
    attr(o, "type")<-attr(x, "type")
    if (attr(o, "type")=="factor")
      attr(o, "levels")<-attr(x, "levels")
    class(o)<-"asc"
    return(o)
  }

