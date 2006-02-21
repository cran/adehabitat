"count.points" <-
function(xy, w)
  {
    if (inherits(w, "asc"))
      w<-as.kasc(list(toto=w))
    if (inherits(w, "kasc"))
      w<-storemapattr(w)
    if (!inherits(w, "mapattr"))
      stop("non convenient format for w")


    xyc<-getXYcoords(w)
    xc<-xyc$x-attr(w, "cellsize")/2
    yc<-xyc$y-attr(w, "cellsize")/2
    xc<-c(xc, max(xc)+attr(w, "cellsize"))
    yc<-c(yc, max(yc)+attr(w, "cellsize"))
    x<-xy[,1]
    y<-xy[,2]

    x<-cut(x, xc)
    y<-cut(y, yc)
    output<-as.matrix(table(x, y))
    if (inherits(x, "kasc")) {
      attr(output, "nrow")<-attr(w, "nrow")
      attr(output, "ncol")<-attr(w, "ncol")
    }
    attr(output, "xll")<-attr(w, "xll")
    attr(output, "yll")<-attr(w, "yll")
    attr(output, "cellsize")<-attr(w, "cellsize")
    attr(output, "type")<-"numeric"
    class(output)<-"asc"
    return(output)
  }

