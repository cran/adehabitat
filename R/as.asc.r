"as.asc" <-
function(x, xll=1, yll=1, cellsize=1, type=c("numeric", "factor"),
                 lev=levels(factor(x)))
  {
    type<-match.arg(type)
    if (!inherits(x, "matrix"))
      stop("x should be a matrix")
    mode(x)<-"numeric"
    attr(x, "xll")<-xll
    attr(x, "yll")<-yll
    attr(x, "cellsize")<-cellsize
    attr(x, "type")<-type
    if (type=="factor")
      attr(x, "levels")<-lev
    class(x)<-"asc"
    return(x)
  }

