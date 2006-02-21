"getkasc" <-
function(x, var)
  {
    w<-x
    if (!inherits(w, "kasc")) stop("Non convenient data")

    v<-x[[var]]
    if ((is.numeric(v))|(is.logical(v))) {
      e<-matrix(w[[var]], ncol=attr(w, "nrow"))
      attr(e, "type")<-"numeric"
    } else {
      tc2<-levels(v)
      v<-as.numeric(v)
      e<-matrix(v, ncol=attr(w, "nrow"))
      attr(e, "type")<-"factor"
      attr(e, "levels")<-tc2
    }
    attr(e, "cellsize")<-attr(w, "cellsize")
    attr(e, "xll")<-attr(w, "xll")
    attr(e, "yll")<-attr(w, "yll")
    class(e)<-"asc"
    return(e)
  }

