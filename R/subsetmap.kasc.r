"subsetmap.kasc" <-
function(x, xlim=NULL, ylim=NULL, ref=names(x)[1], ...)
  {
    if (!inherits(x, "kasc"))
      stop("x should be of class kasc")
    if ((is.null(xlim))|(is.null(ylim))) {
      image(getkasc(x, ref), main="select the boudaries of the subset")
      ii<-locator(2)
      xlim<-ii$x
      ylim<-ii$y
    }
    so<-list()
    for (i in names(x))
      so[[i]]<-subsetmap.asc(getkasc(x, i), xlim=xlim, ylim=ylim)
    so<-as.kasc(so)
    return(so)
  }

