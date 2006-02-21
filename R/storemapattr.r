"storemapattr" <-
function(x)
  {
    if ((!inherits(x,"asc"))&(!inherits(x,"kasc")))
      stop("x should be a map of class asc or kasc")
    toto<-0
    if (inherits(x, "asc"))
      x<-as.kasc(list(x=x))
    toto<-getkascattr(x,toto)
    class(toto)<-"mapattr"
    return(toto)
  }

