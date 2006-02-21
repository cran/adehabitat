"colasc" <-
function(x, ...)
  {
    if (!inherits(x, "asc"))
      stop("Should be an \"asc\" object")
    l<-list(...)
    n<-names(l)
    i<-NA
    tc<-levels(x)
    for (i in n) {
      if (!any(tc==i))
        stop(paste(i, "is not a valid level of the factors"))
    }
    coul<-0
    for (i in 1:length(tc)) {
      u<-tc[i]
      coul[i]<-l[[u]]
    }
    return(coul)
  }

