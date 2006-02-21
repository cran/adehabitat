"buffer.ani" <-
function(pts, fac, x, dist)
  {
    if (inherits(x, "asc"))
      x<-as.kasc(list(toto=x))
    if (inherits(x, "kasc"))
      x<-storemapattr(x)
    if (!inherits(x, "mapattr"))
      stop("non convenient format for x")
    if (length(fac)!=nrow(pts))
      stop("factor should have the same length as pts")
    
    lipts<-split(pts, fac)
    sorties<-list()
    
    for (i in names(lipts)) {
      ptst<-lipts[[i]]
      sorties[[i]]<-buffer(ptst, x, dist)
    }
    
    sor<-as.kasc(sorties)
    
    return(sor)
  }

