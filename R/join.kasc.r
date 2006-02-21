"join.kasc" <-
function(pts, w)
  {
    x<-w
    if (!inherits(x, "kasc")) stop("non convenient data")
    sorties<-1:nrow(pts)
    for (i in 1:length(x)) {
      carp<-getkasc(x, names(x)[i])
      fac<-join.asc(pts, carp)
      sorties<-cbind.data.frame(sorties, fac)
    }
    sorties<-sorties[,-1]
    names(sorties)<-names(x)
    return(sorties)
  }

