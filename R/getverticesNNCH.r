"getverticesNNCH" <-
function(x, percent = 95)
  {
    if (!inherits(x, "NNCH"))
      stop("x should be of class \"NNCH\"")
    res<-list()
    for (kk in 1:length(x))
      res[[kk]]<-x[[kk]]$polygons[[max(which(x[[kk]]$area$levels<=percent))]]
    names(res)<-names(x)
    class(res)<-"NNCHver"
    return(res)
  }

