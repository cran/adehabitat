"getverticeshr" <-
function(x, lev=95)
  {
    if ((!inherits(x,"khr")))
      stop("non convenient data-type")
    if (inherits(x,"khrud"))
      x<-getvolumeUD(x)
    if (inherits(x,"kbbhrud"))
      x<-getvolumeUD(x)
    contour<-list()
    
    for (i in 1:length(x)) {
      ud<-x[[i]]$UD
      ud[ud>lev]<-NA
      ud[!is.na(ud)]<-1
      contour[[i]]<-getcontour(ud)
    }
    names(contour)<-names(x)
    class(contour) <- "kver"
    return(contour)
  }

