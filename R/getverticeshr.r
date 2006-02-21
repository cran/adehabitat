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
      jj <- labcon(ud)
      jj <- table(factor(c(jj)))
      if (any(jj<4))
        stop("Some parts of the home range contain less than 3 pixels.
Increase the size of the grid used for the estimation in
the function 'kernelUD' (parameter 'grid') and try again")
      contour[[i]]<-getcontour(ud)
    }
    names(contour)<-names(x)
    class(contour) <- "kver"
    return(contour)
  }

