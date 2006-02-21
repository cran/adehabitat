"getXYcoords" <-
function(w)
  {
    if ((((!inherits(w, "asc"))&
         (!inherits(w, "kasc")))&
        (!inherits(w,"sahrlocs")))&
        (!inherits(w,"mapattr")))
      stop("non convenient object")
    
    cs<-attr(w, "cellsize")
    xll<-attr(w, "xll")
    yll<-attr(w, "yll")
    if (inherits(w,"asc")) {
      nr<-nrow(w)
      nc<-ncol(w)
    }
    if (((inherits(w,"kasc"))|(inherits(w, "sahrlocs")))|
        (inherits(w, "mapattr"))){
      nc<-attr(w, "nrow")
      nr<-attr(w, "ncol")
    }
    x<-xll+c(0:(nr-1))*cs
    y<-yll+c(0:(nc-1))*cs
    return(list(x=x, y=y))
  }

