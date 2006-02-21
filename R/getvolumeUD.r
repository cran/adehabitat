"getvolumeUD" <-
function(x)
  {
    if ((!inherits(x, "khrud"))&(!inherits(x, "kbbhrud")))
      stop("x should be an object of class \"khrud\" or \"kbbhrud\"")

    for (i in 1:length(x)) {
      asc<-x[[i]]$UD
      cs<-attr(asc,"cellsize")
      v<-.C("calcvolume", as.double(t(asc)), as.integer(ncol(asc)),
            as.integer(nrow(asc)), as.double(cs), PACKAGE="adehabitat")[[1]]

      ##
      index<-1:length(v)
      vord<-v[order(v, decreasing=TRUE)]
      indord<-index[order(v, decreasing=TRUE)]
      vsu<-cumsum(vord)
      vreord<-vsu[order(indord)]*100
      u<-matrix(vreord, ncol=ncol(asc), byrow=TRUE)
      x[[i]]$UD<-getascattr(asc,u)
    }
    class(x)<-c("khrvol", "khr")
    return(x)
  }

