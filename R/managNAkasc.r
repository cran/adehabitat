"managNAkasc" <-
function(x)
  {
    if (!inherits(x,"kasc")) stop("non convenient data")
    class(x)<-"data.frame"
    ## Conservation que des pixels non NA pour toutes les cartes
    tmpy<-is.na(x)
    tmp<-apply(tmpy, 1, function(x) sum(as.numeric(x)))
    x[tmp!=0,]<-rep(NA, ncol(x))
    class(x)<-c("kasc", "data.frame")
    return(x)
  }

