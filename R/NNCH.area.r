"NNCH.area" <-
function(x, percent=c(95,90,80,70,60,50,40,30,20,10))
  {
    if (!inherits(x, "NNCH"))
      stop("x should be of class \"NNCH\"")

    res<-matrix(0,nrow=length(percent), ncol=length(x))

    for (kk in 1:length(x)) {
      for (i in 1:length(percent))
        res[i,kk]<-x[[kk]]$area[max(which(x[[kk]]$area$levels<=percent[i])),2]
    }
    res<-as.data.frame(res)
    row.names(res)<-percent
    names(res)<-names(x)
    class(res) <- c("hrsize", "data.frame")
    attr(res, "units") <- attr(x, "units")
    return(res)
  }

