"sahrlocs2niche" <-
function(x,  ani=names(x$hr),
                         var=names(x$sa), used=c("hr", "locs"))
  {
    used<-match.arg(used)
    if (!inherits(x,"sahrlocs")) stop("non convenient data")
    output<-list()
    sa<-getsahrlocs(x)
    sa<-sa[var]
    hr<-x$hr[ani]
    locs<-x$locs[ani]
    class(sa)<-c("kasc", "data.frame")
    e<-kasc2df(sa)
    output$tab<-e$tab
    output$index<-e$index

    if (used=="hr") {
      Y<-hr
      for (i in 1:ncol(hr)) Y[is.na(Y[,i]),i]<-0
    }
    if (used=="locs") Y<-locs
    Y<-Y[e$index,]

    output$y<-as.data.frame(Y)

    return(output)
        
  }

