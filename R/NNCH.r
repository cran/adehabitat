"NNCH" <-
function(xy, id=NULL, k=10, unin = c("m", "km"),
               unout = c("ha", "km2", "m2"))
  {
    if (ncol(xy)!=2)
      stop("xy should have two columns")
    if (is.null(id))
      id<-rep(1,nrow(xy))
    id<-factor(id)
    
    if (k>=min(table(id)))
      stop("too large number of neighbors")
    if (nrow(xy)!=length(id))
      stop("id should have the same length as xy")
      if (min(table(id))<5)
    stop("At least 5 relocations are required to fit an home range")

    id <- id[!is.na(xy[, 1])]
    xy <- xy[!is.na(xy[, 1]), ]
    id <- id[!is.na(xy[, 2])]
    xy <- xy[!is.na(xy[, 2]), ]
    unin <- match.arg(unin)
    unout <- match.arg(unout)
    class(xy[,1])<-"double"
    class(xy[,2])<-"double"
    if (!require(gpclib)) 
      stop("package gpclib required")
    res<-list()
    
    for (kk in 1:nlevels(id)) {
      xyt<-xy[id==levels(id)[kk],]
      
      li<-list()
      li2<-list()
      lin<-list()
      lin2<-list()
      ar<-0
      
      dij<-as.matrix(dist(xyt))
      idt<-1:nrow(dij)
      
      for (i in 1:nrow(xyt)) {
        iid<-idt[order(dij[i,])][1:k]
        xytmp<-xyt[iid,]
        ch<-chull(xytmp[,1], xytmp[,2])
        li[[i]]<-as(xytmp[ch,], "gpc.poly")
        lin[[i]]<-iid
      }
    
      aa<-unlist(lapply(li, area.poly))
      li<-li[order(aa)]
      lin<-lin[order(aa)]
      idbis<-idt[order(aa)]
      li2[[1]]<-li[[1]]
      lin2[[1]]<-lin[[1]]
      
      for (i in 2:length(li)) {
        li2[[i]]<-union(li2[[i-1]], li[[i]])
        lin2[[i]]<-unique(c(lin2[[i-1]], lin[[i]]))
      }
      
      n<-unlist(lapply(lin2, length))/nrow(xyt)
      ar<-unlist(lapply(li2, area.poly))

      cons<-1:length(ar)
      
      if (unin == "m") {
        if (unout == "ha") 
          ar <- ar/10000
        if (unout == "km2") 
          ar <- ar/1e+06
      }
      if (unin == "km") {
        if (unout == "ha") 
          ar <- ar * 100
        if (unout == "m2") 
          ar <- ar * 1e+06
      }

      
      names(li2)<-round(n*100)
      area<-data.frame(levels=round(n*100,2), area=ar)
      dup<-!duplicated(area)
      area=area[dup,]
      row.names(area)<-1:nrow(area)
      res[[levels(id)[kk]]]<-list(area=area,
                                  polygons=li2[dup], xy=xyt)
      

    }
    attr(res, "units") <- unout
    class(res)<-"NNCH"
    return(res)
  }

