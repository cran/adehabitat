"as.sahrlocs" <-
function(mlocs, mhr, msa, descan=NULL)
  {
    if (!inherits(mlocs, "kasc")) stop("non convenient data")
    if (!inherits(mhr, "kasc")) stop("non convenient data")
    if (!inherits(msa, "kasc")) stop("non convenient data")

    atze<-attributes(msa)
    
    nlocs<-nrow(as.data.frame(unclass(mlocs)))
    nhr<-nrow(as.data.frame(unclass(mhr)))
    nsa<-nrow(as.data.frame(unclass(msa)))

    if (!((nlocs==nhr)&(nlocs==nsa)))
      stop("the \"asc\" objects should describe the same area")

    nclocs<-ncol(as.data.frame(unclass(mlocs)))
    nchr<-ncol(as.data.frame(unclass(mhr)))
    if (nclocs!=nchr) stop("different number of individuals in mhr and mlocs")

    output<-list(sa=as.data.frame(unclass(msa)), hr=as.data.frame(unclass(mhr)),
                 locs=as.data.frame(unclass(mlocs)), descan=descan)
    
    attr(output, "nrow")<-atze$nrow
    attr(output, "ncol")<-atze$ncol
    attr(output, "xll")<-atze$xll
    attr(output, "yll")<-atze$yll
    attr(output, "cellsize")<-atze$cellsize
    class(output)<-"sahrlocs"

    return(output)
  }

