"getcontour" <-
function(x)
  {

    if (!inherits(x, "asc"))
      stop("should be an object of class asc")
    xyc<-getXYcoords(x)


    rajfond<-function(x)
      {
        nr<-nrow(x)
        nc<-ncol(x)
        
        f<-rep(0,nr)
        x<-cbind(f,x,f)
        f<-rep(0,nc+2)
        x<-rbind(f,x,f)
      }
    
    x[!is.na(x)]<-1
    x[is.na(x)]<-0

    x<-rajfond(x)
    toto<-.C("seqeticorr", as.double(t(x)), as.integer(nrow(x)),
             as.integer(ncol(x)), PACKAGE="adehabitat")
    etiquete<-matrix(toto[[1]], nrow=nrow(x), byrow=TRUE)
    etiquete<-etiquete[-c(1,nrow(etiquete)),-c(1,ncol(etiquete))]
    entree<-list()
    sorties<-c(0, 0, 0)
    lev<-levels(factor(toto[[1]]))
    lev<-lev[lev!="0"]
    for (i in lev) {
      j<-as.numeric(i)
      tmp<-etiquete
      tmp[tmp!=j]<-0
      tmp[tmp==j]<-1
      tmp<-rajfond(tmp)
      if (sum(as.vector(tmp))<3)
        stop("The parts of the object should contain at least 3 pixels")
      toto<-.C("lcontour", as.double(t(tmp)), as.integer(nrow(tmp)),
               as.integer(ncol(tmp)),  as.integer(0), PACKAGE="adehabitat")[[4]]
      pol<-.C("getcontour", as.double(t(tmp)), as.integer(nrow(tmp)),
              as.integer(ncol(tmp)), integer(toto), integer(toto),
              as.integer(toto), PACKAGE="adehabitat")
      xt<-c(0,xyc$x,0)
      yt<-c(0,xyc$y,0)
      x<-xt[pol[[4]]]
      y<-yt[pol[[5]]]
      sorties<-rbind(sorties, cbind(rep(j,length(x)), x, y))
    }
    sorties<-sorties[-1,]
    row.names(sorties)<-1:nrow(sorties)
    sorties<-as.data.frame(sorties)
    sorties[,1]<-factor(sorties[,1])
    names(sorties)<-c("id","x", "y")
    sorties<-as.area(sorties)
    return(sorties)
  }

