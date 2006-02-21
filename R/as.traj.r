"as.traj" <-
function(id, xy, date, burst=id, ...)
  {
    if (ncol(xy)!=2)
      stop("xy should have two columns")
    if (!inherits(date, "POSIXct"))
      stop("date should be of class \POSIXct\"")
    id <- factor(id)
    burst <- factor(burst)
    if (!all(apply(table(id,burst)>0,2,sum)==1))
      stop("one burst level should belong to only one id level")
  
    names(xy)<-c("x", "y")
    bas<-data.frame(id=id,
                    xy, date=date, burst=burst, ...)
    foo<-function(x) x[order(x$date),]
    li<-split(bas, bas$burst)
    nl <- unlist(lapply(li, nrow)) > 1
    li <- li[nl]
    if (any(!nl))
      warning(paste("At least two relocations are needed for a burst:\n",
                    sum(!nl), "circuits have been deleted"))
    li<-lapply(li, foo)
    
    ## Vérification que pas de doublons au niveau des dates
    foob<-function(x) {
      ind<-rep(0,nrow(x))
      for (i in 2:nrow(x)) {
        if ((as.numeric(x$date))[i]==(as.numeric(x$date))[i-1])
          ind[i]<-1
      }
      return(x[ind==0,])
    }
    
    li<-lapply(li, foob)
    bas<-do.call("rbind", li)
    row.names(bas)<-as.character(1:nrow(bas))
    bas$id <- factor(bas$id)
    bas$burst <- factor(bas$burst)
    class(bas)<-c("traj", "data.frame")
    return(bas)
  }

