"distfacmap" <-
function(x)
  {
    if (!inherits(x, "asc"))
      stop("x should be of class \"asc\"")
    if (attr(x, "type")!="factor")
      stop("x should be of type \"factor\"")
    xyc <- getXYcoords(x)
    xc <- rep(xyc$x, times=length(xyc$y))
    yc <- rep(xyc$y, each=length(xyc$x))
    xyc<-data.frame(x=xc,y=yc)
    lev <- as.numeric(levels(factor(c(x))))
    li <- list()
    
    for (i in lev) {
      tmp <- x
      tmp[x!=i] <- NA
      tmp[x==i] <- 1
      ptsoui <- xyc[!is.na(c(tmp)),]
      toto <- .C("distxyr", as.double(t(as.matrix(xyc))),
                 as.double(t(as.matrix(ptsoui))),
                 as.integer(nrow(xyc)), as.integer(nrow(ptsoui)),
                 double(nrow(xyc)), PACKAGE="adehabitat")
      li[[i]] <- toto[[5]]
    }
    names(li) <- levels(x)
    ka <- as.kasc(list(x1=x))
    li <- as.data.frame(li)
    li <- getkascattr(ka,li)
    li <- setmask(li, x)
    return(li)
  }

