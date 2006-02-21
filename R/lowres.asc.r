"lowres.asc" <-
function(x, np=2, ...)
  {
    if (!inherits(x, "asc"))
      stop("x sould be of class \"asc\"")
    nr<-nrow(x)
    nc<-ncol(x)
    xy<-getXYcoords(x)
    cs<-attr(x, "cellsize")
    if (attr(x, "type")=="factor") {
      typ <- "factor"
      lev <- levels(x)
    } else {
      typ <- "numeric"
    }
    x<-x[1:(nr-(((nr/np)-floor(nr/np)))*np),1:(nc-(((nc/np)-floor(nc/np)))*np)]
    nr<-nrow(x)
    nc<-ncol(x)

    if (typ=="factor") {
      repr<- as.numeric(levels(factor(as.vector(x))))
      lev <- lev[repr]
      x <- as.numeric(as.character(factor(x)))
      x <- matrix(x, nrow=nr, ncol=nc)
    }
    
    x[is.na(x)]<--9999
    xs<-matrix(0, nrow=nr/np, ncol=nc/np)
    if (typ == "numeric") {
      mat<-.C("regrouascnumr", as.double(t(x)), as.double(t(xs)),
              as.double(nrow(x)), as.double(ncol(x)),
              as.double(nrow(xs)), as.double(ncol(xs)), PACKAGE = "adehabitat")[[2]]
    } else {
      mat<-.C("regroufacascr", as.double(t(x)), as.double(t(xs)), as.integer(np),
              as.integer(length(lev)), as.integer(nrow(x)), as.integer(ncol(x)),
              as.integer(nrow(xs)), as.integer(ncol(xs)), PACKAGE = "adehabitat")[[2]]
    }
    mat<-matrix(mat,ncol=ncol(xs), byrow=TRUE)
    mat[mat==-9999]<-NA
    attr(mat, "xll")<-mean(xy$x[1:np])
    attr(mat, "yll")<-mean(xy$y[1:np])
    attr(mat, "cellsize")<-cs*np
    attr(mat, "type")<-typ
    if (typ == "factor")
      attr(mat, "levels") <- lev
    class(mat)<-"asc"
    return(mat)
  }

