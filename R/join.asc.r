"join.asc" <-
function(pts, x)
  {
    if (!inherits(x, "asc")) stop("non convenient data")
    xy<-getXYcoords(x)
    xy$x<-xy$x+attr(x, "cellsize")/2
    xy$x<-c(xy$x, xy$x[1]-attr(x, "cellsize")/2)
    xy$y<-xy$y+attr(x, "cellsize")/2
    xy$y<-c(xy$y, xy$y[1]-attr(x, "cellsize")/2)

    xf<-as.numeric(cut(pts[,1], xy$x))
    yf<-as.numeric(cut(pts[,2], xy$y))

    fact<-0
    if (attr(x, "type")=="factor") 
      ct<-attr(x, "levels")

    for (i in 1:nrow(pts)) {
      if (attr(x, "type")=="numeric") {
        u<-x[xf[i],yf[i]]
        fact[i]<-u
      }
      if (attr(x, "type")=="factor") {
        u<-x[xf[i],yf[i]]
        tmp<-ct[u]
        if (length(tmp)==1) {
          fact[i]<-tmp
        } else {
          fact[i]<-NA
        }
      }
    }
    if (attr(x, "type")=="factor") fact<-factor(fact)
    return(fact)
  }

