"plot.ngNNCH" <-
function(x, ...)
  {
    if (!inherits(x, "ngNNCH"))
      stop("x should be of class \"ngNNCH\"")
    xx<-as.numeric(row.names(x))
    if (ncol(x)!=1) {
      opar <- par(mfrow=n2mfrow(ncol(x)))
      on.exit(par(opar))
    }
    if (ncol(x)>1)
      lapply(1:ncol(x), function(y) plot(as.numeric(rownames(x)), x[,y],
                                         main=colnames(x)[y],
                                         xlab="Number of neighbors",
                                         ylab="Home-range size",
                                         pch = 16, ty="b",...))
    if (ncol(x)==1)
      plot(as.numeric(rownames(x)), x[,1], xlab="Number of neighbors",
           ylab="Home-range size", pch = 16, ty="b",...)
    
  }

