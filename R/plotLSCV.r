"plotLSCV" <-
function(x)
  {
    if (!inherits(x, "khrud"))
      stop("x should be an object of class \"khrud\"")
    opar<-par(mfrow=n2mfrow(length(x)))
    for (i in 1:length(x)) {
      plot(x[[i]]$h$CV[,1], x[[i]]$h$CV[,2], pch=16, main=names(x)[i],
           xlab="h parameter", ylab="CV(h)", cex=0.5)
      lines(x[[i]]$h$CV[,1], x[[i]]$h$CV[,2])
    }
    par(opar)
  }

