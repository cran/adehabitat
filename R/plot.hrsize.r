"plot.hrsize" <-
function(x, ...)
  {
    if (!inherits(x, "hrsize"))
      stop("should be of class hrsize")
    opar<-par(mfrow=n2mfrow(ncol(x)))
    on.exit(par(opar))
    for (i in 1:ncol(x)) {
      plot(as.numeric(row.names(x)),
           x[,i],
           main=names(x)[i], pch=16, cex=0.5,
           xlab="Home-range level",
           ylab=paste("Home-range size (",attr(x, "units"),")",sep=""))
      lines(as.numeric(row.names(x)),
            x[,i])
    }
  }

