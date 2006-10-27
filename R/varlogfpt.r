"varlogfpt" <-
function(f, graph=TRUE)
  {
    if (!inherits(f, "fipati"))
      stop("f should be of class 'fipati'")
    if (graph)
      opar <- par(mfrow=n2mfrow(length(f)))
    s <- attr(f, "radii")
    soso <- lapply(f, function(y) {
      so <- apply(y,2,function(z) var(log(z), na.rm=TRUE))
      if (graph)
        plot(s, so, ty="l", xlab="scale", ylab="Variance of log(FPT)",
             main=attr(y,"burst"))
      return(so)
    })
    soso <- as.data.frame(do.call("rbind",soso))
    row.names(soso) <- unlist(lapply(f, function(z) attr(z, "burst")))
    names(soso) <- paste("r",1:ncol(soso), sep="")
    attr(soso, "radii") <- attr(f,"radii")
    if (graph)
      par(opar)
    if (graph) {
      invisible(soso)
    } else {
      return(soso)
    }
  }

