"plot.fipati" <-
function(x, scale, warn = TRUE, ...)
  {
    if (!inherits(x, "fipati"))
      stop("x should be of class 'fipati'")
    opar <- par(mfrow=n2mfrow(length(x)))
    att <- attr(x, "radii")
    ind <- which.min(abs(att-scale))
    if (warn)
      if (abs(att[ind] - scale) > 1e-7)
        warning(paste("No radius equal to ",scale,
                      ", displayed radius is ", att[ind], sep =""))
    lapply(1:length(x), function(i) {
      u <- x[[i]]
      plot(attr(u,"date"), u[,ind], main = attr(u, "burst"), ylab="FPT",...)
      lines(attr(u,"date"), u[,ind])})
    par(opar)
  }

