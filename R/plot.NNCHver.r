"plot.NNCHver" <-
function(x, which = names(x),
                       colpol = NA,
                       colborder = rep("black", length(x)),
                       lwd = 2, add = FALSE, ...)
  {
    if (!inherits(x, "NNCHver"))
      stop("y should be of class NNCHver")
    xt<-unlist(lapply(x, function(x) lapply(attr(x, "pts"), function(i) i$x)))
    yt<-unlist(lapply(x, function(x) lapply(attr(x, "pts"), function(i) i$y)))

    if (!add)
      plot(xt, yt, asp=1, ty = "n", ...)

    res<-x[which]
    
    lapply(1:length(res), function(x) plot(res[[x]],
                                           poly.args = list(col = colpol[x],
                                           border = colborder[x], lwd = lwd,
                                           ...), add=TRUE))
    invisible(NULL)
    
  }

