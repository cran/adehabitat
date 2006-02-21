"plot.area" <-
function(x, which=levels(x[,1]),
                      colpol = rep("green", nlevels(x[,1])),
                      colborder = rep("black", nlevels(x[,1])),
                      lwd = 2, add = FALSE, ...)
  {
    if (!inherits(x, "area"))
      stop("x should be of class \"area\"")
    if (!add)
      plot.default(x[,2:3], type = "n", asp = 1,...)
    li <- split(x[,2:3], x[,1])
    lapply(1:length(which), function(i) polygon(li[[which[i]]],
                                                col = colpol[i],
                                                border = colborder[i],
                                                lwd = lwd))
    invisible(NULL)
  }

