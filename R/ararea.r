"ararea" <-
function(x)
  {
    if (!inherits(x, "area"))
      stop("x should be of class \"area\"")
    if (!require(gpclib))
      stop("package gpclib needed for this function")
    uu <- split(x[,2:3], x[,1])
    foo <- function(y) {
      class(y) <- "data.frame"
      u <- area.poly(as(y, "gpc.poly"))
    }
    res <- unlist(lapply(uu, foo))
    names(res) <- names(uu)
    return(res)
  }

