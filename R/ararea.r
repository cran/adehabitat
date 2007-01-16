"ararea" <- function(x)
  {
      ## Verifications
      if (!inherits(x, "area"))
          stop("x should be of class \"area\"")

      ## package gpclib needed
      if (!require(gpclib))
          stop("package gpclib needed for this function")

      ## Computes the area of each polygon
      uu <- split(x[,2:3], x[,1])
      foo <- function(y) {
          class(y) <- "data.frame"
          u <- area.poly(as(y, "gpc.poly"))
      }

      ## Output
      res <- unlist(lapply(uu, foo))
      names(res) <- names(uu)
      return(res)
  }

