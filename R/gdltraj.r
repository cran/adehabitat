gdltraj <- function(x, min, max,
                         type=c("POSIXct","sec","min","hour",
                           "mday","mon","year","wday","yday"))
  {
    if (!inherits(x, "ltraj"))
      stop("x should be of class \"ltraj\"")
    type <- match.arg(type)
    if (type=="POSIXct") {
      x <- lapply(x, function(y) y[(y$date>min)&(y$date<max),])
    } else {
      x <- lapply(x, function(y) {
        da <- as.POSIXlt(y$date)[[type]]
        return(y[(da>=min)&(da<max),])
      })
    }
    if (all(sapply(x,nrow)==0))
      stop("No relocations within the specified interval")
    x[sapply(x, nrow)==0]<-NULL
    class(x) <- c("ltraj", "list")
    return(x)
  }
