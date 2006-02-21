"as.area" <-
function(x)
  {
    if (!inherits(x, "data.frame"))
      stop("x should be of class \"data.frame\"")
    if (ncol(x) != 3)
      stop("x should have three columns")
    if (!is.factor(x[,1]))
      x<-factor(x[,1])
    class(x)<-c("area", "data.frame")
    return(x)
  }

