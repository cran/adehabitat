"asc2spixdf" <-
function(a)
  {
    if (!inherits(a, "asc"))
      stop("a should be of class \"asc\"")
    if (!require(sp))
      stop("the package sp is required for this function")
    xyc <- getXYcoords(a)
    xc <- rep(xyc$x, times=length(xyc$y))
    yc <- rep(xyc$y, each=length(xyc$x))
    xyc<-data.frame(x=xc,y=yc)
    cons <- (1:length(c(a)))[!is.na(c(a))]
    var <- c(a)[cons]
    xyc <- xyc[cons,]
    names(xyc) <- c("x","y")
    df1 <- data.frame(xyc, var)
    coordinates(df1) <- c("x","y")
    gridded(df1) <- TRUE
    return(df1)
  }

