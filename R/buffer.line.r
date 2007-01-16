"buffer.line" <- function(xy, x, dist)
{
    ## Verifications
    if (inherits(x, "kasc"))
        x <- getkasc(x, 1)
    if (!inherits(x, "asc"))
        stop("x should be an object of class asc")

    ## First remove the eventual missing values
    xy <- xy[!is.na(xy[,1]),]
    xy <- xy[!is.na(xy[,2]),]

    ## very small jitter to avoid the points loated exactly on the
    ## limit between two pixels
    ra<- attr(x, "cellsize")/100
    xy[,1]<-jitter(xy[,1], amount=ra)
    xy[,2]<-jitter(xy[,2], amount=ra)

    ## a first buffer on the points
    bu <- buffer(xy, x, dist)
    bu[is.na(bu)]<-0

    ## preparation for a buffer on the lines
    carter <- matrix(0, nrow=nrow(x), ncol = ncol(x))
    xyg <- getXYcoords(x)
    xgr<-xyg$x
    ygr<-xyg$y

    ## the above results are passed as arguments to the C function
    ## "bufligr" which computes a buffer around the lines
    toto <- .C("bufligr", as.double(t(xy)), as.double(dist),
               as.double(t(carter)), as.double(xgr),
               as.double(ygr), as.integer(nrow(x)),
               as.integer(ncol(x)), as.integer(nrow(xy)),
               PACKAGE="adehabitat")[[3]]

    ## the buffer on the line is summed to the buffer on the points
    output <- matrix(toto, nrow = nrow(x), byrow = TRUE)
    output <- output + bu
    output[output>0]<-1

    ## Output as an asc object
    output[output == 0] <- NA
    attr(output, "xll") <- attr(x, "xll")
    attr(output, "yll") <- attr(x, "yll")
    attr(output, "cellsize") <- attr(x, "cellsize")
    attr(output, "type") <- "numeric"
    class(output) <- "asc"
    return(output)
}

