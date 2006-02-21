"image.asc" <-
function (x, clfac = NULL, col = gray((240:1)/256), ...) 
{
    if (!inherits(x, "asc")) 
        stop("not an \"asc\" object")
    z <- x
    xy <- getXYcoords(z)
    x <- xy$x
    y <- xy$y
    if (attr(z, "type") == "numeric") 
        image(x = x, y = y, z, asp = 1, col = col, ...)
    if (attr(z, "type") == "factor") {
        if (is.null(clfac)) {
            clfac <- rainbow(nlevels(z))
            clfac <- clfac[as.numeric(levels(factor(z)))]
        }
        image(x = x, y = y, z, asp = 1, col = clfac, ...)
    }
}

