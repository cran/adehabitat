plot.hrsize <- function (x, ...)
{
    if (!inherits(x, "hrsize"))
        stop("should be of class hrsize")
    opar <- par(mfrow = n2mfrow(ncol(x)))
    on.exit(par(opar))
    if (!is.null(attr(x, "xlabel"))) {
       xlabel <- attr(x, "xlabel")
    } else {
       xlabel <- "Home-range level"
    }
    if (!is.null(attr(x, "ylabel"))) {
       ylabel <- attr(x, "ylabel")
    } else {
       ylabel <- paste("Home-range size (", attr(x, "units"), ")", sep = "")
    }

    for (i in 1:ncol(x)) {
        plot(as.numeric(row.names(x)), x[, i], main = names(x)[i],
            pch = 16, cex = 0.5, xlab = xlabel, ylab = ylabel)
        lines(as.numeric(row.names(x)), x[, i])
    }
}
