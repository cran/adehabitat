"predict.enfa" <-
function (object, index, attr, nf, ...)
{
    if (!inherits(object, "enfa"))
        stop("should be an object of class \"enfa\"")
    warning("the enfa is not mathematically optimal for prediction:\n please consider the madifa instead")
    if ((missing(nf)) || (nf > object$nf))
        nf <- object$nf
    Zli <- object$li[, 1:(nf + 1)]
    f1 <- function(x) rep(x, object$pr)
    Sli <- apply(Zli, 2, f1)
    m <- apply(Sli, 2, mean)
    cov <- t(as.matrix(Sli)) %*% as.matrix(Sli)/nrow(Sli)
    maha <- mahalanobis(Zli, center = m, cov = cov)
    map <- getkasc(df2kasc(data.frame(toto = maha, tutu = maha),
        index, attr), "toto")
    return(invisible(map))
}

