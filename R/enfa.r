"enfa" <-
function (tab, pr, scannf = TRUE, nf = 1) 
{
    call <- match.call()
    if (any(is.na(tab))) 
        stop("na entries in table")
    if (!is.vector(pr))
        stop("pr should be a vector")
    if (any(unlist(lapply(tab, is.factor))))
      stop("factors not yet implemented")
    row.w <- rep(1, nrow(tab))/nrow(tab)
    f1 <- function(v) sum(v * row.w)/sum(row.w)
    f2 <- function(v) sqrt(sum(v * v * row.w)/sum(row.w))
    center <- apply(tab, 2, f1)
    tab <- sweep(tab, 2, center)
    norm <- apply(tab, 2, f2)
    norm[norm < 1e-08] <- 1
    tab <- as.matrix(sweep(tab, 2, norm, "/"))
    lw <- pr/sum(pr)
    Rg <- crossprod(tab)/nrow(tab)
    ZtQ <- apply(tab, 2, function(x) x * lw)
    Rs <- crossprod(ZtQ, tab)
    mar <- colSums(ZtQ)
    m <- sum(mar^2)
    eRs <- eigen(Rs)
    Rs12 <- eRs$vectors %*% diag(eRs$values^(-1/2)) %*% t(eRs$vectors)
    z <- Rs12 %*% mar
    y <- z/as.numeric(sqrt(crossprod(z)))
    W <- Rs12 %*% Rg %*% Rs12
    H <- (diag(ncol(tab)) - y %*% t(y)) %*% W %*% (diag(ncol(tab)) - 
        y %*% t(y))
    s <- eigen(H)$values[-ncol(tab)]
    if (scannf) {
        barplot(s)
        cat("Select the number of specialization axes: ")
        nf <- as.integer(readLines(n = 1))
    }
    if (nf <= 0 | nf > (ncol(tab) - 1)) 
        nf <- 1
    co <- matrix(nrow = ncol(tab), ncol = nf + 1)
    co[, 1] <- mar
    co[, 2:(nf + 1)] <- (Rs12 %*% eigen(H)$vectors)[, 1:nf]
    f3 <- function(i) co[, i]/sqrt(crossprod(co[, i])/length(co[, 
        i]))
    c1 <- matrix(unlist(lapply(1:(nf + 1), f3)), ncol(tab))/sqrt(nrow(co))
    li <- data.frame(tab %*% c1[, 1:(nf + 1)])
    c1 <- data.frame(c1)
    names(c1) <- c("Mar", paste("Spe", (1:nf), sep = ""))
    row.names(c1) <- dimnames(tab)[[2]]
    names(li) <- c("Mar", paste("Spe", (1:nf), sep = ""))
    enfa <- list(call = call, tab = data.frame(tab), pr = pr, 
                 nf = nf, m = m, s = s, lw = lw, li = li, co = c1, 
                 mar = mar)
    class(enfa) <- "enfa"
    return(invisible(enfa))
}

