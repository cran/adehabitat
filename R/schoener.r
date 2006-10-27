"schoener" <-
function(tr, keep, byburst=TRUE)
  {
    if (!inherits(tr, "traj"))
      stop("tr should be of class traj")
    li <- split(tr, tr$id)
    if (byburst)
      li <- split(tr, tr$burst)

    foo <- function(tr) {
      d <- unclass(tr$date)
      x <- tr[,c("x","y")]
      r2 <- sum(((x[,1]-mean(x[,1]))^2) +
                ((x[,2]-mean(x[,2]))^2))/(nrow(x) -1)
      diffd <- outer(d,d,"-")
      t2tmp <- as.matrix(dist(x)^2)
      cons <- diffd>keep[1]&diffd<keep[2]
      t2 <- sum(t2tmp[cons])/sum(cons)
      rat <- t2/r2
      n <- nrow(x)
      m <- sum(cons)
      return(c(rat, n, m))
    }

    rr <- do.call("rbind", lapply(li, foo))
    rr <- as.data.frame(rr)
    row.names(rr) <- names(li)
    names(rr) <- c("value","n","m")
    return(rr)
  }

