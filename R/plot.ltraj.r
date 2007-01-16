plot.ltraj <- function (x, id = unique(unlist(lapply(x, attr, which="id"))),
                        burst = unlist(lapply(x, attr, which="burst")),
                        asc = NULL, area = NULL, xlim = NULL,
                        ylim = NULL, colasc = gray((240:1)/256),
                        colpol = "green", addpoints = TRUE,
                        addlines = TRUE, perani = TRUE, final = TRUE, ...)
{
    polygon <- area
    if (!is.null(area)) {
        if (!inherits(area, "area"))
            stop("area should be an object of class area")
    }
    if (!inherits(x, "ltraj"))
        stop("x should be an object of class ltraj")

    ## supprimer les NA
    x <- lapply(x, function(i) {
        jj <- i[!is.na(i$x),]
        attr(jj, "id") <- attr(i,"id")
        attr(jj, "burst") <- attr(i,"burst")
        return(jj)
    })
    class(x) <- c("ltraj","list")
    id <- id
    burst <- burst
    x <- ltraj2traj(x)
    i <- split(x, x$id)
    x <- do.call("rbind", i[id])
    x$id <- factor(x$id)
    x$burst <- factor(x$burst)
    bu <- levels(x$burst)
    burst <- burst[burst%in%bu]

    i <- split(x, x$burst)
    x <- do.call("rbind", i[burst])
    x$id <- factor(x$id)
    x$burst <- factor(x$burst)

    if (!perani)
        idc <- "burst"
    else idc <- "id"
    li <- split(x, x[[idc]])
    id <- levels(x[[idc]])
    if (length(li)>1)
        opar <- par(mar = c(0.1, 0.1, 2, 0.1), mfrow = n2mfrow(length(li)))
    m <- unlist(lapply(li, function(x) mean(x$date)))
    nli <- names(li)
    nli <- nli[order(m)]

    if (is.null(xlim)) {
        maxxl <- max(unlist(lapply(li, function(ki) range(ki$x)[2] - range(ki$x)[1])))
        xlim <- lapply(li, function(ki) c(min(ki$x), min(ki$x)+maxxl))
    } else {
        ma <- max(unlist(lapply(li, function(ki) range(ki$x)[2])))
        mi <- min(unlist(lapply(li, function(ki) range(ki$x)[1])))
        xlim <- lapply(li, function(ki) c(mi,ma))
    }
    if (is.null(ylim)) {
        maxyl <- max(unlist(lapply(li, function(ki) range(ki$y)[2] - range(ki$y)[1])))
        ylim <- lapply(li, function(ki) c(min(ki$y), min(ki$y)+maxyl))
  } else {
      ma <- max(unlist(lapply(li, function(ki) range(ki$y)[2])))
      mi <- min(unlist(lapply(li, function(ki) range(ki$y)[1])))
      ylim <- lapply(li, function(ki) c(mi,ma))
  }
    names(xlim) <- names(li)
    names(ylim) <- names(li)

    for (i in nli) {
    if (!is.null(asc))
        image(asc, col = colasc, xlim = xlim[i][[1]], ylim = ylim[i][[1]],
              main = i,
              axes = (length(li)==1), ...)
    else plot(li[i][[1]]$x, li[i][[1]]$y, type = "n", asp = 1,
              xlim = xlim[i][[1]],
              ylim = ylim[i][[1]], axes = (length(li)==1),
              main = i, ...)
    box()
    if (!is.null(polygon)) {
        pol <- split(polygon[, 2:3], factor(polygon[, 1]))
        for (j in 1:length(pol)) polygon(pol[[j]], col = colpol)
    }
    if (addlines) {
        for (j in levels(factor(li[[i]]$burst))) {
            lines(x$x[x$burst == j], x$y[x$burst == j])
        }
    }
    if (addpoints) {
        for (j in levels(factor(li[[i]]$burst))) {
            points(x$x[x$burst == j], x$y[x$burst == j],
                   pch = 21, col = "black", bg = "white")
        }
    }
    if (final) {
      for (j in levels(factor(li[[i]]$burst))) {
          points(x$x[x$burst == j][c(1, length(x$x[x$burst ==
                     j]))], x$y[x$burst == j][c(1, length(x$y[x$burst ==
                                j]))], pch = 14, col = c("blue", "red"))
      }
  }
}
    if (length(li)>1)
    par(opar)
}
