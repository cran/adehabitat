"mcp.area" <- function(xy, id, percent = seq(20,100, by=5),
                       unin=c("m", "km"),
                       unout=c("ha", "km2", "m2"), plotit = TRUE)
{
    ## Verifications
    unin<-match.arg(unin)
    unout<-match.arg(unout)
    if (length(id) != nrow(xy))
        stop("xy and id should be of the same length")
    if (!require(gpclib))
        stop("package gpclib required")

    ## remove the missing values
    xy <- xy[!is.na(xy[, 1]), ]
    xy <- xy[!is.na(xy[, 2]), ]
    id <- id[!is.na(xy[, 1])]
    id <- id[!is.na(xy[, 2])]

    ## Bases
    lev<-percent
    res<-list()
    ar<-matrix(0,nrow=length(lev),
               ncol=nlevels(factor(id)))

    ## For each home range level, computes the MCP, and its area
    for (i in 1:length(lev)) {
        res[[i]]<-mcp(xy, id, percent=lev[i])
        class(res[[i]])<-"data.frame"
        res[[i]]<-split(res[[i]][,2:3], res[[i]][,1])
        for (j in 1:nlevels(factor(id)))
            ar[i,j]<-area.poly(as(res[[i]][[j]], "gpc.poly"))
    }

    ar <- as.data.frame(ar)
    names(ar)<-levels(factor(id))

    ## output units
    if (unin=="m") {
        if (unout=="ha")
            ar<-ar/10000
        if (unout=="km2")
            ar<-ar/1000000
    }
    if (unin=="km") {
        if (unout=="ha")
            ar<-ar*100
        if (unout=="m2")
            ar<-ar*1000000
    }

    ## output
    row.names(ar)<-lev
    class(ar)<-c("hrsize", "data.frame")
    attr(ar, "units")<-unout
    if (plotit)
        plot(ar)
    return(ar)
}

