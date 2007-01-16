"plot.kselect" <- function(x, xax=1, yax=2, ...)
{
    ## Verifications
    if (!inherits(x, "kselect"))
        stop("Use only with 'kselect' objects")
    if (x$nf == 1) {
        warnings("One axis only : not yet implemented")
        return(invisible())
    }
    if (xax > x$nf)
        stop("Non convenient xax")
    if (yax > x$nf)
        stop("Non convenient yax")
    def.par <- par(no.readonly = TRUE)
    on.exit(par(def.par))

    ## The layout of the graphs
    nf <- layout(matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3),
                 respect = TRUE)
    par(mar = c(0.1, 0.1, 0.1, 0.1))

    ## 1. correlations between the PCA axes and the K-select axes
    s.corcircle(x$as, xax, yax, sub = "Axis", csub = 2, clab = 1.25)

    ## 2. scores of the environmental variables on the
    ##    K-select axes (eigenvectors)
    s.arrow(x$l1, xax, yax, sub = "Variables", csub = 2, clab = 1.25)

    ## 3. the eigenvalues of the analysis
    scatterutil.eigen(x$eig, wsel = c(xax, yax))

    ## 4. main graph: coordinates of the uncentered marginality vectors

    ## 4.1. We project the available points (all animals pooled) on
    ##      the axes of the K-select: matrix U (rows: points, columns: axes)
    U<-as.matrix(x$l1*x$lw)
    ls<-as.matrix(x$initab)%*%U

    ## 4.2. coordinates of the "available centroids" on the axes
    ##      of the K-select
    liani<-split(as.data.frame(ls), x$initfac)
    liwei<-split(x$initwei, x$initfac)
    mav<-as.data.frame(t(as.matrix(data.frame(lapply(liani,
                                                     function(x) apply(x, 2, mean))))))
    names(mav)<-names(x$li)
    row.names(mav)<-names(x$tab)

    ## 4.3. coordinates of the "used centroids" on the axes
    ##      of the K-select
    mutemp<-list()
    for (i in 1:length(liwei))
        mutemp[[i]]<-apply(liani[[i]], 2, function(x) weighted.mean(x, liwei[[i]]))
    mut<-as.data.frame(t(as.matrix(data.frame(mutemp))))
    names(mut)<-names(x$li)
    row.names(mut)<-names(x$tab)

    ## 4.4. The Marginality vectors are displayed as arrows connecting the
    ##      "available" centroids to the "used" centroids

    s.label(rbind(mav, mut), xax, yax, clab = 0, cpo = 0,
            sub = "Marginality vectors", csub = 2) ## background
    for (i in 1:length(liani))
        arrows(mav[i,xax], mav[i,yax], mut[i,xax], mut[i,yax],
               lwd=2, angle=20) ## arrows
    s.label(mav, xax, yax, add.plot=TRUE, clab=1.5) ## labels


    ## 5. coordinates of the uncentered available points on the
    ##    axes of the K-select
    s.class(as.data.frame(ls), x$initfac, cstar=0,
            cellipse=0, clab=1.5, sub="Available Resource units", csub=2)
    for (i in 1:length(liani))
        polygon(liani[[i]][chull(liani[[i]][,xax], liani[[i]][,yax]),xax],
                liani[[i]][chull(liani[[i]][,xax], liani[[i]][,yax]),yax])

    ## 6. coordinates of the recentred marginality vectors on the axes
    ##    on the K-select
    s.arrow(x$co, xax, yax, clab = 1.25, cpo = 0.5, sub = "Animals",
            csub = 2)

}

