"hist.kselect" <-
function(x, xax = 1, mar=c(0,0,0,0), ampl=1,
                       col.out=gray(0.75), col.in=gray(0.75), ncell=TRUE,
                       denout=NULL, denin=NULL, lwdout=1, lwdin=1,
                       maxy=1, csub=2,
                       possub=c("bottomleft", "topleft", "bottomright", "topright"),
                       ncla=15, ...)
  {
    possub<-match.arg(possub)
    if (!inherits(x, "kselect")) stop("should be a 'kselect' object")
    
    ## 1. Creation de la liste
    Xi<-x$initab
    Xrecalc<-t(as.matrix(apply(Xi, 1, function(y) y*x$lw/sum(x$lw))))%*%as.matrix(x$l1)
    li.Xi<-split(as.data.frame(Xrecalc), x$initfac)
    li.wei<-split(x$initwei, x$initfac)
    rx<-range(Xrecalc[,xax])
    br<-seq(rx[1]-(rx[2]-rx[1])/100, rx[2]+(rx[2]-rx[1])/100, length=ncla)
    
    def.par <- par(no.readonly = TRUE)
    on.exit(par(def.par))
    ngraph<-length(li.Xi)
    par(mfrow = n2mfrow(ngraph+1), mar=mar)

    for (i in 1:ngraph) {
      Xtmp<-li.Xi[[i]]
      wgtmp<-li.wei[[i]]

      ## Histogramme extérieur
      vext<-Xtmp[,xax]
      
      ## Histogramme interieur
      poids<-wgtmp
      if (ncell) poids[poids>0]<-1
      vint<-rep(vext,poids)

      ## Calcul des histogrammes
      h<-hist(vext, plot=FALSE, breaks=br, ...)
      hhr<-hist(vint, breaks=h$breaks, plot=FALSE, ...)
      plot(rx, c(-maxy, maxy), type="n",
                     axes=FALSE, ylim=c(-maxy,maxy),
                     main="")
      
      ## Trace des histogrammes
      p<--hhr$counts/sum(hhr$counts)
      q<-h$counts/sum(h$counts)
      rect(hhr$breaks[-length(hhr$breaks)], 0, hhr$breaks[-1],
           p*ampl, col=col.in, lwd=lwdin, density=denin)
      rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1],
           q*ampl, col=col.out, lwd=lwdout, density=denout)
      arrows(mean(vext),0.2, mean(vext), 0, lwd=2, angle=20, length=0.1)
      arrows(mean(vint, na.rm=TRUE), -0.2,mean(vint, na.rm=TRUE),
             0, lwd=2, angle=20, length=0.1)
      scatterutil.sub(names(li.Xi)[i],
                       csub=csub, possub=possub)
      box()

      
    }
    
    plot(c(-2,2),c(-2,2), type="n", axes=FALSE, xlab="", ylab="")
    lines(c(0,0), c(-1, 1), lwd=2)
    lines(c(-0.1,0.1), c(-1, -1), lwd=2)
    lines(c(-0.1,0.1), c(1,1), lwd=2)
    text(0, 1.5, as.character(round(maxy/ampl,2)), cex=1.5)
    text(0, -1.5, as.character(round(-maxy/ampl, 2)), cex=1.5)
    lines(c(-1,1), c(0,0), lwd=2)
    lines(c(-1,-1), c(-0.1,0.1), lwd=2)
    lines(c(1,1), c(-0.1,0.1), lwd=2)

    text(1.5, 0, as.character(round(rx[2],2)), cex=1.5)
    text(-1.5, 0, as.character(round(rx[1], 2)), cex=1.5)
  }

