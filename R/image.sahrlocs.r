"image.sahrlocs" <-
function(x, ani=names(x$hr),
                         var=names(x$sa),
                         mar=c(0,0,0,0), axes=FALSE, dfidxy=NULL,
                         colpts="black", pch=21, bg="white", inv=FALSE, cexpts=0.6,
                         csub=2, possub=c("bottomleft", "bottomright",
                                   "topleft", "topright"), ...)
  {
    possub<-match.arg(possub)
    if (!inherits(x, "sahrlocs"))
      stop("The object x should be of \"sahrlocs\" type")
    ngraph<-length(ani)*length(var)
    opar<-par(mfrow=n2mfrow(ngraph), mar=mar)
    on.exit(par(opar))
    if (!is.null(dfidxy)) lxy<-split(dfidxy, dfidxy[,1])

    ## Creation d'un mini-objet compo.hr
    hr<-x$hr[ani]
    sa<-x$sa[var]
    chr<-list()

    for (i in 1:length(names(hr))) {
      hrt<-hr[,i]
      so<-sa
      so$ani9999<-hrt
      class(so)<-c("kasc", "data.frame")
      so<-managNAkasc(so)
      chr[[names(hr)[i]]]<-so[names(so)!="ani9999"]
    }

    xy<-getXYcoords(x)
    xc<-xy$x
    yc<-xy$y

    ## Calcul du range
    r<-list()
    minx<-0
    maxx<-0
    miny<-0
    maxy<-0
    
    for (i in 1:length(ani)) {
      rtmp<-matrix(chr[[ani[i]]][[1]], ncol=attr(x, "nrow"))
      rowx<-row(rtmp)
      coly<-col(rtmp)
      minx[i]<-min(rowx[!is.na(rtmp)])
      maxx[i]<-max(rowx[!is.na(rtmp)])
      miny[i]<-min(coly[!is.na(rtmp)])
      maxy[i]<-max(coly[!is.na(rtmp)])
      r[[i]]<-c(maxx[i]-minx[i], maxy[i]-miny[i])
    }
    r<-as.data.frame(r)
    rx<-max(r[1,])*(attr(x, "cellsize"))
    ry<-max(r[2,])*(attr(x, "cellsize"))
    cou<-gray((256:1)/256)
    if (inv) cou<-gray((1:256)/256)

    
    for (i in 1:length(ani)){
      for (j in 1:length(var)){
        df<-chr[[ani[i]]]
        class(df)<-"data.frame"
        
        if (is.numeric(df[[var[j]]])) {
          im<-matrix(df[[var[j]]], ncol=attr(x, "nrow"))
          mx<-min(x$sa[[var[j]]][!is.na(x$sa[[var[j]]])])
          Mx<-max(x$sa[[var[j]]][!is.na(x$sa[[var[j]]])])
          mxMx<-seq(mx, Mx, length=256)
          mx1<-min(im[!is.na(im)])
          Mx1<-max(im[!is.na(im)])
          cou1<-cou[(mxMx>mx1)&(mxMx<Mx1)]
          
          image(xc, yc, im, xlim=c(xc[minx[i]]-rx/5, xc[minx[i]]+6*rx/5),
                ylim=c(yc[miny[i]], yc[miny[i]]+ry), asp=1,
                , axes=axes, col=cou1, ...)
          box()
          scatterutil.sub(paste(ani[i]," : ",var[j]),
                          csub=csub, possub=possub)
        } else {
          im<-matrix(as.numeric(df[[var[j]]]), ncol=attr(x, "nrow"))
          image(xc, yc, im, xlim=c(xc[minx[i]]-rx/5, xc[minx[i]]+6*rx/5),
                ylim=c(yc[miny[i]], yc[miny[i]]+ry), asp=1,
                , axes=axes, col=rainbow(nlevels(df[[var[j]]])), ...)
          box()
          scatterutil.sub(paste(ani[i]," : ",var[j]),
                          csub=csub, possub=possub)
        } 
        
        if (!is.null(dfidxy))
          points(lxy[[ani[i]]][,2], lxy[[ani[i]]][,3],
                 pch=pch, col=colpts, bg=bg, cex=cexpts, ...)
        
      }
    }
  }

