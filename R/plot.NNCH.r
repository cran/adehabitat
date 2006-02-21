"plot.NNCH" <-
function(x, which = names(x), add.points=TRUE, pch=21,
                    bgpts="white", colpts="black",
                    cex=0.7, add=FALSE, same4all=TRUE, border=NA, ...)
  {
    if (!inherits(x, "NNCH"))
      stop("x should be of class \"NNCH\"")
    x<-x[which]
    if (length(x)>1) {
      opar<-par(mfrow=n2mfrow(length(x)))
      on.exit(par(opar))
    }
    
    if (same4all) {
      xxx<-do.call("rbind", lapply(x, function(x) x$xy))
      rx<-range(xxx[,1])
      ry<-range(xxx[,2])
    }
    
    for (kk in names(x)) {
      if (!same4all) {
        rx<-range(x[[kk]]$xy[,1])
        ry<-range(x[[kk]]$xy[,2])
      }
      if(!add) {
        if (length(x)>1) 
          plot(x[[kk]]$xy, ty="n", asp=1, main=kk,
               xlim=rx, ylim=ry,...)
        if (length(x)==1) 
          plot(x[[kk]]$xy, ty="n", asp=1, xlim=rx,
               ylim=ry,...)
      }
      
      gr<-grey(x[[kk]]$area$levels/100)
      li2<-x[[kk]]$polygons
      for (i in length(li2):1)
        plot(li2[[i]], poly.args=list(col=gr[i], border=border), add=TRUE)
      if (add.points)
        points(x[[kk]]$xy, pch=pch, bg=bgpts, col=colpts, cex=cex)
    }
  }

