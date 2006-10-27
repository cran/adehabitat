"kernelUD" <-
function(xy, id=NULL, h="href", grid=40, same4all=FALSE,
         hlim=c(0.1, 1.5), kern = c("bivnorm", "epa"))
  {
      kern <- match.arg(kern)
    if (ncol(xy)!=2)
      stop("xy should have 2 columns")
    if ((!is.null(id))&(length(id)!=nrow(xy)))
      stop("id should have the same length as xy")
    if ((!is.numeric(h))&(h!="href")&(h!="LSCV"))
      stop("h should be numeric or equal to either \"href\" or \"LSCV\"")
    if ((h == "LSCV")&(kern == "epa"))
      stop("LSCV is not implemented with an Epanechnikov kernel")
    if (is.null(id))
      id<-rep(1, nrow(xy))
    id<-factor(id)
      if (min(table(id))<5)
    stop("At least 5 relocations are required to fit an home range")


    ## split de xy
    lixy<-split(xy, id)
    sorties<-list()
    typh<-h
    htmp<-h
    gr<-grid

    ##
    if (same4all) {
      if (length(as.vector(gr))==1) {
        if (!is.numeric(gr))
          stop("grid should be an object of class asc or a number")
        xli<-range(xy[,1])
        yli<-range(xy[,2])
        xli<-c(xli[1]-0.3*abs(xli[2]-xli[1]),xli[2]+0.3*abs(xli[2]-xli[1]))
        yli<-c(yli[1]-0.3*abs(yli[2]-yli[1]),yli[2]+0.3*abs(yli[2]-yli[1]))
        xygg<-data.frame(x=xli, y=yli)
        grid<-ascgen(xygg, nrcol=grid)
        cellsize<-attr(grid, "cellsize")
        lx<-nrow(grid)*cellsize
        ly<-ncol(grid)*cellsize
        ref<-lx
        if (ly>lx)
          ref<-ly
        xll<-attr(grid, "xll")
        yll<-attr(grid, "yll")

        ## On rajoute des colonnes et des lignes
        xll<-xll-lx/2
        yll<-yll-ly/2
        arajlig<-ceiling((lx/2)/cellsize)
        arajcol<-ceiling((ly/2)/cellsize)
        mrajlig<-matrix(0, ncol=ncol(grid), nrow=arajlig)
        grid<-rbind(mrajlig, grid, mrajlig)
        mrajcol<-matrix(0, ncol=arajcol, nrow=nrow(grid))
        grid<-cbind(mrajcol, grid, mrajcol)

        ## rajout des attributs
        attr(grid, "xll")<-xll
        attr(grid, "yll")<-yll
        attr(grid, "cellsize")<-cellsize
        attr(grid, "type")<-"numeric"
        class(grid)<-"asc"
      }
    }

    ## Boucle estimation UD pour chaque ani
    for (i in 1:nlevels(id)) {
      df<-lixy[[i]]

      ## 1. Calcul de h
      varx<-var(df[,1])
      vary<-var(df[,2])
      sdxy<-sqrt(0.5*(varx+vary))
      n<-nrow(df)
      ex<-(-1/6)
      href<-sdxy*(n^ex)
      if (kern=="epa")
        href <- href*1.77

      if (h=="href") {
        htmp<-href
      }
      if (h=="LSCV") {
        hvec<-seq(hlim[1]*href, hlim[2]*href, length=100)
        CV<-.C("CVmise", as.integer(nrow(df)), as.double(df[,1]),
               as.double(df[,2]),
               as.double(hvec), double(length(hvec)),
               as.integer(length(hvec)), PACKAGE="adehabitat")[[5]]
        htmp<-hvec[CV==min(CV)]
        if ((CV[CV==min(CV)]==CV[1])|(CV[CV==min(CV)]==CV[length(CV)]))
          warning("The algorithm did not converge \nwithin the specified range of hlim: try to increase it")
      }


      ## 3. Construction de la grille
      if (length(as.vector(gr))==1) {
        if (!is.numeric(gr))
          stop("grid should be an object of class asc or a number")

        if (!same4all) {
          grid<-matrix(0, ncol=gr, nrow=gr)
          rgx<-range(df[,1])
          rgy<-range(df[,2])
          lx<-rgx[2]-rgx[1]
          ly<-rgy[2]-rgy[1]
          ref<-lx
          if (ly>lx)
            ref<-ly

          xll<-rgx[1]
          yll<-rgy[1]
          cellsize<-ref/ncol(grid)

          ## On rajoute des colonnes et des lignes
          xll<-xll-lx/2
          yll<-yll-ly/2
          arajlig<-ceiling((lx/2)/cellsize)
          arajcol<-ceiling((ly/2)/cellsize)
          mrajlig<-matrix(0, ncol=ncol(grid), nrow=arajlig)
          grid<-rbind(mrajlig, grid, mrajlig)
          mrajcol<-matrix(0, ncol=arajcol, nrow=nrow(grid))
          grid<-cbind(mrajcol, grid, mrajcol)

          ## rajout des attributs
          attr(grid, "xll")<-xll
          attr(grid, "yll")<-yll
          attr(grid, "cellsize")<-cellsize
          attr(grid, "type")<-"numeric"
          class(grid)<-"asc"
        }
      }
      grille<-grid
      xylo<-getXYcoords(grid)
      xg<-xylo$x
      yg<-xylo$y

      if (kern=="bivnorm") {
        toto<-.C("kernelhr", double(nrow(grid)*ncol(grid)),as.double(xg),
                 as.double(yg),
                 as.integer(ncol(grid)), as.integer(nrow(grid)),
                 as.integer(nrow(df)), as.double(htmp),
                 as.double(df[,1]), as.double(df[,2]), PACKAGE="adehabitat")
      }
      if (kern=="epa") {
        toto<-.C("kernepan", double(nrow(grid)*ncol(grid)),as.double(xg),
                 as.double(yg),
                 as.integer(ncol(grid)), as.integer(nrow(grid)),
                 as.integer(nrow(df)), as.double(htmp),
                 as.double(df[,1]), as.double(df[,2]), PACKAGE="adehabitat")
      }
      UD<-matrix(toto[[1]], nrow=nrow(grid), byrow=TRUE)
      UD<-getascattr(grid, UD)
      if (typh=="LSCV") {
        CV<-data.frame(h=hvec, CV=CV)
        convergence<-min(CV[,2])!=CV[1,2]
        htmp<-list(CV=CV, convergence=convergence, h=htmp)
      }
      sorties[[names(lixy)[i]]]<-list(UD=UD, h=htmp, locs=df, hmeth=typh)
    }
    class(sorties)<-c("khrud", "khr")
    return(sorties)
  }

