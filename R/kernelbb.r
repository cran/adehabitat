"kernelbb" <-
function(tr, sig1, sig2, grid = 40, same4all=FALSE, byburst=FALSE)
  {
    x <- tr
    if (!inherits(x, "traj"))
      stop("x should be of class \"traj\"")
    sorties <- list()
    gr <- grid
    xy<-x[,c("x","y")]
    sig12<-sig1^2
    sig22<-sig2^2
    h<-c(sig1, sig2)
    names(h)<-c("sig1","sig2")
    fac<-x$burst
    if (!byburst)
      fac<-x$id
    fac<-factor(fac)
    lixy<-split(x,fac)
    
    if (same4all) {
      if (length(as.vector(gr)) == 1) {
        if (!is.numeric(gr)) 
          stop("grid should be an object of class asc or a number")
        xli <- range(xy[, 1])
        yli <- range(xy[, 2])
        xli <- c(xli[1] - 0.3 * abs(xli[2] - xli[1]), xli[2] + 
                 0.3 * abs(xli[2] - xli[1]))
        yli <- c(yli[1] - 0.3 * abs(yli[2] - yli[1]), yli[2] + 
                 0.3 * abs(yli[2] - yli[1]))
        xygg <- data.frame(x = xli, y = yli)
        grid <- ascgen(xygg, nrcol = grid)
        cellsize <- attr(grid, "cellsize")
        lx <- nrow(grid) * cellsize
        ly <- ncol(grid) * cellsize
        ref <- lx
        if (ly > lx) 
          ref <- ly
        xll <- attr(grid, "xll")
        yll <- attr(grid, "yll")
        xll <- xll - lx/2
        yll <- yll - ly/2
        arajlig <- ceiling((lx/2)/cellsize)
        arajcol <- ceiling((ly/2)/cellsize)
        mrajlig <- matrix(0, ncol = ncol(grid), nrow = arajlig)
        grid <- rbind(mrajlig, grid, mrajlig)
        mrajcol <- matrix(0, ncol = arajcol, nrow = nrow(grid))
        grid <- cbind(mrajcol, grid, mrajcol)
        attr(grid, "xll") <- xll
        attr(grid, "yll") <- yll
        attr(grid, "cellsize") <- cellsize
        attr(grid, "type") <- "numeric"
        class(grid) <- "asc"
      }
    }

    for (i in 1:length(lixy)) {
      dft<-lixy[[i]]
      df<-dft[,c("x","y")]
      if (length(as.vector(gr)) == 1) {
        if (!is.numeric(gr)) 
          stop("grid should be an object of class asc or a number")
        if (!same4all) {
          grid <- matrix(0, ncol = gr, nrow = gr)
          rgx <- range(df[, 1])
          rgy <- range(df[, 2])
          lx <- rgx[2] - rgx[1]
          ly <- rgy[2] - rgy[1]
          ref <- lx
          if (ly > lx) 
            ref <- ly
          xll <- rgx[1]
          yll <- rgy[1]
          cellsize <- ref/ncol(grid)
          xll <- xll - lx/2
          yll <- yll - ly/2
          arajlig <- ceiling((lx/2)/cellsize)
          arajcol <- ceiling((ly/2)/cellsize)
          mrajlig <- matrix(0, ncol = ncol(grid), nrow = arajlig)
          grid <- rbind(mrajlig, grid, mrajlig)
          mrajcol <- matrix(0, ncol = arajcol, nrow = nrow(grid))
          grid <- cbind(mrajcol, grid, mrajcol)
          attr(grid, "xll") <- xll
          attr(grid, "yll") <- yll
          attr(grid, "cellsize") <- cellsize
          attr(grid, "type") <- "numeric"
          class(grid) <- "asc"
        }
      }
      
      xyg<-getXYcoords(grid)
      date<-as.double(dft$date)-min(as.double(dft$date))
      toto<-.C("kernelbb", as.double(t(grid)), as.double(xyg$x),
               as.double(xyg$y), as.integer(ncol(grid)),as.integer(nrow(grid)),
               as.integer(nrow(x)), as.double(sig12), as.double (sig22), 
               as.double(df$x), as.double(df$y), as.double(date),
               PACKAGE="adehabitat")
      UD <- matrix(toto[[1]], nrow = nrow(grid), byrow = TRUE)
      UD <- getascattr(grid, UD)
      sorties[[names(lixy)[i]]] <- list(UD = UD, locs = lixy[[i]],
                                        h = h, hmeth = "bb")
    }
    class(sorties) <- c("kbbhrud", "khr")
    return(sorties)
  }

