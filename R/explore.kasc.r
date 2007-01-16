explore.kasc <- function (ka, coltxt="blue",
                          hscale = 1, vscale = 1, ...)
{
    if (!inherits(ka,"kasc"))
      stop("ka should be of class kasc")
    nn <<- NULL
    whi <<- 1 ## current graph

    ## function replot: to replot the content of the object
    replot <- function() {
        if (lim) {
            xlim <- range(getXYcoords(ka)$x)
            ylim <- range(getXYcoords(ka)$y)
        }
        nn <<- n2mfrow(length(ka))
        split.screen(c(1,2))
        split.screen(nn, screen=2)
        on.exit(close.screen(all.screens=TRUE))
        na <- names(ka)
        sapply(1:length(na), function(j) {
            i <- na[j]
            screen(j+2)
            opar <- par(mar=c(0,0,2,0))
            image(getkasc(ka,i), main=i, axes=F, xlim=xlim, ylim=ylim)
            if (ajoupo) {
                text(a5[1], a5[2], ka[ia5,i], col=coltxt, font=2, cex=1.15)
            }
            box()
            par(opar)
        })
        screen(1)
        opar <- par(mar=c(0,0,2,0))
        image(getkasc(ka,whi), main=na[whi], axes=FALSE,
              xlim=xlim, ylim=ylim)
        cusr <<- par("usr")
        cplt <<- par("plt")
        if (ajouli)
            lines(c(a1[1], a2[1]), c(a1[2], a2[2]), lwd = 2,
                  col = "red")
        box()
        par(opar)
    }


    N <- length(ka)
    D <- 0
    xlim <- range(getXYcoords(ka)$x)
    ylim <- range(getXYcoords(ka)$y)
    lim <- TRUE
    ajouli <- FALSE
    ajoupo <- FALSE
    opt <- options(warn = -1)
    on.exit(options(opt))

    if (!require(tkrplot))
        stop("'tkrplot' package needed\n")

    help.txt <- paste("\n-------- TO OBTAIN THIS HELP, TYPE 'h' ------------------",
                      "z/o            -- Zoom in/Out",
                      "Right-Click    -- Identify value",
                      "Left-Click     -- Select variable /
                  Measure the distance between two points",
                      "q              -- Quit",
                      "---------------------------------------------------------",
                      "\n", sep = "\n")

    tt <- tktoplevel()
    tkwm.title(tt, "Exploration of kasc maps")
    img <- tkrplot(tt, replot, hscale = hscale, vscale = vscale)
    txt <- tktext(tt, bg = "white", font = "courier 10")
    scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt, ...))
    tkconfigure(txt, yscrollcommand = function(...) tkset(scr,
                     ...))
    tkpack(img, side = "top")
    tkpack(txt, side = "left", fill = "both", expand = TRUE)
    tkpack(scr, side = "right", fill = "y")
    iw <- as.numeric(tcl("image", "width", tkcget(img, "-image")))
    ih <- as.numeric(tcl("image", "height", tkcget(img, "-image")))

    showz <- function() tkrreplot(img)
    type <- function(s) {
        tkinsert(txt, "end", s)
        tksee(txt, "end")
    }
    type(help.txt)
    cc <- function(x, y) {
        x <- (as.real(x) - 1)/iw
        y <- 1 - (as.real(y) - 1)/ih
        px <- (x - cplt[1])/(cplt[2] - cplt[1])
        py <- (y - cplt[3])/(cplt[4] - cplt[3])
        ux <- px * (cusr[2] - cusr[1]) + cusr[1]
        uy <- py * (cusr[4] - cusr[3]) + cusr[3]
        c(ux, uy)
    }
    cc2 <- function(x, y) {
        x <- (as.real(x) - 1)/iw
        y <- 1 - (as.real(y) - 1)/ih
        px <- (x - cplt[1])/(cplt[2] - cplt[1])
        py <- (y - cplt[3])/(cplt[4] - cplt[3])
        c(px, py)
    }

    mm.t <- function(x, y) {
        veri <- cc2(x,y)
        if (veri[1]<0.5) {
            i <- D
            if (i == 0) {
                ux <- (2*veri[1]) * (cusr[2] - cusr[1]) + cusr[1]
                uy <- veri[2]* (cusr[4] - cusr[3]) + cusr[3]
                a1 <<- c(ux, uy)
                D <<- 1
            }
            if (i == 1) {
                ux <- (2*veri[1]) * (cusr[2] - cusr[1]) + cusr[1]
                uy <- veri[2]* (cusr[4] - cusr[3]) + cusr[3]
                a2 <<- c(ux, uy)
                D <<- 0
                di <- sqrt(sum((a2 - a1)^2))
                type(paste("distance:", di, "\n"))
                ajouli <<- TRUE
                showz()
                ajouli <<- FALSE
            }
        } else {
            veri[1] <- 2*(veri[1]-0.5)
            rc <- veri
            rc[1] <- as.numeric(cut(veri[2],seq(0,1,length=nn[1]+1)))
            rc[2] <- as.numeric(cut(veri[1],seq(0,1,length=nn[2]+1)))
            rc[1] <- nn[1] - rc[1] +1
            whi <<- nn[2]*(rc[1]-1) + rc[2]
            showz()
        }
        return()
    }
    mm.t2 <- function(x, y) {
        veri <- cc2(x,y)
        ux <- (2*veri[1]) * (cusr[2] - cusr[1]) + cusr[1]
        uy <- veri[2]* (cusr[4] - cusr[3]) + cusr[3]
        a3 <<- c(ux, uy)
        xyc <- getXYcoords(ka)
        yc <- rep(xyc$y, each=length(xyc$x))
        xc <- rep(xyc$x, length(xyc$y))
        iy <- rep(1:length(xyc$y), each=length(xyc$x))
        ix <- rep(1:length(xyc$x), length(xyc$y))

        di <- sqrt((xc - a3[1])^2 + (yc - a3[2])^2)
        a5 <<- unlist(cbind(xc,yc)[which.min(di),])
        ia5 <<- which.min(di)
        ajoupo <<- TRUE
        showz()
        ajoupo <<- FALSE
        type(paste("Index:", ia5,"\n"))
        return()
    }
    mm.mouset <- function(x, y) {
        veri <- cc2(x,y)
        if (veri[1]<0.5) {
        ux <- (2*veri[1]) * (cusr[2] - cusr[1]) + cusr[1]
        uy <- veri[2]* (cusr[4] - cusr[3]) + cusr[3]
        a8 <<- c(ux, uy)
    }
        return()
    }
    kb <- function(A) {
        key <- tolower(A)
        if (key == "q") {
            tkdestroy(tt)
            return("OK - Finished")
        }
        if (key == "z") {
            tmppx <- (cusr[1:2] - cusr[1])
            xlim <<- c(a8[1] - tmppx[2]/4, (a8[1] + tmppx[2]/4))
            tmppy <<- (cusr[3:4] - cusr[3])
            ylim <<- c(a8[2] - tmppy[2]/4, (a8[2] + tmppy[2]/4))
            lim <<- FALSE
            showz()
        }
        if (key == "o") {
            lim <<- TRUE
            showz()
        }
    }
    showz()
    tkbind(tt, "<Key>", kb)
    tkbind(img, "<Button-1>", mm.t)
    tkbind(img, "<Motion>", mm.mouset)
    tkbind(img, "<Button-3>", mm.t2)
    tkwait.window(tt)

}
