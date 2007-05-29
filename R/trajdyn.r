"trajdyn" <- function(x, burst = attr(x[[1]],"burst"), hscale=1,
                      vscale=1, recycle = TRUE,
                      display = c("guess", "windows", "tk"), ...)
{
    if (!inherits(x, "ltraj"))
        stop("x should be of class 'ltraj'")
    typeII <- attr(x,"typeII")

    ## supprimer les NA
    x <- lapply(x, function(i) {
        jj <- i[!is.na(i$x),]
        attr(jj, "id") <- attr(i,"id")
        attr(jj, "burst") <- attr(i,"burst")
        return(jj)
    })
    class(x) <- c("ltraj","list")
    attr(x, "typeII") <- typeII
    attr(x, "regular") <- is.regular(x)
    u <- x
    x<- v <- x[burst = burst]
    ajouli <- FALSE
    ajoupo <- FALSE
    ajoubu <- FALSE
    addpoints <- TRUE
    addlines <- TRUE
    lim <- TRUE
    buadd <- burst
    K=1
    N=nrow(x[[1]])
    cusr <- cplt <- rep(0 + NA, 4)

    opt <- options(warn=-1)
    on.exit(options(opt))
    dsp <- substring(match.arg(display), 1, 1)
    if (dsp == "g")
        dsp <- switch(getOption("device"), windows = "w", "t")
    if (dsp == "t" && !require(tkrplot))
        stop("'tkrplot' package needed\n")


### fonction replot de base
    replot <- function() {
        opar <- par(mar=c(0,0,0,0), bg="white")
        attr(x[[1]],"id") <- " "
        if (lim) {
            xlim <- range(x[[1]]$x)
            ylim <- range(x[[1]]$y)
        }
        plot(x, id = attr(x[[1]],"id"), addlines=F, addp=F, final=FALSE,
             xlim = xlim, ylim = ylim, ...)
        cusr <<- par("usr")
        cplt <<- par("plt")
        scatterutil.sub(as.character(x[[1]]$date[K]), 1, "topleft")
        if (ajoubu) {
            lapply(u[burst=buadd], function(zz) {
                if (addpoints)
                    points(zz[,c("x","y")], pch=16, col="grey")
                if (addlines)
                    lines(zz[,c("x","y")], pch=16, col="grey")})
        }
        if (addpoints)
            points(x[[1]][1:K,c("x","y")], pch=16)
        if (addlines)
            if (K>1)
                lines(x[[1]][1:K,c("x","y")], lwd=2)
        if (ajouli)
            lines(c(a1[1], a2[1]),c(a1[2], a2[2]), lwd=2, col="red")
        if (ajoupo)
            points(a5[1], a5[2], pch=16, col="red", cex=1.7)
        iti <- unlist(x[[1]][K,c("x","y")])
      points(iti[1],iti[2], col="blue", pch=16, cex=1.4)
      par(opar)
    }


    help.txt <- paste("\n-------- TO OBTAIN THIS HELP, TYPE 'h' ------------------",
                      "n/p            -- Next/Previous relocation",
                      "a              -- show all relocations",
                      "g              -- Go to...",
                      "0-9            -- show a given part of the path",
                      "b              -- change Burst",
                      "i              -- add/remove other bursts on the graph",
                      "z/o            -- Zoom in/Out",
                      "Left-Click     -- measure the distance between two points",
                      "Right-Click    -- identify a relocation",
                      "r/l            -- add or remove points/Lines",
                      "q              -- Quit",
                      "---------------------------------------------------------",
                      "\n", sep = "\n")
    D <-0
    a1 <- 0
    a2 <- 0

    if (dsp == "t") {
      tt <- tktoplevel()
      tkwm.title(tt, "Exploration of Animal Movements")
      img <- tkrplot(tt, replot, hscale = hscale, vscale = vscale)
      txt <- tktext(tt, bg = "white", font = "courier 10")
      scr <- tkscrollbar(tt, repeatinterval = 5,
                         command = function(...) tkyview(txt, ...))
      tkconfigure(txt, yscrollcommand = function(...) tkset(scr, ...))
      tkpack(img, side = "top")
      tkpack(txt, side = "left", fill = "both", expand = TRUE)
      tkpack(scr, side = "right", fill = "y")

      iw <- as.numeric(tcl("image", "width", tkcget(img, "-image")))
      ih <- as.numeric(tcl("image", "height", tkcget(img, "-image")))
    }

    showz <- function() switch(dsp, w = replot(),
                               t = {tkrreplot(img)})
    type <- function(s) switch(dsp, w = cat(s), t = {
      tkinsert(txt, "end", s)
      tksee(txt, "end")
    })
    type(help.txt)

    cc <- function(x, y) {
      if (dsp == "t") {
        x <- (as.real(x) - 1)/iw
        y <- 1 - (as.real(y) - 1)/ih
      }
      px <- (x - cplt[1])/(cplt[2] - cplt[1])
      py <- (y - cplt[3])/(cplt[4] - cplt[3])
      ux <- px * (cusr[2] - cusr[1]) + cusr[1]
      uy <- py * (cusr[4] - cusr[3]) + cusr[3]
      c(ux,uy)
    }

    mm.w <- function(buttons, x, y) {
      if (buttons == 0) {
        i<-D
        if (i == 0) {
          a1 <<- cc(x,y)
          D <<- 1
        }
        if (i == 1) {
          a2 <<- cc(x,y)
          D <<- 0
          di <- sqrt(sum((a2-a1)^2))
          cat(paste("distance:",round(di,6),"\n"))
          lines(c(a1[1],a2[1]),c(a1[2],a2[2]), lwd=2, col="red")
        }
        return()
      }
      if (buttons == 2) {
        w <- v[[1]][1:K,]
        a3 <<- cc(x,y)
        di <- sqrt((w$x-a3[1])^2 + (w$y-a3[2])^2)
        print(w[which.min(di),])
        cat("\n")
        points(w[which.min(di),c("x","y")], pch=16, col="red", cex=1.7)
        return()
      }
    }
    mm.w <- function(buttons, x, y) {
      if (buttons == 0) {
        i<-D
        if (i == 0) {
          a1 <<- cc(x,y)
          D <<- 1
        }
        if (i == 1) {
          a2 <<- cc(x,y)
          D <<- 0
          di <- sqrt(sum((a2-a1)^2))
          cat(paste("distance:",round(di,6),"\n"))
          lines(c(a1[1],a2[1]),c(a1[2],a2[2]), lwd=2, col="red")
        }
        return()
      }
      if (buttons == 2) {
        w <- v[[1]][1:K,]
        a3 <<- cc(x,y)
        di <- sqrt((w$x-a3[1])^2 + (w$y-a3[2])^2)
        print(w[which.min(di),])
        cat("\n")
        points(w[which.min(di),c("x","y")], pch=16, col="red", cex=1.7)
        return()
      }
    }
    mm.t <- function(x, y) {
      i<-D
      if (i == 0) {
        a1 <<- cc(x,y)
        D <<- 1
      }
      if (i == 1) {
        a2 <<- cc(x,y)
        D <<- 0
        di <- sqrt(sum((a2-a1)^2))
        type(paste("distance:",di,"\n"))
        ajouli <<- TRUE
        showz()
        ajouli <<- FALSE
      }
      return()
    }

    mm.t2 <- function(x, y) {
      w <- v[[1]][1:K,]
      a3 <<- cc(x,y)
      di <- sqrt((w$x-a3[1])^2 + (w$y-a3[2])^2)
      a5 <<- unlist(w[which.min(di),c("x","y")])
      ajoupo <<- TRUE
      showz()
      ajoupo <<- FALSE
      tmp <- w[which.min(di),]
      se <-unlist(lapply((max(nchar(names(tmp))+
                              nchar(sapply(tmp,as.character))+1) -
                          nchar(names(tmp))-nchar(sapply(tmp, as.character))),
                         function(zz) paste(rep(" ",zz),
                                            collapse="")))
      so<-unlist(lapply(1:length(tmp),
                        function(i) paste(paste(names(tmp)[i],
                                                as.character(tmp[1,i]),
                                                sep = se[i]),"\n")))
      type(paste("Relocation",row.names(w)[which.min(di)],":\n"))
      sapply(so,type)
      type("\n")
      return()
    }


    mm.mouse <- function(buttons, x, y) {
          a8 <<- cc(x,y)
          return()
    }

    mm.mouset <- function(x, y) {
          a8 <<- cc(x,y)
          return()
    }



    kb <- function(A) {
      key <- tolower(A)
      if (key == "q") {
        if (dsp=="t")
          tkdestroy(tt)
        return("OK - Finished")
      }
      if (key %in% c(0:9)) {
        if (key > 0)
          K <<- round(seq(1,N,length=11))[as.numeric(key)+1]
        if (key == 0)
          K <<- 1
        showz()
      }
      if (key == "z") {
          tmppx <<- (cusr[1:2]-cusr[1])/2
          xlim <<-  c((a8[1] - (tmppx[2] - tmppx[1])/2),
                      (a8[1] + (tmppx[2] - tmppx[1])/2))

          tmppy <<- (cusr[3:4]-cusr[3])/2
          ylim <<-  c((a8[2] - (tmppy[2] - tmppy[1])/2),
                      (a8[2] + (tmppy[2] - tmppy[1])/2))

          lim <<- FALSE
          showz()
      }
      if (key == "o") {
          lim <<-TRUE
          showz()
      }
      if (key == "n") {
        if (K<=N)
          K <<- K+1
        if (K>N) {
          if (recycle) K <<- 1
          if (!recycle) {
            K <<- N
            cat("End of burst !\n")
          }
        }
        showz()
      }
      if (key == "l") {
        addlines <<- !addlines
        showz()
      }
      if (key == "g") {
        if (dsp == "w") {
          recom <- TRUE
          while (recom) {
            rr <- readline("Enter a relocation number: ")
            recom <- FALSE
            if (!(rr%in%row.names(x[[1]]))) {
              cat("invalid number\n")
              recom <- TRUE
            }
          }
          K <<- which(row.names(x[[1]])==as.numeric(rr))
          showz()
        }
        if (dsp == "t") {
          lv <- tclVar(row.names(x[[1]])[1])
          tu <- tktoplevel(tt, width=500, height=50)
          tkwm.title(tu, "Enter a relocation number")
          tkwm.resizable(tu, 0, 0)
          en <- tkentry(tu, textvariable=lv, width=50)
          submit.but <- tkbutton(tu, text="    OK     ",
                                 command=function() {
                                   rr <- tclvalue(lv)
                                   if (!(rr%in%row.names(x[[1]]))) {
                                     tkmessageBox(message="invalid number",
                                                  type="ok")
                                   } else {
                                     K <<- which(row.names(x[[1]])==as.numeric(rr))
                                     showz()
                                     tkdestroy(tu)}})
          tkpack(en, side = "top", fill = "both")
          tkpack(submit.but, side = "bottom")
          tkwait.window(tu)
        }
      }
      if (key == "r") {
        addpoints <<- !addpoints
        showz()
      }
      if (key == "b") {
        K <<- 1
        if (dsp == "w") {
          hoho <- select.list(unlist(lapply(u, function(y) attr(y, "burst"))))
          type(paste("Choice of the burst:", hoho,"\n\n"))
          x <<- v <<- u[burst=hoho]
          N <<- nrow(x[[1]])
          showz()
        }
        if (dsp == "t") {
          lv <- tclVar(unlist(lapply(u, function(y) attr(y, "burst"))))
          bubu <- unlist(lapply(u, function(y) attr(y, "burst")))
          tu <- tktoplevel(tt)
          tkwm.title(tu, "Choose a burst of relocations")
          tkwm.resizable(tu, 0, 0)
          tfr <- tkframe(tu)
          tli <- tklistbox(tfr, bg = "white", font = "courier 12",
                           listvariable = lv)
          scr2 <- tkscrollbar(tfr, repeatinterval = 5,
                              command = function(...) tkyview(tli, ...))
          tkconfigure(tli, yscrollcommand = function(...) tkset(scr2, ...))
          submit.but <- tkbutton(tu, text="    OK     ",
                                 command=function() {
                                   hoho <<- ifelse(nchar(tclvalue(tkcurselection(tli)))==0, 1,
                                                   as.numeric(tclvalue(tkcurselection(tli)))+1)
                                   type(paste("Choice of the burst:", bubu[hoho],"\n\n"))
                                   tkdestroy(tu)})
          tkpack(tli, side = "left", fill = "both", expand = TRUE)
          tkpack(scr2, side = "right", fill = "y")
          tkpack(tfr, side = "right", fill = "y")
          tkpack(submit.but, side = "bottom")
          tkwait.window(tu)
          x <<- v <<- u[burst=bubu[hoho]]
          N <<- nrow(x[[1]])
          showz()
        }
      }
      if (key == "i") {
        if (ajoubu) {
          ajoubu <<- FALSE
          showz()
        } else {
          if (dsp == "w") {
            buadd <<- select.list(unlist(lapply(u,
                                                function(y) attr(y, "burst"))),
                                  multiple=TRUE)
            if (length(buadd>0)) {
              type(paste("show bursts:", paste(buadd, collapse=" "),"\n\n"))
              ajoubu <<- TRUE
              showz()
            }
          }
          if (dsp == "t") {
            lv <- tclVar(unlist(lapply(u, function(y) attr(y, "burst"))))
            bubu <- unlist(lapply(u, function(y) attr(y, "burst")))
            tu <- tktoplevel(tt)
            tkwm.title(tu, "Choose one or several bursts")
            tkwm.resizable(tu, 0, 0)
            tfr <- tkframe(tu)
            tli <- tklistbox(tfr, bg = "white", font = "courier 12",
                             listvariable = lv, selectmode="multiple")
            scr2 <- tkscrollbar(tfr, repeatinterval = 5,
                                command = function(...) tkyview(tli, ...))
            tkconfigure(tli, yscrollcommand = function(...) tkset(scr2, ...))
            submit.but <- tkbutton(tu, text="    OK     ",
                                   command=function() {
                                     argg <- ifelse(nchar(tclvalue(tkcurselection(tli)))==0,
                                                    1,0)
                                     if (argg==0) {
                                       ajoubu <<- TRUE
                                       buadd <<- bubu[as.numeric(unlist(strsplit(tclvalue(tkcurselection(tli)), " ")))+1]
                                       type(paste("show bursts:", paste(buadd, collapse=" "),"\n\n"))
                                       showz()
                                       tkdestroy(tu)}})
            tkpack(tli, side = "left", fill = "both", expand = TRUE)
            tkpack(scr2, side = "right", fill = "y")
            tkpack(tfr, side = "right", fill = "y")
            tkpack(submit.but, side = "bottom")
            tkwait.window(tu)
            x <<- v <<- u[burst=bubu[hoho]]
            N <<- nrow(x[[1]])
            showz()
          }
        }
      }
      if (key == "p") {
        if (K>1)
          K <<- K-1
        if (K==1) {
          if (recycle)
            K <<- N
          if (!recycle) {
            K <<- 1
            cat("Beginning of burst!\n")
          }
        }
        showz()
      }
      if (key == "a") {
        K <<- N
        showz()
      }
      if (key == "h")
        type(help.txt)
      return()
    }

    showz()
    toto <- switch(dsp, w = getGraphicsEvent("", onKeybd = kb, onMouseDown = mm.w,
                                             onMouseMove = mm.mouse),
                   t ={tkbind(tt, "<Key>", kb)
                       tkbind(img, "<Button-1>", mm.t)
                       tkbind(img, "<Motion>", mm.mouset)
                       tkbind(img, "<Button-3>", mm.t2)
                       tkwait.window(tt)})
  }

