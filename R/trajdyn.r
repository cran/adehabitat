"trajdyn" <- function(x, burst = attr(x[[1]],"burst"), hscale=1,
                      vscale=1, recycle = TRUE,
                      display = c("guess", "windows", "tk"), ...)
{
    if (!inherits(x, "ltraj"))
        stop("x should be of class 'ltraj'")
    e1 <- new.env(parent = baseenv())
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
    assign("x", x[burst = burst], envir=e1)
    assign("v", x[burst = burst], envir=e1)
    assign("ajouli", FALSE, envir=e1)
    assign("ajoupo", FALSE, envir=e1)
    assign("ajoubu", FALSE, envir=e1)
    assign("addpoints", TRUE, envir=e1)
    assign("addlines", TRUE, envir=e1)
    assign("lim", TRUE, envir=e1)
    assign("buadd", burst, envir=e1)
    assign("K",1, envir=e1)
    assign("N",nrow(get("x", envir=e1)[[1]]), envir=e1)
    assign("cusr", rep(0 + NA, 4), envir=e1)
    assign("cplt", rep(0 + NA, 4), envir=e1)
    opt <- options(warn=-1)
    on.exit(options(opt))
    dsp <- substring(match.arg(display), 1, 1)
    if (dsp == "g")
        dsp <- switch(.Platform$OS.type, windows = "w", "t")
    if (dsp == "t" && !require(tkrplot))
        stop("'tkrplot' package needed\n")


### fonction replot de base
    replot <- function() {
        opar <- par(mar=c(0,0,0,0), bg="white")
        tmptmp <- get("x", envir=e1)
        attr(tmptmp[[1]], "id") <- " "
        assign("x", tmptmp, envir=e1)
        if (get("lim", envir=e1)) {
            assign("xlim", range(get("x", envir=e1)[[1]]$x), envir=e1)
            assign("ylim", range(get("x", envir=e1)[[1]]$y), envir=e1)
        }
        plot(get("x", envir=e1), id = attr(get("x", envir=e1)[[1]],"id"), addlines=FALSE, addp=FALSE, final=FALSE,
             xlim = get("xlim", envir=e1), ylim = get("ylim", envir=e1), ...)
        assign("cusr", par("usr"), envir=e1)
        assign("cplt", par("plt"), envir=e1)
        scatterutil.sub(as.character(get("x", envir=e1)[[1]]$date[get("K", envir=e1)]), 1, "topleft")
        if (get("ajoubu", envir=e1)) {
            lapply(u[burst=get("buadd", envir=e1)], function(zz) {
                if (get("addpoints", envir=e1))
                    points(zz[,c("x","y")], pch=16, col="grey")
                if (get("addlines", envir=e1))
                    lines(zz[,c("x","y")], pch=16, col="grey")})
        }
        if (get("addpoints", envir=e1))
            points(get("x", envir=e1)[[1]][1:get("K", envir=e1),c("x","y")], pch=16)
        if (get("addlines", envir=e1))
            if (get("K", envir=e1)>1)
                lines(get("x", envir=e1)[[1]][1:get("K", envir=e1),c("x","y")], lwd=2)
        if (get("ajouli", envir=e1))
            lines(c(get("a1", envir=e1)[1], get("a2", envir=e1)[1]),c(get("a1", envir=e1)[2], get("a2", envir=e1)[2]), lwd=2, col="red")
        if (get("ajoupo", envir=e1))
            points(get("a5", envir=e1)[1], get("a5", envir=e1)[2], pch=16, col="red", cex=1.7)
        iti <- unlist(get("x", envir=e1)[[1]][get("K", envir=e1),c("x","y")])
      points(iti[1],iti[2], col="blue", pch=16, cex=1.4)
      par(opar)
    }


    help.txt <- paste("\n-------- to obtain this help, type 'h' ------------------",
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
    assign("D",0, envir=e1)
    assign("a1", 0, envir=e1)
    assign("a2", 0, envir=e1)

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
        x <- (as.double(x) - 1)/iw
        y <- 1 - (as.double(y) - 1)/ih
      }
      px <- (x - get("cplt", envir=e1)[1])/(get("cplt", envir=e1)[2] - get("cplt", envir=e1)[1])
      py <- (y - get("cplt", envir=e1)[3])/(get("cplt", envir=e1)[4] - get("cplt", envir=e1)[3])
      ux <- px * (get("cusr", envir=e1)[2] - get("cusr", envir=e1)[1]) + get("cusr", envir=e1)[1]
      uy <- py * (get("cusr", envir=e1)[4] - get("cusr", envir=e1)[3]) + get("cusr", envir=e1)[3]
      c(ux,uy)
    }

    mm.w <- function(buttons, x, y) {
      if (buttons == 0) {
        i<-get("D", envir=e1)
        if (i == 0) {
          assign("a1", cc(x,y), envir=e1)
          assign("D", 1, envir=e1)
        }
        if (i == 1) {
          assign("a2", cc(x,y), envir=e1)
          assign("D", 0, envir=e1)
          di <- sqrt(sum((get("a2", envir=e1)-get("a1", envir=e1))^2))
          cat(paste("distance:",round(di,6),"\n"))
          lines(c(get("a1", envir=e1)[1],get("a2", envir=e1)[1]),c(get("a1", envir=e1)[2],get("a2", envir=e1)[2]), lwd=2, col="red")
        }
        return()
      }
      if (buttons == 2) {
        w <- get("v",envir=e1)[[1]][1:get("K", envir=e1),]
        assign("a3", cc(x,y), envir=e1)
        di <- sqrt((w$x-get("a3", envir=e1)[1])^2 + (w$y-get("a3", envir=e1)[2])^2)
        print(w[which.min(di),])
        cat("\n")
        points(w[which.min(di),c("x","y")], pch=16, col="red", cex=1.7)
        return()
      }
    }
    mm.w <- function(buttons, x, y) {
      if (buttons == 0) {
        i<-get("D", envir=e1)
        if (i == 0) {
          assign("a1",  cc(x,y), envir=e1)
          assign("D", 1, envir=e1)
        }
        if (i == 1) {
          assign("a2", cc(x,y), envir=e1)
          assign("D", 0, envir=e1)
          di <- sqrt(sum((get("a2", envir=e1)-get("a1", envir=e1))^2))
          cat(paste("distance:",round(di,6),"\n"))
          lines(c(get("a1", envir=e1)[1],get("a2", envir=e1)[1]),c(get("a1", envir=e1)[2],get("a2", envir=e1)[2]), lwd=2, col="red")
        }
        return()
      }
      if (buttons == 2) {
        w <- get("v",envir=e1)[[1]][1:get("K", envir=e1),]
        assign("a3", cc(x,y), envir=e1)
        di <- sqrt((w$x-get("a3", envir=e1)[1])^2 + (w$y-get("a3", envir=e1)[2])^2)
        print(w[which.min(di),])
        cat("\n")
        points(w[which.min(di),c("x","y")], pch=16, col="red", cex=1.7)
        return()
      }
    }
    mm.t <- function(x, y) {
      i<-get("D", envir=e1)
      if (i == 0) {
        assign("a1", cc(x,y), envir=e1)
        assign("D", 1, envir=e1)
      }
      if (i == 1) {
        assign("a2", cc(x,y), envir=e1)
        assign("D", 0, envir=e1)
        di <- sqrt(sum((get("a2", envir=e1)-get("a1", envir=e1))^2))
        type(paste("distance:",di,"\n"))
        assign("ajouli", TRUE, envir=e1)
        showz()
        assign("ajouli", FALSE, envir=e1)
      }
      return()
    }

    mm.t2 <- function(x, y) {
      w <- get("v",envir=e1)[[1]][1:get("K", envir=e1),]
      assign("a3", cc(x,y), envir=e1)
      di <- sqrt((w$x-get("a3", envir=e1)[1])^2 + (w$y-get("a3", envir=e1)[2])^2)
      assign("a5", unlist(w[which.min(di),c("x","y")]), envir=e1)
      assign("ajoupo", TRUE, envir=e1)
      showz()
      assign("ajoupo", FALSE, envir=e1)
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
          assign("a8", cc(x,y), envir=e1)
          return()
    }

    mm.mouset <- function(x, y) {
          assign("a8", cc(x,y), envir=e1)
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
          assign("K", round(seq(1,get("N", envir=e1),length=11))[as.numeric(key)+1], envir=e1)
        if (key == 0)
          assign("K", 1, envir=e1)
        showz()
      }
      if (key == "z") {
          assign("tmppx", (get("cusr", envir=e1)[1:2]-
                           get("cusr", envir=e1)[1])/2, envir=e1)

          assign("xlim",  c((get("a8", envir=e1)[1] -
                             (get("tmppx", envir=e1)[2] -
                              get("tmppx", envir=e1)[1])/2),
                            (get("a8", envir=e1)[1] +
                             (get("tmppx", envir=e1)[2] -
                              get("tmppx", envir=e1)[1])/2)), envir=e1)

          assign("tmppy", (get("cusr", envir=e1)[3:4]-get("cusr", envir=e1)[3])/2, envir=e1)
          assign("ylim", c((get("a8", envir=e1)[2] - (get("tmppy",envir=e1)[2] - get("tmppy",envir=e1)[1])/2),
                           (get("a8", envir=e1)[2] + (get("tmppy",envir=e1)[2] - get("tmppy",envir=e1)[1])/2)), envir=e1)

          assign("lim", FALSE, envir=e1)
          showz()
      }
      if (key == "o") {
          assign("lim", TRUE, envir=e1)
          showz()
      }
      if (key == "n") {
          if (get("K", envir=e1)<=get("N", envir=e1))
            assign("K", get("K", envir=e1)+1, envir=e1)
        if (get("K", envir=e1)>get("N", envir=e1)) {
          if (recycle) assign("K", 1, envir=e1)
          if (!recycle) {
            assign("K", get("N", envir=e1), envir=e1)
            cat("End of burst !\n")
          }
        }
        showz()
      }
      if (key == "l") {
        assign("addlines", !get("addlines", envir=e1), envir=e1)
        showz()
      }
      if (key == "g") {
        if (dsp == "w") {
          recom <- TRUE
          while (recom) {
            rr <- readline("Enter a relocation number: ")
            recom <- FALSE
            if (!(rr%in%row.names(get("x", envir=e1)[[1]]))) {
              cat("invalid number\n")
              recom <- TRUE
            }
          }
          assign("K", which(row.names(get("x", envir=e1)[[1]])==as.numeric(rr)), envir=e1)
          showz()
        }
        if (dsp == "t") {
          lv <- tclVar(row.names(get("x", envir=e1)[[1]])[1])
          tu <- tktoplevel(tt, width=500, height=50)
          tkwm.title(tu, "Enter a relocation number")
          tkwm.resizable(tu, 0, 0)
          en <- tkentry(tu, textvariable=lv, width=50)
          submit.but <- tkbutton(tu, text="    OK     ",
                                 command=function() {
                                   rr <- tclvalue(lv)
                                   if (!(rr%in%row.names(get("x", envir=e1)[[1]]))) {
                                     tkmessageBox(message="invalid number",
                                                  type="ok")
                                   } else {
                                     assign("K", which(row.names(get("x", envir=e1)[[1]])==as.numeric(rr)), envir=e1)
                                     showz()
                                     tkdestroy(tu)}})
          tkpack(en, side = "top", fill = "both")
          tkpack(submit.but, side = "bottom")
          tkwait.window(tu)
        }
      }
      if (key == "r") {
        assign("addpoints", !get("addpoints", envir=e1), envir=e1)
        showz()
      }
      if (key == "b") {
        assign("K", 1, envir=e1)
        if (dsp == "w") {
          assign("hoho", select.list(unlist(lapply(u, function(y) attr(y, "burst")))), envir=e1)
          type(paste("Choice of the burst:", get("hoho", envir=e1),"\n\n"))
          assign("x",u[burst=get("hoho", envir=e1)], envir=e1)
          assign("v",u[burst=get("hoho", envir=e1)], envir=e1)
          assign("N", nrow(get("x", envir=e1)[[1]]), envir=e1)
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
                                   assign("hoho", ifelse(nchar(tclvalue(tkcurselection(tli)))==0, 1, as.numeric(tclvalue(tkcurselection(tli)))+1), envir=e1)
                                   type(paste("Choice of the burst:", bubu[get("hoho", envir=e1)],"\n\n"))
                                   tkdestroy(tu)})
          tkpack(tli, side = "left", fill = "both", expand = TRUE)
          tkpack(scr2, side = "right", fill = "y")
          tkpack(tfr, side = "right", fill = "y")
          tkpack(submit.but, side = "bottom")
          tkwait.window(tu)
          assign("x",u[burst=bubu[get("hoho", envir=e1)]], envir=e1)
          assign("v",u[burst=bubu[get("hoho", envir=e1)]], envir=e1)
          assign("N", nrow(get("x", envir=e1)[[1]]), envir=e1)
          showz()
        }
      }
      if (key == "i") {
        if (get("ajoubu", envir=e1)) {
          assign("ajoubu", FALSE, envir=e1)
          showz()
        } else {
          if (dsp == "w") {
            assign("buadd", select.list(unlist(lapply(u,
                                                      function(y) attr(y, "burst"))),
                                  multiple=TRUE), envir=e1)
            if (length(get("buadd", envir=e1)>0)) {
              type(paste("show bursts:", paste(get("buadd", envir=e1), collapse=" "),"\n\n"))
              assign("ajoubu", TRUE, envir=e1)
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
                                       assign("ajoubu", TRUE, envir=e1)
                                       assign("buadd", bubu[as.numeric(unlist(strsplit(tclvalue(tkcurselection(tli)), " ")))+1], envir=e1)
                                       type(paste("show bursts:", paste(get("buadd", envir=e1), collapse=" "),"\n\n"))
                                       showz()
                                       tkdestroy(tu)}})
            tkpack(tli, side = "left", fill = "both", expand = TRUE)
            tkpack(scr2, side = "right", fill = "y")
            tkpack(tfr, side = "right", fill = "y")
            tkpack(submit.but, side = "bottom")
            tkwait.window(tu)
            assign("x", u[burst=bubu[get("hoho", envir=e1)]], envir=e1)
            assign("v", u[burst=bubu[get("hoho", envir=e1)]], envir=e1)
            assign("N", nrow(get("x", envir=e1)[[1]]), envir=e1)
            showz()
          }
        }
      }
      if (key == "p") {
        if (get("K", envir=e1)>1)
          assign("K", get("K", envir=e1)-1, envir=e1)
        if (get("K", envir=e1)==1) {
          if (recycle)
            assign("K", get("N", envir=e1), envir=e1)
          if (!recycle) {
            assign("K", 1, envir=e1)
            cat("Beginning of burst!\n")
          }
        }
        showz()
      }
      if (key == "a") {
        assign("K", get("N", envir=e1), envir=e1)
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

