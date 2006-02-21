"plot.sahrlocs" <-
function(x, ani=names(x$hr),
                        var=names(x$sa),
                        type=c("hr.in.sa", "locs.in.hr", "locs.in.sa"),
                        ncla=4, ylog = FALSE,
                        caxis = 0.7, clab = 0.7,
                        errbar=c("SE", "CI"), alpha=0.05,
                        draw=TRUE, ...)
{
  type<-match.arg(type)
  errbar<-match.arg(errbar)
  if (!inherits(x, "sahrlocs"))
    stop("should be an object of class \"sahrlocs\"")
  if (any(is.na(match(ani, names(x$hr)))))
    stop(paste("\"",
               ani[is.na(match(ani, names(x$hr)))],
               "\" is not a valid name"))
  if (length(ani)<2)
    stop("please select at least 2 individuals")
  
  if (any(is.na(match(var, names(x$sa)))))
    stop(paste("\"",
               var[is.na(match(var, names(x$sa)))],
               "\" is not a valid variable"))
  ngraph<-length(var)+1
  if (draw) {
    opar<-par(mfrow=c(1,2), ask=TRUE)
    on.exit(par(opar))
  }
  ## liste de sortie
  liso<-list()
  ty<-strsplit(type, ".in.")[[1]]

  for (i in var) {
    v<-x$sa[[i]]

    ## ordonner les wi?
    if (is.factor(v))
      noorder<-TRUE
    else
      noorder<-FALSE

    if (!is.factor(v))
      v<-cut(v, breaks=ncla)
    if (ty[2]=="sa") {
      av<-table(v)
      nav<-names(av)
      av<-as.vector(av)
      names(av)<-nav
      if (ty[1]=="locs") {
        locs<-x$locs[ani]
        us<-t(as.matrix(as.data.frame(apply(locs,2,function(x) table(rep(v, x))))))
        liso[[i]]<-widesII(us, av, alpha=alpha)
        if (draw)
          plot(liso[[i]], ylog=ylog, main=i, clab=clab, caxis=caxis, errbar=errbar, noorder=noorder)
      }
      else {
        hr<-x$hr[ani]
        hr <- as.data.frame(apply(hr, 2, function(x) {x[is.na(x)] <- 0; return(x)}))
        us<-t(as.matrix(as.data.frame(apply(hr,2,function(x) table(rep(v, x))))))
        liso[[i]]<-widesII(us, av, alpha=alpha)
        if (draw)
          plot(liso[[i]], ylog=FALSE, main=i, clab=clab, caxis=caxis, errbar=errbar, noorder=noorder)
      }
    }
    else {
      hr<-x$hr[ani]
      hr <- as.data.frame(apply(hr, 2, function(x) {x[is.na(x)] <- 0; return(x)}))
      av<-t(as.matrix(as.data.frame(apply(hr,2,function(x) table(rep(v, x))))))
      locs<-x$locs[ani]
      us<-t(as.matrix(as.data.frame(apply(locs,2,function(x) table(rep(v, x))))))
      ## Vérifications que pas de classes vides
      toto<-as.vector(apply(av,2,sum))
      av<-av[,toto!=0]
      us<-us[,toto!=0]
      options(warn=-1)
      liso[[i]]<-widesIII(us, av, alpha=alpha)
      options(warn=0)
      if (draw)
        plot(liso[[i]], ylog, main=i, clab=clab, caxis=caxis, errbar=errbar, noorder=noorder)
    }
  }
  class(liso)<-"plotsahr"
  invisible(liso)
}

