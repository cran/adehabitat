"plot.kselect" <-
function(x, xax=1, yax=2, ...)
{
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


  
  nf <- layout(matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3), 
               respect = TRUE)
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  s.corcircle(x$as, xax, yax, sub = "Axis", csub = 2, clab = 1.25)
  s.arrow(x$l1, xax, yax, sub = "Variables", csub = 2, clab = 1.25)
  scatterutil.eigen(x$eig, wsel = c(xax, yax))

  ## Graphe principal...
  ## polygones, vecteurs, et en tout petit, les ru
  ## 1. Calcul des RU
  U<-as.matrix(x$l1*x$lw)
  ls<-as.matrix(x$initab)%*%U
  liani<-split(as.data.frame(ls), x$initfac)
  liwei<-split(x$initwei, x$initfac)

  mav<-as.data.frame(t(as.matrix(data.frame(lapply(liani, function(x) apply(x, 2, mean))))))
  names(mav)<-names(x$li)
  mutemp<-list()
  for (i in 1:length(liwei))
    mutemp[[i]]<-apply(liani[[i]], 2, function(x) weighted.mean(x, liwei[[i]]))
  mut<-as.data.frame(t(as.matrix(data.frame(mutemp))))

  names(mut)<-names(x$li)
  row.names(mut)<-names(x$tab)
  row.names(mav)<-names(x$tab)
  s.label(rbind(mav, mut), xax, yax, clab = 0, cpo = 0, sub = "Marginality vectors", 
          csub = 2)
  

  for (i in 1:length(liani))
      arrows(mav[i,xax], mav[i,yax], mut[i,xax], mut[i,yax], lwd=2, angle=20)
  s.label(mav, xax, yax, add.plot=TRUE, clab=1.5)
  
  
  ## Resource units
  s.class(as.data.frame(ls), x$initfac, cstar=0, cellipse=0, clab=1.5, sub="Available Resource units", csub=2)
  
  for (i in 1:length(liani))
    polygon(liani[[i]][chull(liani[[i]][,xax], liani[[i]][,yax]),xax],
            liani[[i]][chull(liani[[i]][,xax], liani[[i]][,yax]),yax])

  ## Animals
  s.arrow(x$co, xax, yax, clab = 1.25, cpo = 0.5, sub = "Animals", 
          csub = 2)
  

}

