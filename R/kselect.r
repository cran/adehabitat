"kselect" <-
function(dudi, factor, weight, scannf = TRUE, nf = 2, ewa = FALSE)
{

                                        # 1. Vérifications
  if (!inherits(dudi, "dudi")) stop("Object of class dudi expected")

  X<-dudi$tab
  f<-factor
  ab<-weight

  if (nrow(X) != length(f))
    stop("The factor should have the same number of observations as the dudi object")
  if (nrow(X) != length(ab))
    stop("The vector of weights should have the same number of observations as the dudi object")
  if (!is.vector(weight))
    stop("The weights should be placed in a vector")
  if (!is.factor(f)) f<-factor(f)

  lo<-split(X,f)
  ab<-split(ab,f)
  if (!ewa)
    poco<-unlist(lapply(ab, function(x) sum(x)/sum(weight)))
  if (ewa)
    poco<-rep(1/nlevels(f), nlevels(f))
  ab<-lapply(ab, function(x) x/sum(x))
  


                                        # 2. Calcul des df des cdg dispo
  m<-data.frame(lapply(lo, function(x) apply(x,2,mean)))

                                        # 3. Calcul des df des cdg utilisés
  n<-list()
  for (i in 1:length(lo)) {
    w<-ab[[i]]
    D<-lo[[i]]
    n[[names(lo)[i]]]<-apply(D,2,function(x) sum(w*x))
  }
  n<-data.frame(n)

                                        # 4. Analyse
  z<-as.dudi(df=n-m, col.w=poco,
             row.w=dudi$cw, call=match.call(), type="kselect",
             scannf = scannf, nf = nf)

  z$initab<-dudi$tab
  z$initfac<-factor
  z$initwei<-weight
  
  U <- as.matrix(z$l1) * unlist(z$lw)
  U <- data.frame(t(as.matrix(dudi$c1)) %*% U)
  row.names(U) <- names(dudi$li)
  names(U) <- names(z$li)
  z$as <- U
  

  return(z)
}

