"sahrlocs2kselect" <-
function(sahr)
  {
  sa<-sahr$sa
  fac <- unlist(lapply(sa, is.factor))
  if (any(fac)) {
    lev<-list()
    for (i in names(fac[fac]))
      lev[[i]]<-levels(sa[[i]])
    sa<-lapply(sa, as.numeric)
    sa<-as.data.frame(sa)
  }
  sa<-as.matrix(sa)
  hr<-sahr$hr
  hr<-lapply(hr, as.numeric)
  hr<-as.matrix(as.data.frame(hr))
  locs<-as.matrix(sahr$locs)

  sa[is.na(sa)]<-(-9999)
  hr[is.na(hr)]<-(-9999)

  nh<-ncol(sa)
  np<-nrow(sa)
  na<-ncol(hr)

  so1<-.C("nls2k", as.double(t(sa)), as.double(t(hr)), as.integer(nh),
          as.integer(np), as.integer(na), PACKAGE="adehabitat")[[5]]
  
  so2<-.C("sahr2ksel", as.double(t(sa)), as.double(t(hr)), as.double(t(locs)),
          as.integer(nh), as.integer(np), as.integer(na), as.integer(so1),
          double (so1*nh), integer(so1), double(so1), PACKAGE="adehabitat")

  ta<-so2[[8]]
  ta<-as.data.frame(matrix(ta, ncol=nh, byrow=TRUE))
  names(ta)<-names(sahr$sa)
  if (any(fac>0)) {
    for (i in names(lev))
      ta[,i]<-factor(ta[[i]], levels=c(1:length(lev[[i]])), labels=lev[[i]])
  }
  factor<-so2[[9]]
  weight<-so2[[10]]

  factor<-factor(factor, labels=names(sahr$hr))
  
  sorties<-list(tab=ta, factor=factor, weight=weight)
  return(sorties)
}

