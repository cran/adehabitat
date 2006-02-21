"profilehab" <-
function(rankma, wi)
{
  s<-(rankma=="+++")|(rankma=="---")
  rm.p<-s
  n.hab<-ncol(rankma)
  classement<-rank(wi)
  rankma<-rankma[order(classement, decreasing=TRUE),order(classement, decreasing=TRUE)]
  rm.p<-rm.p[order(classement, decreasing=TRUE),order(classement, decreasing=TRUE)]
  habitat<-paste(" ",colnames(rankma)[1],sep="")
  for (i in 2:n.hab) habitat<-paste(habitat,colnames(rankma)[i],sep="  ")
  habitat<-paste(habitat," ",sep="")
  nbcar.nom<-nchar(colnames(rankma))+2
  carac<-c(1:n.hab)
  profil<-matrix(ncol=1,nrow=n.hab)
  
  for (i in 1:n.hab){
    for (j in 1:n.hab){
      if (rm.p[i,j]) carac[j]<-" " else carac[j]<-"-"
      if (rm.p[i,j]) t<-" " else t<-"-"
      for (k in 1:(nbcar.nom[j]-1)) carac[j]<-paste(carac[j],t,sep="")
    }
    carac.t<-carac[1]
    for (j in 2:n.hab) carac.t<-paste(carac.t,carac[j],sep="")
    profil[i,1]<-carac.t
    carac.t<-0
    carac<-c(1:n.hab)
  }
  
  rownames(profil)<-colnames(rankma)
  profil<-rbind(habitat,profil)
  colnames(profil)<-""
  return(profil)
}

