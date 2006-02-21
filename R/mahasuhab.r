"mahasuhab" <-
function(kasc, pts, type=c("distance", "probability"))
  {
    x<-pts
    type<-match.arg(type)
    if (!inherits(kasc, "kasc"))
      stop("should be an object of class \"kasc\"")
    if (ncol(x)!=2)
      stop("x should have 2 columns")
    kasc<-managNAkasc(kasc)

    ## utilisation:
    hihi<-join.kasc(x, kasc)
    used<-list()
    for (i in 1:ncol(hihi)) {
      if (is.factor(hihi[,i]))
        used[[i]]<-acm.disjonctif(data.frame(hihi[,i]))[,-1]
      else
        used[[i]]<-hihi[,i]
    }
    used[[i+1]]<-rep(1, nrow(hihi))
    hihi<-as.data.frame(used)
    hihi<-hihi[!is.na(hihi[,1]),]
    mu<-apply(hihi,2, function(x) mean(x, na.rm=TRUE))
    varcov<-t(as.matrix(hihi))%*%as.matrix(hihi)/nrow(hihi)

    ## disponibilit�
    ava<-list()
    for (i in 1:ncol(kasc)) {
      if (is.factor(kasc[,i]))
        ava[[i]]<-acm.disjonctif(data.frame(kasc[,i]))[,-1]
      else
        ava[[i]]<-kasc[,i]
    }
    ava[[i+1]]<-rep(1, nrow(kasc))
    df<-as.data.frame(ava)
    map<-mahalanobis(as.matrix(df), mu, varcov)
    if (type=="probability")
      map<-1-pchisq(map, ncol(hihi)-1)
    mat<-matrix(map, attr(kasc,"ncol"), attr(kasc,"nrow"))
    mat<-getascattr(getkasc(kasc, names(kasc)[1]), mat)
    
    return(mat)
  }

