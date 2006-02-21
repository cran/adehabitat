".buffer.point.unic" <-
function(x, md)
  {
    res<-attr(x, "cellsize")
    nmax<-ceiling(md/res)
    
    calc<-matrix(NA,ncol = 2*nmax+1, nrow=2*nmax+1)
    cour<-nmax+1

    for (i in 1:nrow(calc)) {
      for (j in 1:ncol(calc)) {
        d1<-(((cour-i-0.5)*res)^2+((cour-j-0.5)*res)^2)
        d2<-(((cour-i-0.5)*res)^2+((cour-j+0.5)*res)^2)
        d3<-(((cour-i+0.5)*res)^2+((cour-j-0.5)*res)^2)
        d4<-(((cour-i+0.5)*res)^2+((cour-j+0.5)*res)^2)
        
        if (min(c(d1,d2,d3,d4))<= md^2)
          calc[i,j]<-1
      }
    }
    return(calc)
  }

"buffer" <-
function(pts, x, dist)
  {
    if (inherits(x, "asc"))
      x<-as.kasc(list(toto=x))
    if (inherits(x, "kasc"))
      x<-storemapattr(x)
    if (!inherits(x, "mapattr"))
      stop("non convenient format for x")
    res<-attr(x, "cellsize")
    nmax<-ceiling(dist/res)
    
    ## calcul du calque
    calc<-.buffer.point.unic(x, dist)
    calc0<-calc
    calc0[is.na(calc0)]<-0
    asc<-count.points(pts, x)
    vasc<-as.vector(asc)
    
    ## Calcul d'un idlig et d'un idcol de la meme longueur que vasc
    ## idasc permettra de déterminer quelles sont les idlig et idcol
    ## des cellules ou >0
    idasc<-1:length(vasc)
    idcons<-idasc[vasc>0]
    idlig<-as.vector(row(asc))
    idcol<-as.vector(col(asc))
    
    ligcons<-idlig[idcons]
    colcons<-idcol[idcons]

    sorties<-matrix(0, nrow=(attr(x, "ncol")+2*nmax),
                ncol=(attr(x, "nrow")+2*nmax))

    for (i in 1:length(idcons)) {
      car<-matrix(0, nrow=(attr(x, "ncol")+2*nmax),
                  ncol=(attr(x, "nrow")+2*nmax))
      
      car[c(ligcons[i]:(ligcons[i]+2*nmax)), c(colcons[i]:(colcons[i]+2*nmax))]<-
        car[c(ligcons[i]:(ligcons[i]+2*nmax)), c(colcons[i]:(colcons[i]+2*nmax))]+
        calc0
      sorties<-sorties+car
    }
    sorties<-sorties[c((nmax+1):(nrow(sorties)-nmax)),c((nmax+1):(ncol(sorties)-nmax))]
    sorties<-matrix(as.numeric(sorties!=0), ncol=attr(x, "nrow"))

    sorties[sorties==0]<-NA
    attr(sorties, "cellsize")<-attr(x, "cellsize")
    attr(sorties, "xll")<-attr(x, "xll")
    attr(sorties, "yll")<-attr(x, "yll")
    attr(sorties, "type")<-"numeric"
    class(sorties)<-"asc"
    return(sorties)
  }

