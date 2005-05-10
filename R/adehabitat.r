##### Chargement de base

cat("This package requires ade4 to be installed\n\n")
require(ade4)
require(gpclib)

.First.lib <- function(lib, pkg) {
  library.dynam("adehabitat", pkg, lib)
}


#####################################################################
#####################################################################
### Estimation du MCP des animaux

mcp<-function(xy, id, percent=95)
{
  xy<-xy[!is.na(xy[,1]),]
  xy<-xy[!is.na(xy[,2]),]
  id<-id[!is.na(xy[,1])]
  id<-id[!is.na(xy[,2])]
  if (length(id)!=nrow(xy)) stop("xy and id should be of the same length")
  if (percent>100) {
	warning("The MCP is estimated using all relocations (percent>100)")
	percent<-100
	}

  id<-factor(id)
  if (min(table(id))<5)
    stop("At least 5 relocations are required to fit an home range")
  
  r<-split(xy, id)
  est.cdg<-function(xy) apply(xy, 2, mean)
  cdg<-lapply(r,est.cdg)
  levid<-levels(id)

### Préparation des sorties
  X<-0
  Y<-0
  ID<-"0"

  for (i in 1:nlevels(id)) {
	k<-levid[i]
	df.t<-r[[levid[i]]]
	cdg.t<-cdg[[levid[i]]]

### Calcul des distances au centre de gravité et conservation des plus proches
	dist.cdg<-function(xyt) {
          d<-sqrt( ( (xyt[1]-cdg.t[1])^2 ) + ( (xyt[2]-cdg.t[2])^2 ) )
          return(d)
        }

	di<-apply(df.t, 1, dist.cdg)
	key<-c(1:length(di))
	
	acons<-key[di<=quantile(di,percent/100)]
	xy.t<-df.t[acons,]
	
	
                                        ## Coordonnées du MCP
	coords.t<-chull(xy.t[,1], xy.t[,2])
	xy.bord<-xy.t[coords.t,]	
	
	X<-c(X,xy.bord[,1])
	Y<-c(Y,xy.bord[,2])
	ID<-c(ID, rep(as.character(levid[i]), nrow(xy.bord)))
      }

  ID<-as.data.frame(ID)
  res<-cbind.data.frame(ID,X,Y)
  res<-res[-1,]
  res[,1]<-factor(res[,1])
  res<-as.area(res)
  return(res)
}


##################################################################
##################################################################
##################################################################
#####
##### colasc pour créer une légende pour un asc de type factor
##### 


colasc<-function(x, ...)
  {
    if (!inherits(x, "asc"))
      stop("Should be an \"asc\" object")
    l<-list(...)
    n<-names(l)
    i<-NA
    tc<-levels(x)
    for (i in n) {
      if (!any(tc==i))
        stop(paste(i, "is not a valid level of the factors"))
    }
    coul<-0
    for (i in 1:length(tc)) {
      u<-tc[i]
      coul[i]<-l[[u]]
    }
    return(coul)
  }







#########################################################################
#########################################################################
#########################################################################
#####
##### import.asc allows to import Arcview ascii raster file


import.asc<- function (file, type = c("numeric", "factor"), lev = NULL,
                       levnb = 1, labnb = 3) 
{
  type <- match.arg(type)
  if (substr(file, nchar(file) - 3, nchar(file)) != ".asc") 
    stop("not a valid .asc file")
  if ((type != "numeric") & (type != "factor")) 
    stop("argument type should be \"factor\" or \"numeric\"")
  if ((type == "numeric") & (!is.null(lev))) 
    stop("lev can be specified only when type is \"factor\" ")
  if ((type == "factor") & (length(lev) == 1)) 
    if (!file.exists(lev)) 
      stop("lev is not a valid file")
  zz <- file(file, "r")
  nc <- readLines(zz, 1)
  nl <- readLines(zz, 1)
  xll <- readLines(zz, 1)
  yll <- readLines(zz, 1)
  cs <- readLines(zz, 1)
  nas <- readLines(zz, 1)
  cs <- strsplit(cs, " ")
  cs <- as.numeric(cs[[1]][length(cs[[1]])])
  cornx <- TRUE
  corny <- TRUE
  xll <- strsplit(xll, " ")
  if ((xll[[1]][1] == "xllcenter") | (xll[[1]][1] == "XLLCENTER")) 
    cornx <- FALSE
  xll <- as.numeric(xll[[1]][length(xll[[1]])])
  yll <- strsplit(yll, " ")
  if ((yll[[1]][1] == "yllcenter") | (xll[[1]][1] == "YLLCENTER")) 
    corny <- FALSE
  yll <- as.numeric(yll[[1]][length(yll[[1]])])
  nas <- strsplit(nas, " ")
  nas <- as.numeric(nas[[1]][length(nas[[1]])])
  nc <- strsplit(nc, " ")
  nc <- as.numeric(nc[[1]][length(nc[[1]])])
  nl <- strsplit(nl, " ")
  nl <- as.numeric(nl[[1]][length(nl[[1]])])
  tmp <- readLines(zz)
  close(zz)
  file.create("toto230876.tmp")
  zz <- file("toto230876.tmp", "w")
  writeLines(tmp, zz)
  close(zz)
  output <-scan("toto230876.tmp", quiet=TRUE)
  file.remove("toto230876.tmp")
  output[output == nas] <- NA
  output<-matrix(c(as.matrix(output)), ncol=nl)
  output <- output[, ncol(output):1]
  if (type == "factor") {
    if (is.null(lev)) 
      lev <- levels(factor(output))
    if (length(lev) > 1) {
      if (length(lev) != length(levels(factor(output)))) 
        stop("uncorrect length of lev")
    }
    if (length(lev) == 1) {
            toto <- read.table(lev, header = TRUE, sep = ",")
            toto <- data.frame(lev = toto[, levnb],
                               hihi = rep(1, nrow(toto)),
                               lab = toto[, labnb])
            toto <- toto[order(toto[, 1]), ]
            if (nrow(toto) != nlevels(factor(output))) 
              stop("lev is not a valid correspondence table exported from Arcview")
            lev <- as.character(toto[, 3])
          }
    attr(output, "levels") <- lev
  }
  attr(output, "xll") <- xll
  if (cornx) 
    attr(output, "xll") <- xll + cs/2
  attr(output, "yll") <- yll
  if (corny) 
    attr(output, "yll") <- yll + cs/2
  attr(output, "cellsize") <- cs
  attr(output, "type") <- type
  class(output) <- "asc"
  return(output)
}




#########################################################################
#########################################################################
#########################################################################
#####
##### export.asc allows to import Arcview ascii raster file


export.asc<-function(x, file)
  {

    if (!inherits(x, "asc")) stop("Non convenient data")    

### File header reading
    if (substr(file, nchar(file)-3, nchar(file))!=".asc")
      file<-paste(file, ".asc", sep="")
    
    file.create(file)
    zz<-file(file, "w")
    nc<-paste("ncols", "         ", nrow(x), sep="")
    nl<-paste("nrows", "         ", ncol(x), sep="")
    xll<-paste("xllcorner", "     ",
               attr(x, "xll")-attr(x, "cellsize")/2, sep="")
    yll<-paste("yllcorner", "     ",
               attr(x, "yll")-attr(x, "cellsize")/2, sep="")
    cs<-paste("cellsize", "      ", attr(x, "cellsize"), sep="")
    nas<-paste("NODATA_value", -9999, sep="  ")

    writeLines(nc, zz)
    writeLines(nl, zz)
    writeLines(xll, zz)
    writeLines(yll, zz)
    writeLines(cs, zz)
    writeLines(nas, zz)

    close(zz)
    x[is.na(x)]<--9999
    x<-x[,ncol(x):1]
    x<-rbind(x, rep("\n", ncol(x)))

    sink(file, append=TRUE)
    cat(x)
    sink()
    
  }





#########################################################################
#########################################################################
#########################################################################
#####
##### print.asc allows to print objects of class "asc"


print.asc<-function(x, ...)
{
  if (!inherits(x, "asc")) stop("Non convenient data")
  cat("Raster map of class \"asc\":\n")
  cat("Cell size: ", attr(x, "cellsize"), "\n")
  cat("Number of rows: ", ncol(x), "\n")
  cat("Number of columns: ", nrow(x), "\n")
  cat("Type: ", attr(x, "type"), "\n")
}





##########################################################################
##########################################################################
##########################################################################
#####
##### buffer(pts, dist) calcule un buffer à la distance dist des points
##### références de pts




## x peut etre soit "asc", soit "kasc"
.buffer.point.unic<-function(x, md)
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

### Pour un seul jeu de points

buffer<-function(pts, x, dist)
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


### pour plusieurs jeux de points

buffer.ani<-function(pts, fac, x, dist)
  {
    if (inherits(x, "asc"))
      x<-as.kasc(list(toto=x))
    if (inherits(x, "kasc"))
      x<-storemapattr(x)
    if (!inherits(x, "mapattr"))
      stop("non convenient format for x")
    if (length(fac)!=nrow(pts))
      stop("factor should have the same length as pts")
    
    lipts<-split(pts, fac)
    sorties<-list()
    
    for (i in names(lipts)) {
      ptst<-lipts[[i]]
      sorties[[i]]<-buffer(ptst, x, dist)
    }
    
    sor<-as.kasc(sorties)
    
    return(sor)
  }







##########################################################################
##########################################################################
##########################################################################
#####
##### getXYcoords allows to get the coordinates of the rows and
##### of the column of an asc object


getXYcoords<-function(w)
  {
    if ((((!inherits(w, "asc"))&
         (!inherits(w, "kasc")))&
        (!inherits(w,"sahrlocs")))&
        (!inherits(w,"mapattr")))
      stop("non convenient object")
    
    cs<-attr(w, "cellsize")
    xll<-attr(w, "xll")
    yll<-attr(w, "yll")
    if (inherits(w,"asc")) {
      nr<-nrow(w)
      nc<-ncol(w)
    }
    if (((inherits(w,"kasc"))|(inherits(w, "sahrlocs")))|
        (inherits(w, "mapattr"))){
      nc<-attr(w, "nrow")
      nr<-attr(w, "ncol")
    }
    x<-xll+c(0:(nr-1))*cs
    y<-yll+c(0:(nc-1))*cs
    return(list(x=x, y=y))
  }


##################################################################
##################################################################
##################################################################
#####
##### image.asc contour.asc and persp.asc  gives
##### a representation of an asc object

image.asc<-function (x, clfac = NULL, col = gray((240:1)/256), ...) 
{
    if (!inherits(x, "asc")) 
        stop("not an \"asc\" object")
    z <- x
    xy <- getXYcoords(z)
    x <- xy$x
    y <- xy$y
    if (attr(z, "type") == "numeric") 
        image(x = x, y = y, z, asp = 1, col = col, ...)
    if (attr(z, "type") == "factor") {
        if (is.null(clfac)) {
            clfac <- rainbow(nlevels(z))
            clfac <- clfac[as.numeric(levels(factor(z)))]
        }
        image(x = x, y = y, z, asp = 1, col = clfac, ...)
    }
}


contour.asc<-function(x, ...)
  {
    if (!inherits(x, "asc")) stop("not an \"asc\" object")
    if (attr(x, "type")=="factor")
      stop("function contour cannot be used with factors")
    z<-x
    xy<-getXYcoords(z)
    x<-xy$x
    y<-xy$y
    contour(x=x, y=y, z,  ...)
  }


persp.asc<-function(x, ...)
  {
    if (!inherits(x, "asc")) stop("not an \"asc\" object")
    if (attr(x, "type")=="factor")
      stop("function persp cannot be used with factors")
    z<-x
    xy<-getXYcoords(z)
    x<-xy$x
    y<-xy$y
    persp(x=x, y=y, z, ...)
  }


plot.asc<-function(x, ...)
  {
    if (!inherits(x, "asc"))
      stop("should be an object of class \"asc\"")
    if (attr(x, "type")=="factor")
      stop("not implemented for factors")
    xy<-getXYcoords(x)
    filled.contour(xy$x, xy$y, x, asp=1, ...)
  }


#####################################################################
#####################################################################
#####################################################################
#####
##### as.kasc creates a kasc object from k asc objects

as.kasc<-function(l)
  {

### 1. Verification que les attributs de tous les asc sont identiques
    clobj<-unlist(lapply(l,class))
    if (!all(clobj=="asc")) stop("input should be a list of \"asc\" objects")
    u<-TRUE
    la<-list()
    for (i in 1:length(l)) la[[i]]<-attributes(l[[i]])
    o<-la[[1]]
    if (o$type=="factor") {
          o<-o[names(o)!="levels"]
        }
    o<-o[names(o)!="type"]
    o<-o[names(o)!="dimnames"]

### 2. stockage des attributs, mais on benne le type de variable
###    et un éventuel levels
    if (length(l)>1) {
      for (i in 2:length(l))
        {
          tmp<-la[[i]]
          if (tmp$type=="factor") {
            tmp<-tmp[names(tmp)!="levels"]
          }
          tmp<-tmp[names(tmp)!="type"]
          tmp<-tmp[names(tmp)!="dimnames"]
          
          u[i]<-all(sort(unlist(tmp))==sort(unlist(o)))
        }
      if (!all(u)) stop("all the objects should have the same attributes")
    }
    
### 3. Calcul du kasc
    u<-as.vector(l[[1]])
    if (attr(l[[1]], "type")=="factor") {
      ct<-levels(l[[1]])
      lab<-list()
      for (j in 1:length(ct)) {
        lab[[j]]<-ct[j]
      }
      lab<-unlist(lab)
      u<-factor(u, levels=1:length(lab), labels=lab)
    }
    output<-data.frame(u)

    if (length(l)>1) {
      for (i in 2:length(l)) {
        u<-as.vector(l[[i]])
        if (attr(l[[i]], "type")=="factor") {
          ct<-levels(l[[i]])
          lab<-list()
          for (j in 1:length(ct)) {
            lab[[j]]<-ct[j]
          }
          lab<-unlist(lab)
          u<-factor(u, levels=1:length(lab), labels=lab)
        }
        output<-cbind.data.frame(output, u)
      }
    }

### 5. Les attributs
    attr(output, "cellsize")<-attr(l[[1]], "cellsize")
    attr(output, "xll")<-attr(l[[1]], "xll")
    attr(output, "yll")<-attr(l[[1]], "yll")
    attr(output, "ncol")<-nrow(l[[1]])
    attr(output, "nrow")<-ncol(l[[1]])
    attr(output, "type")<-unlist(lapply(l, function(x) attr(x, "type")))
    names(output)<-names(l)
    class(output)<-c("kasc","data.frame")
    return(output)    
  }



#############################
##
## Pour ne conserver que les zones pas NA partout

managNAkasc<-function(x)
  {
    if (!inherits(x,"kasc")) stop("non convenient data")
    class(x)<-"data.frame"
    ## Conservation que des pixels non NA pour toutes les cartes
    tmpy<-is.na(x)
    tmp<-apply(tmpy, 1, function(x) sum(as.numeric(x)))
    x[tmp!=0,]<-rep(NA, ncol(x))
    class(x)<-c("kasc", "data.frame")
    return(x)
  }




#####################################################################
#####################################################################
#####################################################################
#####
##### image.kasc donne des images des cartes de l'objet kasc


image.kasc<-function(x,  var=names(x),
                     mar=if (length(var)>1) c(0,0,2,0) else c(5.1,4.1,4.1,2.1),
                     axes=(length(var) == 1),
                     clfac=NULL, col=gray((240:1)/256), mfrow=NULL,
                     ...)
  {
    w<-x
    if (!inherits(w,"kasc")) stop("object should be of class \"kasc\"")
    if (is.null(mfrow))
      mfrow=n2mfrow(length(var))
    opar<-par(mfrow=mfrow, mar=mar)
    on.exit(par(opar))
    for (i in var) {
      el<-getkasc(x, i)
      if (attr(el, "type")=="factor") {
        if (!is.null(clfac)) {
          clf<-clfac[[i]]
        } else {
          clf<-NULL
        }
        if (length(var)>1)
          image.asc(el, main=i, axes=axes, clfac=clf, ... )
        if (length(var)==1)
          image.asc(el, axes=axes, clfac=clf, ... )
      } else {
        if (length(var)>1)
          image.asc(el, main=i, axes=axes, col=col, ...)
        if (length(var)==1)
          image.asc(el, axes=axes, col=col, ... )
      }
    }
  }




#########################################################################
#########################################################################
#########################################################################
#####
##### print.kasc allows to print objects of class "kasc"

print.kasc<-function(x, ...)
{
  if (!inherits(x, "kasc")) stop("Non convenient data")
  cat("Raster map of class \"kasc\":\n")
  cat("Cell size: ", attr(x, "cellsize"), "\n")
  cat("Number of rows: ", attr(x, "nrow"), "\n")
  cat("Number of columns: ", attr(x, "ncol"), "\n\n")

  cat("Variables measured:\n")
  n<-names(x)
  for (i in 1:length(n)) {
    if (is.factor(x[[i]])) {
      typ<-"factor"
    } else {
      typ<-"numeric"
    }
    cat(paste(i, ". ", n[i], ": ", typ, "\n", sep=""))
  }
  cat("\n")
}



#########################################################################
#########################################################################
#########################################################################
#####
##### getkasc for conversion from kasc to asc


getkasc<-function(x, var)
  {
    w<-x
    if (!inherits(w, "kasc")) stop("Non convenient data")

    v<-x[[var]]
    if ((is.numeric(v))|(is.logical(v))) {
      e<-matrix(w[[var]], ncol=attr(w, "nrow"))
      attr(e, "type")<-"numeric"
    } else {
      tc2<-levels(v)
      v<-as.numeric(v)
      e<-matrix(v, ncol=attr(w, "nrow"))
      attr(e, "type")<-"factor"
      attr(e, "levels")<-tc2
    }
    attr(e, "cellsize")<-attr(w, "cellsize")
    attr(e, "xll")<-attr(w, "xll")
    attr(e, "yll")<-attr(w, "yll")
    class(e)<-"asc"
    return(e)
  }







################################################################
################################################################
################################################################
#####
##### count.points.id compte le nombre de points
##### tombant dans chaque cellule d'un raster

count.points<-function(xy, w)
  {
    if (inherits(w, "asc"))
      w<-as.kasc(list(toto=w))
    if (inherits(w, "kasc"))
      w<-storemapattr(w)
    if (!inherits(w, "mapattr"))
      stop("non convenient format for w")


    xyc<-getXYcoords(w)
    xc<-xyc$x-attr(w, "cellsize")/2
    yc<-xyc$y-attr(w, "cellsize")/2
    xc<-c(xc, max(xc)+attr(w, "cellsize"))
    yc<-c(yc, max(yc)+attr(w, "cellsize"))
    x<-xy[,1]
    y<-xy[,2]

    x<-cut(x, xc)
    y<-cut(y, yc)
    output<-as.matrix(table(x, y))
    if (inherits(x, "kasc")) {
      attr(output, "nrow")<-attr(w, "nrow")
      attr(output, "ncol")<-attr(w, "ncol")
    }
    attr(output, "xll")<-attr(w, "xll")
    attr(output, "yll")<-attr(w, "yll")
    attr(output, "cellsize")<-attr(w, "cellsize")
    attr(output, "type")<-"numeric"
    class(output)<-"asc"
    return(output)
  }




count.points.id<-function(xy, id, w)
  {
    x<-xy[,1]
    y<-xy[,2]
    id<-factor(id)
    lx<-split(x, id)
    ly<-split(y, id)
    output<-list()
    for (i in 1:length(levels(id))) 
      output[[levels(id)[i]]]<-count.points(cbind(lx[[i]], ly[[i]]), w)

    output<-as.kasc(output)
    }
           
    


###################################################################
###################################################################
###################################################################
#####
##### mcp.rast = geotraitement d'un MCP avec une carte raster

mcp.rast<-function(poly, w)
  {
    if (inherits(w, "asc"))
      w <- as.kasc(list(to=w))
    if (!inherits(w, "kasc")) stop("non convenient data")
    if (ncol(poly)!=2)
      stop("poly should have two columns")
    if (!all(poly[1,]==poly[nrow(poly),]))
      poly<-rbind(poly, poly[1,])
    xy<-getXYcoords(w)
    huhu<-getkasc(w, names(w)[1])
    huhu[is.na(huhu)]<--9999
    
    toto<-.C("rastpolaire", as.double(poly[,1]), as.double(poly[,2]),
             as.double(xy$x), as.double(xy$y), as.double(t(huhu)),
             as.integer(nrow(huhu)), as.integer(ncol(huhu)),
             as.integer(nrow(poly)), PACKAGE="adehabitat")
    
    output<-matrix(toto[[5]], nrow = nrow(huhu), byrow = TRUE)    
    output[output==0]<-NA

    attr(output, "xll")<-attr(w, "xll")
    attr(output, "yll")<-attr(w, "yll")
    attr(output, "cellsize")<-attr(w, "cellsize")
    attr(output, "type")<-"numeric"
    class(output)<-"asc"
    return(output)
  }
    
    
hr.rast<-function(mcp, w)
  {
    if (inherits(w, "asc"))
      w <- as.kasc(list(to=w))
    if (!inherits(w, "kasc"))
      stop("Non convenient data")
    if (!inherits(mcp, "area"))
      stop("mcp should be of class \"area\"")
    lpc<-split(mcp[,2:3], mcp[,1])
    output<-list()
    for (i in 1:length(lpc))
      output[[names(lpc)[i]]]<-mcp.rast(lpc[[i]], w)
    output<-as.kasc(output)
    return(output)
  }

  




######################################################################
######################################################################
######################################################################
#####
##### as.sahrlocs = création d'un objet de type sahrlocs

as.sahrlocs<-function(mlocs, mhr, msa, descan=NULL)
  {
    if (!inherits(mlocs, "kasc")) stop("non convenient data")
    if (!inherits(mhr, "kasc")) stop("non convenient data")
    if (!inherits(msa, "kasc")) stop("non convenient data")

    atze<-attributes(msa)
    
    nlocs<-nrow(as.data.frame(unclass(mlocs)))
    nhr<-nrow(as.data.frame(unclass(mhr)))
    nsa<-nrow(as.data.frame(unclass(msa)))

    if (!((nlocs==nhr)&(nlocs==nsa)))
      stop("the \"asc\" objects should describe the same area")

    nclocs<-ncol(as.data.frame(unclass(mlocs)))
    nchr<-ncol(as.data.frame(unclass(mhr)))
    if (nclocs!=nchr) stop("different number of individuals in mhr and mlocs")

    output<-list(sa=as.data.frame(unclass(msa)), hr=as.data.frame(unclass(mhr)),
                 locs=as.data.frame(unclass(mlocs)), descan=descan)
    
    attr(output, "nrow")<-atze$nrow
    attr(output, "ncol")<-atze$ncol
    attr(output, "xll")<-atze$xll
    attr(output, "yll")<-atze$yll
    attr(output, "cellsize")<-atze$cellsize
    class(output)<-"sahrlocs"

    return(output)
  }

print.sahrlocs<-function(x, ...)
  {
    if (!inherits(x, "sahrlocs")) stop("object should be of type \"sahrlocs\"")

    cat("************** Object of type sahrlocs **************\n\n")
    nr<-attr(x, "nrow")
    nc<-attr(x, "ncol")
    cat("The area of interest is a ", nr, "*", nc, " raster matrix\n")
    nc<-ncol(x$locs)
    cat(nc, " animals are available :\n")
    print(names(as.data.frame(unclass(x$hr))), ...)
    cat("\n\n the following variables are available for the study area:\n")
    print(names(as.data.frame(unclass(x$sa))), ...)

    if (!is.null(x$descan)) {
      cat("\nthe following variables are available for each monitored animal:\n")
      print(names(x$descan), ...)
    } else {
      cat("\nno variables have been measured on the animals\n")
    }
    
  }
    




#########################################################################
#########################################################################
#########################################################################
#####
##### getsahrlocs = get one component of the oject sahrlocs

getsahrlocs<-function(x, what=c("sa", "hr", "locs"))
  {
    what<-match.arg(what)
    sahr<-x
    rm(x)
    if (!inherits(sahr, "sahrlocs")) stop("non convenient data type")
    if (is.na(match(what, c("sa", "hr", "locs"))))
      stop("what should be either \"sa\", \"hr\", or \"locs\"")

    output<-sahr[[what]]
    attr(output, "nrow")<-attr(sahr, "nrow")
    attr(output, "ncol")<-attr(sahr, "ncol")
    attr(output, "xll")<-attr(sahr, "xll")
    attr(output, "yll")<-attr(sahr, "yll")
    attr(output, "cellsize")<-attr(sahr, "cellsize")
    class(output)<-c("kasc", "data.frame")

    return(output)
  }







#########################################################################
#########################################################################
#########################################################################
#####
##### image.sahrlocs = graphical display of the composition of the home range


image.sahrlocs<-function(x, ani=names(x$hr),
                         var=names(x$sa),
                         mar=c(0,0,0,0), axes=FALSE, dfidxy=NULL,
                         colpts="black", pch=21, bg="white", inv=FALSE, cexpts=0.6,
                         csub=2, possub=c("bottomleft", "bottomright",
                                   "topleft", "topright"), ...)
  {
    possub<-match.arg(possub)
    if (!inherits(x, "sahrlocs"))
      stop("The object x should be of \"sahrlocs\" type")
    ngraph<-length(ani)*length(var)
    opar<-par(mfrow=n2mfrow(ngraph), mar=mar)
    on.exit(par(opar))
    if (!is.null(dfidxy)) lxy<-split(dfidxy, dfidxy[,1])

    ## Creation d'un mini-objet compo.hr
    hr<-x$hr[ani]
    sa<-x$sa[var]
    chr<-list()

    for (i in 1:length(names(hr))) {
      hrt<-hr[,i]
      so<-sa
      so$ani9999<-hrt
      class(so)<-c("kasc", "data.frame")
      so<-managNAkasc(so)
      chr[[names(hr)[i]]]<-so[names(so)!="ani9999"]
    }

    xy<-getXYcoords(x)
    xc<-xy$x
    yc<-xy$y

    ## Calcul du range
    r<-list()
    minx<-0
    maxx<-0
    miny<-0
    maxy<-0
    
    for (i in 1:length(ani)) {
      rtmp<-matrix(chr[[ani[i]]][[1]], ncol=attr(x, "nrow"))
      rowx<-row(rtmp)
      coly<-col(rtmp)
      minx[i]<-min(rowx[!is.na(rtmp)])
      maxx[i]<-max(rowx[!is.na(rtmp)])
      miny[i]<-min(coly[!is.na(rtmp)])
      maxy[i]<-max(coly[!is.na(rtmp)])
      r[[i]]<-c(maxx[i]-minx[i], maxy[i]-miny[i])
    }
    r<-as.data.frame(r)
    rx<-max(r[1,])*(attr(x, "cellsize"))
    ry<-max(r[2,])*(attr(x, "cellsize"))
    cou<-gray((256:1)/256)
    if (inv) cou<-gray((1:256)/256)

    
    for (i in 1:length(ani)){
      for (j in 1:length(var)){
        df<-chr[[ani[i]]]
        class(df)<-"data.frame"

        if (is.numeric(df[[var[j]]])) {
          im<-matrix(df[[var[j]]], ncol=attr(x, "nrow"))
          mx<-min(x$sa[[var[j]]][!is.na(x$sa[[var[j]]])])
          Mx<-max(x$sa[[var[j]]][!is.na(x$sa[[var[j]]])])
          mxMx<-seq(mx, Mx, length=256)
          mx1<-min(im[!is.na(im)])
          Mx1<-max(im[!is.na(im)])
          cou1<-cou[(mxMx>mx1)&(mxMx<Mx1)]
          
          image(xc, yc, im, xlim=c(xc[minx[i]]-rx/5, xc[minx[i]]+6*rx/5),
                ylim=c(yc[miny[i]], yc[miny[i]]+ry), asp=1,
                , axes=axes, col=cou1, ...)
          box()
          scatterutil.sub(paste(ani[i]," : ",var[j]),
                          csub=csub, possub=possub)
        } else {
          im<-matrix(as.numeric(df[[var[j]]]), ncol=attr(x, "nrow"))
          image(xc, yc, im, xlim=c(xc[minx[i]]-rx/5, xc[minx[i]]+6*rx/5),
                ylim=c(yc[miny[i]], yc[miny[i]]+ry), asp=1,
                , axes=axes, col=rainbow(nlevels(df[[var[j]]])), ...)
          box()
          scatterutil.sub(paste(ani[i]," : ",var[j]),
                          csub=csub, possub=possub)
        } 
        
        if (!is.null(dfidxy))
          points(lxy[[ani[i]]][,2], lxy[[ani[i]]][,3],
                 pch=pch, col=colpts, bg=bg, cex=cexpts, ...)
        
      }
    }
  }




#########################################################################
#########################################################################
#########################################################################
#####
##### plot.sahrlocs = graphical display of the selection

plot.sahrlocs<-function(x, ani=names(x$hr),
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

print.plotsahr<-function(x, ...)
{
  cat("***** List of class \"plotsahr\" *****\n\n")
  cat("Selection ratios are computed for the following variables:\n\n")
  for (i in 1:length(x))
    cat(names(x)[i], "\n")
  cat("each variable is a component of the list\n\n")
}





######################################################################
######################################################################
######################################################################
#####
##### kasc2df gives a data frame with all NAs removed and an index
##### giving the position of the kept cells in the initial kasc
##### fromduditokasc converts a dudi object into kasc for mapping


kasc2df<-function(x, var=names(x))
  {
    if (!inherits(x, "kasc")) stop("Non convenient data type")

    w<-data.frame(x[var])
    index<-c(1:nrow(w))
    abenner<-function(x){
      if (any(is.na(x))) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    cons<-apply(w, 1, abenner)
    indcons<-index[cons]
    wcons<-data.frame(w[cons,])
    output<-list(index=indcons, tab=wcons)
  }

df2kasc<-function(df, index, x)
  {
    if (!inherits(df,"data.frame")) stop("non convenient data type")
    if ((!inherits(x,"kasc"))&(!inherits(x,"mapattr")))
      stop("non convenient data type")
    if (ncol(df)<2)
      stop("df should contain at least two columns")
    
    o<-x
    class(o)<-"data.frame"
    N<-attr(o, "nrow")*attr(o, "ncol")
    indw<-c(1:N)
    li<-df
    n1<-nrow(li)

    compl<-matrix(NA, nrow=N-n1, ncol=ncol(li))
    output<-as.data.frame(rbind(as.matrix(li), compl))
    indcompl<-indw[is.na(match(indw, index))]
    indtot<-c(index, indcompl)
    output<-output[sort(indtot, index.return=TRUE)$ix,]
    class(output)<-c("kasc","data.frame")
    attr(output, "nrow")<-attr(x, "nrow")
    attr(output, "ncol")<-attr(x, "ncol")
    attr(output, "xll")<-attr(x, "xll")
    attr(output, "yll")<-attr(x, "yll")
    attr(output, "cellsize")<-attr(x, "cellsize")

    return(output)
  }





#####################################################################
#####################################################################
### kselect: analyse k-select

kselect<-function(dudi, factor, weight, scannf = TRUE, nf = 2, ewa = FALSE)
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







print.kselect<-function (x, ...) 
{
    cat("Duality diagramm\n")
    cat("class: ")
    cat(class(x))
    cat("\n$call: ")
    print(x$call)
    cat("\n$nf:", x$nf, "axis-components saved")
    cat("\n$rank: ")
    cat(x$rank)
    cat("\neigen values: ")
    l0 <- length(x$eig)
    cat(signif(x$eig, 4)[1:(min(5, l0))])
    if (l0 > 5) 
        cat(" ...\n")
    else cat("\n")
    sumry <- array("", c(5, 4), list(1:5, c("vector", "length", 
        "mode", "content")))
    sumry[1, ] <- c("$cw", length(x$cw), mode(x$cw), "column weights")
    sumry[2, ] <- c("$lw", length(x$lw), mode(x$lw), "row weights")
    sumry[3, ] <- c("$eig", length(x$eig), mode(x$eig), "eigen values")
    sumry[4, ] <- c("$initfac", length(x$initfac), mode(x$initfac), "initial factor")
    sumry[5, ] <- c("$initwei", length(x$initwei), mode(x$initwei), "row weights of inittab")
    class(sumry) <- "table"
    print(sumry, ...)
    cat("\n")
    sumry <- array("", c(7, 4), list(1:7, c("data.frame", "nrow", 
        "ncol", "content")))
    sumry[1, ] <- c("$tab", nrow(x$tab), ncol(x$tab), "modified array")
    sumry[2, ] <- c("$li", nrow(x$li), ncol(x$li), "row coordinates")
    sumry[3, ] <- c("$l1", nrow(x$l1), ncol(x$l1), "row normed scores")
    sumry[4, ] <- c("$co", nrow(x$co), ncol(x$co), "column coordinates")
    sumry[5, ] <- c("$c1", nrow(x$c1), ncol(x$c1), "column normed scores")
    sumry[6, ] <- c("$initab", nrow(x$initab), ncol(x$initab), "initial table centered per animal")
    sumry[7, ] <- c("$as", nrow(x$as), ncol(x$as), "axis upon kselect axis")
    class(sumry) <- "table"
    print(sumry)
}






######################################################################
######################################################################
######################################################################
#####
##### sahrlocs2niche = preparation of the niche analysis



sahrlocs2niche<-function(x,  ani=names(x$hr),
                         var=names(x$sa), used=c("hr", "locs"))
  {
    used<-match.arg(used)
    if (!inherits(x,"sahrlocs")) stop("non convenient data")
    output<-list()
    sa<-getsahrlocs(x)
    sa<-sa[var]
    hr<-x$hr[ani]
    locs<-x$locs[ani]
    class(sa)<-c("kasc", "data.frame")
    e<-kasc2df(sa)
    output$tab<-e$tab
    output$index<-e$index

    if (used=="hr") {
      Y<-hr
      for (i in 1:ncol(hr)) Y[is.na(Y[,i]),i]<-0
    }
    if (used=="locs") Y<-locs
    Y<-Y[e$index,]

    output$y<-as.data.frame(Y)

    return(output)
        
  }





######################################################################
######################################################################
######################################################################
#####
##### s.kselect = plot of kselect analysis

kplot.kselect<-function (object, xax = 1, yax = 2, csub = 2,
                         possub = c("topleft", "bottomleft", "bottomright", "topright"),
                         addval=TRUE, cpoint=1, csize=1, clegend=2, ...) 
{
  possub<-match.arg(possub)
  x<-object
  if (!inherits(x, "kselect")) 
    stop("x should be a 'kselect' object")
  if (x$nf == 1) {
    hist.kselect(x)
    return(invisible())
  }
  
  ## 1. Calcul des coordonnées des lignes du tableau initial
  Xi<-x$initab
  Xrecalc<-t(as.matrix(apply(Xi, 1, function(y) y*x$lw/sum(x$lw))))%*%as.matrix(x$l1)
  rx<-range(Xrecalc[,xax])
  ry<-range(Xrecalc[,yax])
  
  li.Xi<-split(as.data.frame(Xrecalc), x$initfac)
  li.wei<-split(x$initwei, x$initfac)
  li.wei<-lapply(li.wei, function(x) x/sum(x) )
  maxsqrtw<-max(sqrt(unlist(li.wei)))
  
  csi<-0
  for (i in 1:length(li.wei))
    csi[i]<-csize*max(sqrt(li.wei[[i]]))/maxsqrtw
  
  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))
  ngraph<-length(li.Xi)
  par(mfrow = n2mfrow(ngraph+1))
  
  for (i in 1:ngraph) {
    Xtmp<-li.Xi[[i]]
    wgtmp<-li.wei[[i]]
    if (addval) {
      s.value(Xtmp, wgtmp, xax, yax,
              sub=names(li.Xi)[i], cpoint=cpoint, xlim=rx, ylim=ry, clegend=0,
              csub=1.5, cgrid=1.5, csize=csi[i])
    }
    s.distri(Xtmp, wgtmp, xax, yax,
             sub=names(li.Xi)[i], add.p=addval, cpoint=cpoint, xlim=rx, ylim=ry,
             ...)
    }
  
  if (addval) {
    coo <- scatterutil.base(dfxy = Xtmp, xax = xax, yax = yax, 
                              xlim = rx, ylim = ry, grid = FALSE, addaxes = FALSE, 
                            cgrid = 0, include.origin = FALSE, origin = c(0,0), 
                            sub = "", csub = 0, possub = "bottomleft", pixmap = NULL, 
                            contour = NULL, area = NULL, add.plot = FALSE)
    
    coeff <- diff(range(coo$x))/15
    br0<-pretty(unlist(li.wei), 4)
    l0 <- length(br0)
    br0 <- (br0[1:(l0 - 1)] + br0[2:l0])/2
    sq0 <- sqrt(abs(br0))
    sq0 <- csize * coeff * sq0/max(sqrt(abs(wgtmp)))
    sig0 <- sign(br0)
    scatterutil.legend.bw.square(pretty(unlist(li.wei), 4), sq0, sig0, clegend=clegend)
  }
}



######################################################################
######################################################################
######################################################################
#####
##### hist.kselect = plot of kselect analysis



hist.kselect<-function(x, xax = 1, mar=c(0,0,0,0), ampl=1,
                       col.out=gray(0.75), col.in=gray(0.75), ncell=TRUE,
                       denout=NULL, denin=NULL, lwdout=1, lwdin=1,
                       maxy=1, csub=2,
                       possub=c("bottomleft", "topleft", "bottomright", "topright"),
                       ncla=15, ...)
  {
    possub<-match.arg(possub)
    if (!inherits(x, "kselect")) stop("should be a 'kselect' object")
    
    ## 1. Creation de la liste
    Xi<-x$initab
    Xrecalc<-t(as.matrix(apply(Xi, 1, function(y) y*x$lw/sum(x$lw))))%*%as.matrix(x$l1)
    li.Xi<-split(as.data.frame(Xrecalc), x$initfac)
    li.wei<-split(x$initwei, x$initfac)
    rx<-range(Xrecalc[,xax])
    br<-seq(rx[1]-(rx[2]-rx[1])/100, rx[2]+(rx[2]-rx[1])/100, length=ncla)
    
    def.par <- par(no.readonly = TRUE)
    on.exit(par(def.par))
    ngraph<-length(li.Xi)
    par(mfrow = n2mfrow(ngraph+1), mar=mar)

    for (i in 1:ngraph) {
      Xtmp<-li.Xi[[i]]
      wgtmp<-li.wei[[i]]

      ## Histogramme extérieur
      vext<-Xtmp[,xax]
      
      ## Histogramme interieur
      poids<-wgtmp
      if (ncell) poids[poids>0]<-1
      vint<-rep(vext,poids)

      ## Calcul des histogrammes
      h<-hist(vext, plot=FALSE, breaks=br, ...)
      hhr<-hist(vint, breaks=h$breaks, plot=FALSE, ...)
      plot(rx, c(-maxy, maxy), type="n",
                     axes=FALSE, ylim=c(-maxy,maxy),
                     main="")
      
      ## Trace des histogrammes
      p<--hhr$counts/sum(hhr$counts)
      q<-h$counts/sum(h$counts)
      rect(hhr$breaks[-length(hhr$breaks)], 0, hhr$breaks[-1],
           p*ampl, col=col.in, lwd=lwdin, density=denin)
      rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1],
           q*ampl, col=col.out, lwd=lwdout, density=denout)
      arrows(mean(vext),0.2, mean(vext), 0, lwd=2, angle=20, length=0.1)
      arrows(mean(vint, na.rm=TRUE), -0.2,mean(vint, na.rm=TRUE),
             0, lwd=2, angle=20, length=0.1)
      scatterutil.sub(names(li.Xi)[i],
                       csub=csub, possub=possub)
      box()

      
    }
    
    plot(c(-2,2),c(-2,2), type="n", axes=FALSE, xlab="", ylab="")
    lines(c(0,0), c(-1, 1), lwd=2)
    lines(c(-0.1,0.1), c(-1, -1), lwd=2)
    lines(c(-0.1,0.1), c(1,1), lwd=2)
    text(0, 1.5, as.character(round(maxy/ampl,2)), cex=1.5)
    text(0, -1.5, as.character(round(-maxy/ampl, 2)), cex=1.5)
    lines(c(-1,1), c(0,0), lwd=2)
    lines(c(-1,-1), c(-0.1,0.1), lwd=2)
    lines(c(1,1), c(-0.1,0.1), lwd=2)

    text(1.5, 0, as.character(round(rx[2],2)), cex=1.5)
    text(-1.5, 0, as.character(round(rx[1], 2)), cex=1.5)
  }

      


################################ 5. Test de la marginalité

rotxy<-function (df)
{
  X<-scale(df[,2:3],scale=FALSE)
  angle<-runif(1,0,2*pi)
  co <- cos(angle)
  si <- sin(angle)
  Y<-as.data.frame(list(id=df[,1], x = co * X[,1] - si * X[,2], y = si *
                        X[,1] + co * X[,2]))
  Y[,2]<-Y[,2]+attr(X, "scaled:center")[1]
  Y[,3]<-Y[,3]+attr(X, "scaled:center")[2]
  return(Y)
}




plot.kselect<-function(x, xax=1, yax=2, ...)
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


## Récupère les attributs des xfrom et les place dans xto
getascattr<-function(xfrom, xto, type=c("numeric", "factor"), lev=NULL)
  {
    type<-match.arg(type)
    if (!inherits(xfrom, "asc"))
      stop("xfrom should be an asc object")
    if (mode(xto)=="logical") {
      mode(xto) <- "numeric"
      xto <- xto + 1
    }
    attr(xto, "xll")<-attr(xfrom, "xll")
    attr(xto, "yll")<-attr(xfrom, "yll")
    attr(xto, "cellsize")<-attr(xfrom, "cellsize")
    attr(xto, "type")<-type
    if (type=="factor") {
      if (is.null(lev))
        lev<-levels(factor(xto))
      attr(xto, "levels")<-lev
    }
    class(xto)<-"asc"
    return(xto)
  }


getkascattr<-function(xkfrom, xkto)
  {
    if (!inherits(xkfrom, "kasc"))
      stop("xkfrom should be a kasc object")
    attr(xkto, "xll")<-attr(xkfrom, "xll")
    attr(xkto, "yll")<-attr(xkfrom, "yll")
    attr(xkto, "cellsize")<-attr(xkfrom, "cellsize")
    attr(xkto, "nrow")<-attr(xkfrom, "nrow")
    attr(xkto, "ncol")<-attr(xkfrom, "ncol")
    class(xkto)<-c("kasc", "data.frame")
    return(xkto)
  }



join.asc<-function(pts, x)
  {
    if (!inherits(x, "asc")) stop("non convenient data")
    xy<-getXYcoords(x)
    xy$x<-xy$x+attr(x, "cellsize")/2
    xy$x<-c(xy$x, xy$x[1]-attr(x, "cellsize")/2)
    xy$y<-xy$y+attr(x, "cellsize")/2
    xy$y<-c(xy$y, xy$y[1]-attr(x, "cellsize")/2)

    xf<-as.numeric(cut(pts[,1], xy$x))
    yf<-as.numeric(cut(pts[,2], xy$y))

    fact<-0
    if (attr(x, "type")=="factor") 
      ct<-attr(x, "levels")

    for (i in 1:nrow(pts)) {
      if (attr(x, "type")=="numeric") {
        u<-x[xf[i],yf[i]]
        fact[i]<-u
      }
      if (attr(x, "type")=="factor") {
        u<-x[xf[i],yf[i]]
        tmp<-ct[u]
        if (length(tmp)==1) {
          fact[i]<-tmp
        } else {
          fact[i]<-NA
        }
      }
    }
    if (attr(x, "type")=="factor") fact<-factor(fact)
    return(fact)
  }

join.kasc<-function(pts, w)
  {
    x<-w
    if (!inherits(x, "kasc")) stop("non convenient data")
    sorties<-1:nrow(pts)
    for (i in 1:length(x)) {
      carp<-getkasc(x, names(x)[i])
      fac<-join.asc(pts, carp)
      sorties<-cbind.data.frame(sorties, fac)
    }
    sorties<-sorties[,-1]
    names(sorties)<-names(x)
    return(sorties)
  }



#####################################################################
#####################################################################
###
### Récupérer le polygone de contour d'un objet raster
getcontour<-function(x)
  {

    if (!inherits(x, "asc"))
      stop("should be an object of class asc")
    xyc<-getXYcoords(x)


    rajfond<-function(x)
      {
        nr<-nrow(x)
        nc<-ncol(x)
        
        f<-rep(0,nr)
        x<-cbind(f,x,f)
        f<-rep(0,nc+2)
        x<-rbind(f,x,f)
      }
    
    x[!is.na(x)]<-1
    x[is.na(x)]<-0

    x<-rajfond(x)
    toto<-.C("seqeticorr", as.double(t(x)), as.integer(nrow(x)),
             as.integer(ncol(x)), PACKAGE="adehabitat")
    etiquete<-matrix(toto[[1]], nrow=nrow(x), byrow=TRUE)
    etiquete<-etiquete[-c(1,nrow(etiquete)),-c(1,ncol(etiquete))]
    entree<-list()
    sorties<-c(0, 0, 0)
    lev<-levels(factor(toto[[1]]))
    lev<-lev[lev!="0"]
    for (i in lev) {
      j<-as.numeric(i)
      tmp<-etiquete
      tmp[tmp!=j]<-0
      tmp[tmp==j]<-1
      tmp<-rajfond(tmp)
      if (sum(as.vector(tmp))<3)
        stop("The parts of the object should contain at least 3 pixels")
      toto<-.C("lcontour", as.double(t(tmp)), as.integer(nrow(tmp)),
               as.integer(ncol(tmp)),  as.integer(0), PACKAGE="adehabitat")[[4]]
      pol<-.C("getcontour", as.double(t(tmp)), as.integer(nrow(tmp)),
              as.integer(ncol(tmp)), integer(toto), integer(toto),
              as.integer(toto), PACKAGE="adehabitat")
      xt<-c(0,xyc$x,0)
      yt<-c(0,xyc$y,0)
      x<-xt[pol[[4]]]
      y<-yt[pol[[5]]]
      sorties<-rbind(sorties, cbind(rep(j,length(x)), x, y))
    }
    sorties<-sorties[-1,]
    row.names(sorties)<-1:nrow(sorties)
    sorties<-as.data.frame(sorties)
    sorties[,1]<-factor(sorties[,1])
    names(sorties)<-c("id","x", "y")
    sorties<-as.area(sorties)
    return(sorties)
  }


###########################################################
###########################################################
###
### ascgen est un générateur d'objets asc

ascgen<-function(xy=NULL, cellsize=NULL, nrcol=10, count=TRUE)
  {
    xl<-c(min(xy[,1]), max(xy[,1]))
    yl<-c(min(xy[,2]), max(xy[,2]))
    rx<-xl[2]-xl[1]
    ry<-yl[2]-yl[1]
    u<-rx
    ref<-"x"
    if (ry>rx) {
      u<-ry
      ref<-"y"
    }
    xll<-xl[1]
    yll<-yl[1]

    if (!is.null(cellsize)) {
      cx<-ceiling(rx/cellsize)+1
      cy<-ceiling(ry/cellsize)+1
      asc<-matrix(0, nrow=cx, ncol=cy)
      attr(asc, "xll")<-xll
      attr(asc, "yll")<-yll
      attr(asc, "cellsize")<-cellsize
      attr(asc, "type")<-"numeric"
      class(asc)<-"asc"
    } else {
      asc<-matrix(0, nrow=nrcol, ncol=nrcol)
      cellsize<-u/(nrcol-1)
      attr(asc, "xll")<-xll
      attr(asc, "yll")<-yll
      attr(asc, "cellsize")<-cellsize
      attr(asc, "type")<-"numeric"
      class(asc)<-"asc"
    }
    
    if (count) {
      kasc<-as.kasc(list(a=asc))
      asc<-count.points(xy, kasc)
    }

    return(asc)
  }


rand.kselect<-function(dudi, factor, weight, nrep=200, alpha=0.05, ewa = FALSE)
  {
    if (!inherits(dudi, "dudi")) 
      stop("Object of class dudi expected")
    if (nrow(dudi$tab) != length(factor)) 
      stop("The factor should have the same length as the dudi object")
    if (nrow(dudi$tab) != length(weight)) 
      stop("The vector of weights should have the same length as the dudi object")
    if (!is.vector(weight)) 
        stop("The weights should be placed in a vector")

    tab<-as.matrix(dudi$tab)
    fac<-as.numeric(factor)
    poids<-as.numeric(weight)
    col1<-ncol(dudi$tab)
    lig1<-nrow(dudi$tab)
    lev1<-nlevels(factor)
    nombreani<-tapply(dudi$tab[,1], factor, length)

    res<-.C("permutksel", as.double(t(tab)), as.integer(fac), as.double(poids),
            as.integer(col1), as.integer(lev1), as.integer(lig1),
            double(col1*lev1), double(col1*lev1), double(col1*lev1),
            as.integer(nombreani), as.integer(nrep), double(1),
            double(nrep), double(lev1), double(lev1*nrep), double(col1),
            double(nrep*col1*lev1), as.double(dudi$cw), as.integer(ewa),
            PACKAGE="adehabitat")

    names(res)<-c("tab", "fac", "weight", "nh", "na",
                  "nl", "use", "ava", "mar",
                  "nbani", "npermut", "obseig",
                  "simeig", "obsmarg", "simmarg", "eigenvp",
                  "simtout", "poco", "ewa")


    ## Tableau de marginalité observé
    obsval<-matrix(res$mar, ncol=lev1)
    margs<-matrix(res$simtout, nrow=nrep, byrow=FALSE)
    pval1<-obsval
    pval2<-obsval
    pval<-obsval
    sign<-obsval

    for (i in 1:nrow(pval)) {
      for (j in 1:ncol(pval)) {
        k<-(i-1)*lev1+j
        pval1[i,j]<-as.randtest(margs[,k], obsval[i,j])$pvalue
        pval2[i,j]<-as.randtest(-margs[,k], -obsval[i,j])$pvalue
      }
    }

    for (i in 1:nrow(pval)) {
      for (j in 1:ncol(pval)) {
        pval[i,j]<-min(c(pval1[i,j], pval2[i,j]))
      }
    }

    for (i in 1:nrow(pval)) {
      for (j in 1:ncol(pval)) {
        if (obsval[i,j]>=0) {
          sign[i,j]<-"+"
          if (pval[i,j]<(alpha/(2*nrow(pval)*ncol(pval))))
            sign[i,j]<-"+++"
        } else {
          sign[i,j]<-"-"
          if (pval[i,j]<(alpha/(2*nrow(pval)*ncol(pval))))
            sign[i,j]<-"---"
        }
      }
    }
    
    colnames(pval)<-levels(factor)
    row.names(pval)<-colnames(tab)
    colnames(obsval)<-levels(factor)
    row.names(obsval)<-colnames(tab)
    colnames(sign)<-levels(factor)
    row.names(sign)<-colnames(tab)

    o<-matrix(res$simmarg, ncol=lev1, byrow=FALSE)
    mat<-matrix(0, nrow=lev1, ncol=2)
    colnames(mat)<-c("observed", "pvalue")
    mat<-as.data.frame(mat)
    for (i in 1:lev1) {
      mat[i,1]<-res$obsmarg[i]
      tmp<-as.randtest(o[,i], res$obsmarg[i])
      mat[i,2]<-tmp$pvalue
    }
    row.names(mat)<-levels(factor)
    global<-c(0,0)
    names(global)<-c("observed","pvalue")
    global[1]<-res$obseig
    global[2]<-as.randtest(res$simeig, res$obseig)$pvalue
    lili<-list(obsval=obsval, pvalue=pval, signification=sign)
    so<-list(global=global, marg=mat, per.ind=lili, alpha=alpha)
    class(so)<-"rand.kselect"
    return(so)
  }

print.rand.kselect<-function(x, ...)
  {
    cat("****** Randomization tests of the k-select analysis ******\n\n")
    cat("Test of the first eigenvalue:\n")
    cat("Observed value:", x$global[1], "\n")
    cat("P-value :", x$global[2], "\n")
    cat("\n")
    cat("Test of the marginality of each individual\n(to be compared with bonferroni alpha level:",
        x$alpha/nrow(x$marg),"):\n\n")
    print(x$marg, ...)
    cat("\nSign of the mean for each animal and each variable: \n")
    cat("(when significant, the sign is tripled) \n\n")
    print(x$per.ind$signification, quote=FALSE)
    cat("\n\nOther elements of the list $per.ind:")
    cat("\n  $obsval: mean of variables for each animal")
    cat("\n  $pvalue: P-value of the means in $obsval\n\n")
  }



setmask<-function(x, mask)
  {
    if ((!inherits(x, "asc"))&(!inherits(x, "kasc")))
      stop("x should be an object of class \"asc\" or \"kasc\"")
    if (!inherits(mask, "asc"))
      stop("mask should be of class \"asc\"")
    
    if (attr(x, "xll")!=attr(mask, "xll"))
      stop("Objects should have the same xll attribute")
    if (attr(x, "yll")!=attr(mask, "yll"))
      stop("Objects should have the same yll attribute")
    if (attr(x, "cellsize")!=attr(mask, "cellsize"))
      stop("Objects should have the same cellsize attribute")
    
    if (inherits(x, "kasc")) {
      if (attr(x, "nrow")!=ncol(mask))
        stop("Maps should have the same number of columns")
      if (attr(x, "ncol")!=nrow(mask))
        stop("Maps should have the same number of rows")
      x$mask0012<-as.vector(mask)
      so<-managNAkasc(x)
      so$mask0012<-NULL
      sorties<-so
    } else {
      if (nrow(x)!=nrow(mask))
        stop("Objects should have the same attributes")
      if (ncol(x)!=ncol(mask))
        stop("Objects should have the same attributes")
      u<-as.kasc(list(x=x, mas=mask))
      so<-managNAkasc(u)
      sorties<-getkasc(so, "x")
    }
    return(sorties)
  }



######################################################################
######################################################################
######################################################################
#####
##### sahrlocs2kselect = preparation of the kselect analysis


sahrlocs2kselect<-function(sahr)
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





#######################################################
#######
####### Wi pour design I


profilehab<-function(rankma, wi)
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


widesII<-function(u, a, avknown=TRUE, alpha=0.05)
  {
    u<-as.matrix(u)
    if (ncol(u)!=length(a))
      stop("used and available matrices should have the same number of habitats")
    uij<-as.matrix(u)
    ai<-a
    pi<-ai/sum(ai)
    n<-nrow(uij)
    I<-ncol(uij)
    uip<-apply(uij,2,sum)
    upj<-apply(uij,1,sum)
    upp<-sum(as.vector(uij))
    oi<-uip/upp
    sorties<-list()
    sorties$used.prop <- oi
    sorties$avail.prop <- pi

    
    ## Calcul de Khi2L1
    Euij<-outer(upj,uip)/upp
    tmp<-log(uij/Euij)
    tmp[abs(tmp)==Inf]<-0
    tmp[is.na(tmp)]<-0
    Khi2L1<-c(tmp<-2*sum(as.vector(uij*tmp)), df<-(I-1)*(n-1),
               1 - pchisq(tmp, df))
    names(Khi2L1)<-c("Khi2L1", "df", "pvalue")
    sorties$Khi2L1<-Khi2L1
    
    ## Calcul de Khi2L2
    if (avknown) {
      Euij<-outer(upj,pi)
      tmp<-log(uij/Euij)
      tmp[abs(tmp)==Inf]<-0
      Khi2L2<-c(tmp<-2*sum(as.vector(uij*tmp)), df<-(I-1)*n,
                1 - pchisq(tmp, df))
      names(Khi2L2)<-c("Khi2L2", "df", "pvalue")
      sorties$Khi2L2<-Khi2L2
      
      ## Calcul de la différence
      Khi2L2MinusL1<-c(tmp<-Khi2L2[1]-Khi2L1[1], I-1,
                       1 - pchisq(tmp, I-1))
      names(Khi2L2MinusL1)<-c("Khi2L2MinusL1", "df", "pvalue")
      sorties$Khi2L2MinusL1<-Khi2L2MinusL1
    }
    else {
      uija<-rbind(uij,ai)
      upja<-apply(uija, 1, sum)
      uipa<-apply(uija, 2, sum)
      Euija<-outer(upja,uipa)/sum(upja)
      tmp<-log(uija/Euija)
      tmp[abs(tmp)==Inf]<-0
      Khi2L2<-c(tmp<-2*sum(as.vector(uija*tmp)), df<-(I-1)*n,
                1 - pchisq(tmp, df))
      names(Khi2L2)<-c("TrickyKhi2", "df", "pvalue")
      sorties$Khi2L2<-Khi2L2
      ## Calcul de la différence
      Khi2L2MinusL1<-c(tmp<-Khi2L2[1]-Khi2L1[1], I-1,
                       1 - pchisq(tmp, I-1))
      names(Khi2L2MinusL1)<-c("Khi2L2MinusL1", "df", "pvalue")
      sorties$Khi2L2MinusL1<-Khi2L2MinusL1

    }
    
    ## Matrice des wi
    wij<-t(t(uij/upj)/pi)
    wi<-(uip/upp)/pi
    sorties$wij<-wij
    sorties$wi<-wi

    ## Calcul de la variance des wi
    if (avknown) {
      varwi<-apply((((t(t(uij)/pi) - outer(upj,wi) )^2)/(n-1)),
                   2, sum)*(n/(upp^2))
      sewi<-sqrt(varwi)
    }
    else {
      Vi<-uip/upp
      varVi<-Vi
      for (i in 1:length(Vi)) {
        varVi[i]<-(sum((u[,i]-Vi[i]*upj)^2 )/(n-1))/(n*(mean(upj)^2))
      }
      varpi<-pi*(1-pi)/sum(ai)
      sewi<-sqrt(((Vi/pi)^2)*(varVi/(Vi^2)+varpi/(pi^2)))
    }
    sorties$se.wi<-sewi
    sorties$ICwiupper<-round(wi+sewi*qnorm(1 - alpha/(2*I)), 4)
    sorties$ICwilower<-round(wi-sewi*qnorm(1 - alpha/(2*I)), 4)

    
    ## calcul des SE des différences des wi
    diffwi<-outer(wi,wi,"-")
    sediffwi<-diffwi

    if (avknown) {
      for (i in 1:I) {
        for (j in 1:I) {
          tmp<-uij[,i]/pi[i] - uij[,j]/pi[j] - wi[i]*upj + wi[j]*upj
          sediffwi[i,j]<-sqrt(((n/(n-1))/(upp^2))*sum(tmp^2))
        }
      }
    }
    else {
      for (i in 1:I) {
        for (j in 1:I) {
          tmp<-(sum((uij[,i]/pi[i]-uij[,j]/pi[j]-diffwi[i,j]*upj)^2)/(n-1))*(n/(upp^2))
          tmp<-tmp+((wi[i]^2)/pi[i]+(wi[j]^2)/pi[j]-(diffwi[i,j]^2))/sum(ai)
          sediffwi[i,j]<-sqrt(tmp)
        }
      }
    }
    ## Calcul des IC
    ## pour les différences de wi
    bonferroni <- alpha/(I * (I - 1)/2)
    ICdiffupper<-round(diffwi+sediffwi*qnorm(1 - bonferroni/2), 4)
    ICdifflower<-round(diffwi-sediffwi*qnorm(1 - bonferroni/2), 4)

    ## ranking matrix
    sig<-diffwi
    for (i in 1:I) {
      for (j in 1:I) {
        if (i!=j) {
          sig[i, j] <- ifelse(diffwi[i, j] < 0, "-", "+")
          if (ICdiffupper[i, j] < 0) 
            sig[i, j] <- "---"
          if (ICdifflower[i, j] > 0) 
            sig[i, j] <- "+++"
        }
        else {
          sig[i,j]<-"0"
        }
      }
    }

    ## sorties
    rownames(diffwi) <- colnames(u)
    colnames(diffwi) <- colnames(u)
    rownames(ICdiffupper) <- colnames(u)
    colnames(ICdiffupper) <- colnames(u)
    rownames(ICdifflower) <- colnames(u)
    colnames(ICdifflower) <- colnames(u)
    rownames(sig) <- colnames(u)
    colnames(sig) <- colnames(u)
    sorties$avknown <- avknown
    sorties$comparisons$diffwi <- diffwi
    sorties$comparisons$ICdiffupper <- ICdiffupper
    sorties$comparisons$ICdifflower <- ICdifflower
    sorties$comparisons$signif <- sig
    sorties$profile <- profilehab(sig, wi)
    sorties$alpha <- alpha
    class(sorties) <- c("wiII", "wi")
    return(sorties)
  }










print.wiII<-function(x, ...)
  {
    if (!inherits(x,"wiII"))
      stop("x should be of class \"wiII\"")
    cat("\n\n************** Manly's Selection ratios for design II ********\n\n")
    cat("1. Test of identical use of habitat by all animals\n")
    cat("   (Classical Khi² performed on the used matrix):\n")
    print(x$Khi2L1)

    cat("2. Test of overall habitat selection:\n")
    print(x$Khi2L2)
    cat("3. Test of hypothesis that animals are on average using resources\n")
    cat("   in proportion to availability, irrespective of whether they are\n")
    cat("   the same or not (Khi2L2 - Khi2L1):\n")
    print(x$Khi2L2MinusL1)
    cat("\n\nTable of selection ratios:\n")
    print(data.frame(Available=x$avail.prop, Used=x$used.prop, Wi=x$wi,
                     SE=x$se.wi, IClower=x$ICwilower, ICupper=x$ICwiupper), ...)
    cat("\n\nBonferroni classement \nBased on", (1 - x$alpha) * 
        100, "% confidence intervals on the differences of Wi :\n")
    print(x$profile, quote = FALSE)
    cat("\n")
}

 
    
    




widesI<-function(u, a, avknown=TRUE, alpha=0.05)
  {
    if (length(u)!=length(a))
      stop("available and used vector should have the same length")
    if (is.null(names(u)))
      names(u)<-paste("Habitat", 1:length(u), sep="")
    sorties<-list()
    ui<-u
    ai<-a
    oi<-ui/sum(ui)
    pi<-ai/sum(ai)
    I<-length(u)
    wi<-oi/pi
    bonferroni<-alpha/(I*(I-1)/2)


    sorties$used.prop<-oi
    sorties$se.used<-sqrt((oi*(1-oi)/sum(ui)))
    sorties$avail.prop<-pi
    sorties$se.avail<-sqrt(pi*(1-pi)/sum(a))
    sorties$wi<-wi
    if (avknown) {
      sorties$se.wi<-sqrt(oi*(1-oi)/(sum(u)*(pi^2)))
    } else {
      sorties$se.wi<-wi*sqrt(1/ui-1/sum(ui)+1/ai-1/sum(ai))
    }
    
    testwi<-((sorties$wi-1)/sorties$se.wi)^2
    sorties$chisquwi<-data.frame(testwi=testwi,
                                 p=1-pchisq(testwi, 1))
    sorties$Bi<-sorties$wi/sum(sorties$wi)
    if (avknown) {
      sorties$Khi2P<-c(tmp<-sum(((ui-sum(u)*pi)^2)/(sum(u)*pi)),
                       length(u)-1,
                       1-pchisq(tmp, I-1))
      tmp<-u*log(u/(sum(u)*pi))
      tmp[is.na(tmp)]<-0
      sorties$Khi2L<-c(tmp<-2*sum(tmp),
                       length(u)-1,
                       1-pchisq(tmp, I-1))
    } else {
      Eui<-(ai+ui)*sum(ui)/(sum(u)+sum(ai))
      Eai<-(ai+ui)*sum(ai)/(sum(u)+sum(ai))
      sorties$Khi2P<-c(tmp<-sum(((ui-Eui)^2)/Eui + ((ai-Eai)^2)/Eai),
                       length(u)-1,
                       1-pchisq(tmp, I-1))
      sorties$Khi2L<-c(tmp<-2*sum(u*log(u/Eui)+ai*log(ai/Eai)),
                       length(u)-1,
                       1-pchisq(tmp, I-1))
    }
    
    names(sorties$Khi2P)<-c("Khi2P", "df", "pvalue")
    names(sorties$Khi2L)<-c("Khi2L", "df", "pvalue")
    diffwi<-matrix(0, nrow=I, ncol=I)
    vardif<-matrix(0, nrow=I, ncol=I)
    sig<-matrix(0, nrow=I, ncol=I)
    ICdiffupper<-matrix(0, nrow=I, ncol=I)
    ICdifflower<-matrix(0, nrow=I, ncol=I)
    sig<-matrix(0, nrow=I, ncol=I)
    
    for (i in 1:I) {
      for (j in 1:I) {
        if (i!=j) { 
          vardif[i,j]<-ifelse(avknown,
                              (oi[i]*(1-oi[i])/(sum(ui)*(pi[i]^2))+
                               oi[j]*(1-oi[j])/(sum(ui)*(pi[j]^2))-
                               2*oi[i]*oi[j]/(sum(ui)*pi[i]*pi[j])),
                              (wi[i]/pi[i]+wi[j]/pi[j]-((wi[i]-wi[j])^2))/sum(ui)+
                              ((wi[i]^2)/pi[i]+(wi[j]^2)/pi[j]-((wi[i]-wi[j])^2))/sum(ai))
          diffwi[i,j]<-(wi[i]-wi[j])
          ICdiffupper[i,j]<-round(diffwi[i,j]+sqrt(vardif[i,j])*qnorm(1-bonferroni/2),4)
          ICdifflower[i,j]<-round(diffwi[i,j]-sqrt(vardif[i,j])*qnorm(1-bonferroni/2),4)
          if (diffwi[i,j]<0) sig[i,j] <- "-"
          if (diffwi[i,j]>0) sig[i,j] <- "+"
          if (diffwi[i,j]==0) sig[i,j] <- "0"
          if (ICdiffupper[i,j]<0)
            sig[i,j]<-"---"
          if (ICdifflower[i,j]>0)
            sig[i,j]<-"+++"
        } else {
          sig[i,j]<-"0"
        }
      }
    }        
    
    rownames(diffwi)<-names(u)
    colnames(diffwi)<-names(u)
    rownames(ICdiffupper)<-names(u)
    colnames(ICdiffupper)<-names(u)
    rownames(ICdifflower)<-names(u)
    colnames(ICdifflower)<-names(u)
    rownames(sig)<-names(u)
    colnames(sig)<-names(u)

    sorties$avknown<-avknown
    sorties$comparisons$diffwi<-diffwi
    sorties$comparisons$ICdiffupper<-ICdiffupper
    sorties$comparisons$ICdifflower<-ICdifflower
    sorties$comparisons$signif<-sig
    sorties$profile<-profilehab(sig, wi)
    sorties$alpha<-alpha
    class(sorties)<-c("wiI", "wi")
    return(sorties)
  }



print.wiI<-function(x, ...)
  {
    cat("\n\n************** Manly's Selection ratios for design I ********\n\n")
    cat("Significance of habitat selection:\n")
    print(x$Khi2L)
    cat("\n\nTable of ratios (p-values should be\n",
        "compared with Bonferroni level=",
        x$alpha/length(x$used.prop),")\n")
    n<-length(x$used.prop)
    z<-qnorm(1-x$alpha/(2*n))
    df<-data.frame(used=x$used.prop, 
                   avail=x$avail.prop, Wi=x$wi,
                   SE.Wi=x$se.wi, P=x$chisquwi[,2], Bi=x$Bi)
    df<-round(as.matrix(df),3)
    print(df, ...)
    cat("\n\nBonferroni classement \nBased on",(1-x$alpha)*100,
        "% confidence intervals on the differences of Wi :\n")
    print(x$profile, quote=FALSE)
    cat("\n")
  }


plot.wi<-function(x, caxis=0.7, clab=1, ylog=FALSE, errbar=c("CI", "SE"),
                  main="Manly selectivity measure", noorder = TRUE, ...)
  {
    errbar<-match.arg(errbar)
    opar<-par(ask=TRUE)
    on.exit(par(opar))
    if (!inherits(x, "wi"))
      stop("x should be of class \wi")
    eb<-ifelse(errbar=="SE", 1, abs(qnorm(x$alpha/length(x$wi))) )
      
    ## plot de wi
    if (noorder)
      wi<-sort(x$wi, decreasing=TRUE)
    else
      wi<-x$wi
    
    if ((any(wi==0))&(ylog)) {
      warning("zero values in x, ylog has been set to FALSE")
      ylog<-FALSE
    }
    logy<-ifelse(ylog, "y", "")

    if (noorder)
      sewi<-x$se.wi[order(x$wi, decreasing=TRUE)]
    else
      sewi<-x$se.wi
    sewi[is.na(sewi)]<-0
    nwi<-names(wi)
    rgy<-range(c(wi, wi+eb*sewi, wi-eb*sewi))
    textleg<-paste("Selection ratios (+/-", errbar,")")
    if (inherits(x, "wiII")|inherits(x,"wiIII"))
      textleg<-paste("Global Selection ratios (+/-", errbar,")")
    if (!ylog) rgy[1]<-0
    plot(wi, axes=FALSE, ylim=rgy, ty="n", xlab="",
         ylab=textleg,
         cex.lab=clab, log=logy, main=main, ...)
    axis(side=1, at=c(1:length(wi)), labels=names(wi), cex.axis=caxis, las=2)
    axis(side=2, cex.axis=caxis)
    box()
    points(c(1:length(wi)), wi, pch=16)
    lines(1:length(wi), wi)
    abline(h=1, lwd=2)
    for (i in 1:length(wi)) {
      lines(c(i,i), c(wi[i]-eb*sewi[i], wi[i]+eb*sewi[i]))
      lines(c(i-0.1,i+0.1), c(wi[i]-eb*sewi[i], wi[i]-eb*sewi[i]))
      lines(c(i-0.1,i+0.1), c(wi[i]+eb*sewi[i], wi[i]+eb*sewi[i]))
    }
    
    ## plot de Bi
    if (inherits(x, "wiI")) {
      if (noorder)
        Bi<-x$Bi[order(x$wi, decreasing=TRUE)]
      else
        Bi<-x$Bi
      plot(Bi, axes=FALSE, ty="n", xlab="", cex.lab=clab, main="Scaled selection ratios",...)
      axis(side=1, at=c(1:length(wi)), labels=names(wi), cex.axis=caxis, las=2)
      axis(side=2, cex.axis=caxis)
      lines(1:length(wi), Bi)
      points(c(1:length(wi)), Bi, pch=16)
      box()
        
      ## plot de utilisé et disponible
      if (noorder) {
        ut<-x$used.prop[order(x$wi, decreasing=TRUE)]
        seu<-x$se.used[order(x$wi, decreasing=TRUE)]
        sea<-x$se.avail[order(x$wi, decreasing=TRUE)]
        av<-x$avail.prop[order(x$wi, decreasing=TRUE)]
      }
      else {
        ut<-x$used.prop
        seu<-x$se.used
        sea<-x$se.avail
        av<-x$avail.prop
      }
      rgy<-range(c(av, ut-eb*seu, ut+eb*seu, av-eb*sea, av+eb*sea))
      rgy<-c(rgy[1], rgy[2]+(rgy[2]-rgy[1])/4)
      plot(ut, axes=FALSE, ty="n", xlab="", cex.lab=clab, ylim=rgy,
           main="Used and available proportions",
           ylab=paste("Porportion (+/-", errbar,")"),...)
      points(1:length(wi)-0.05, av, pch=16)
      points(1:length(wi)+0.05, ut, pch=2)
      for (i in 1:length(wi)) {
          lines(c(i,i)+0.05, c(ut[i]-eb*seu[i], ut[i]+eb*seu[i]))
          lines(c(i-0.02,i+0.02)+0.05, c(ut[i]-eb*seu[i], ut[i]-eb*seu[i]))
          lines(c(i-0.02,i+0.02)+0.05, c(ut[i]+eb*seu[i], ut[i]+eb*seu[i]))
        }
      if (!x$avknown) {
        for (i in 1:length(wi)) {
          lines(c(i,i)-0.05, c(av[i]-eb*sea[i], av[i]+eb*sea[i]))
          lines(c(i-0.02,i+0.02)-0.05, c(av[i]-eb*sea[i], av[i]-eb*sea[i]))
          lines(c(i-0.02,i+0.02)-0.05, c(av[i]+eb*sea[i], av[i]+eb*sea[i]))
        }
      }
        
      axis(side=1, at=c(1:length(wi)), labels=names(wi), cex.axis=caxis, las=2)
      axis(side=2, cex.axis=caxis)
      box()
      legend(1,rgy[2], c("Available", "Used"), pch=c(16,2), cex=clab)
    }
    else {
      if (noorder)
        wij<-x$wij[,order(x$wi, decreasing=TRUE)]
      else
        wij<-x$wij
      iii<-as.vector(wij)
      rgy<-range(iii[!is.na(iii)])
      plot(1, ty="n", ylim=rgy, xlim=c(1,ncol(wij)), xlab="",
           ylab=paste("Selection ratios"),
           cex.lab=clab, log=logy, axes=FALSE,
           main=main, ...)
      axis(side=1, at=c(1:length(wi)), labels=names(wi), cex.axis=caxis, las=2)
      axis(side=2, cex.axis=caxis)
      box()
      pt<-seq(-0.1, 0.1, by=0.2/nrow(wij))
      
      for (j in 1:nrow(wij)) {
        points(c(1:length(wi)), wij[j,], pch=16, col=j)
        lines(1:length(wi), wij[j,], col=j)
        abline(h=1, lwd=2)
      }
      rgx<-ncol(wij)/5
      legend(ncol(wij)-rgx, rgy[1]+19*(rgy[2]-rgy[1])/20,
             legend=row.names(wij), pch=16, col=1:nrow(wij),
             lwd=1, cex=clab)
    }
  }


####################################################################
###
### DV par la méthode kernel




kernelUD<-function(xy, id=NULL, h="href", grid=40, same4all=FALSE,
                   hlim=c(0.1, 1.5), kern = "bivnorm")
  {
    if (ncol(xy)!=2)
      stop("xy should have 2 columns")
    if ((!is.null(id))&(length(id)!=nrow(xy)))
      stop("id should have the same length as xy")
    if ((!is.numeric(h))&(h!="href")&(h!="LSCV"))
      stop("h should be numeric or equal to either \"href\" or \"LSCV\"")
    if ((h == "LSCV")&(kern == "epa"))
      stop("LSCV is not implemented with an Epanechnikov kernel")
    if (is.null(id))
      id<-rep(1, nrow(xy))
    id<-factor(id)
      if (min(table(id))<5)
    stop("At least 5 relocations are required to fit an home range")


    ## split de xy
    lixy<-split(xy, id)
    sorties<-list()
    typh<-h
    htmp<-h
    gr<-grid

    ## 
    if (same4all) {
      if (length(as.vector(gr))==1) {
        if (!is.numeric(gr))
          stop("grid should be an object of class asc or a number")
        xli<-range(xy[,1])
        yli<-range(xy[,2])
        xli<-c(xli[1]-0.3*abs(xli[2]-xli[1]),xli[2]+0.3*abs(xli[2]-xli[1]))
        yli<-c(yli[1]-0.3*abs(yli[2]-yli[1]),yli[2]+0.3*abs(yli[2]-yli[1]))
        xygg<-data.frame(x=xli, y=yli)
        grid<-ascgen(xygg, nrcol=grid)
        cellsize<-attr(grid, "cellsize")
        lx<-nrow(grid)*cellsize
        ly<-ncol(grid)*cellsize
        ref<-lx
        if (ly>lx)
          ref<-ly
        xll<-attr(grid, "xll")
        yll<-attr(grid, "yll")
        
        ## On rajoute des colonnes et des lignes
        xll<-xll-lx/2
        yll<-yll-ly/2
        arajlig<-ceiling((lx/2)/cellsize)
        arajcol<-ceiling((ly/2)/cellsize)
        mrajlig<-matrix(0, ncol=ncol(grid), nrow=arajlig)
        grid<-rbind(mrajlig, grid, mrajlig)
        mrajcol<-matrix(0, ncol=arajcol, nrow=nrow(grid))
        grid<-cbind(mrajcol, grid, mrajcol)
        
        ## rajout des attributs
        attr(grid, "xll")<-xll
        attr(grid, "yll")<-yll
        attr(grid, "cellsize")<-cellsize
        attr(grid, "type")<-"numeric"
        class(grid)<-"asc"
      }
    }
      
    ## Boucle estimation UD pour chaque ani
    for (i in 1:nlevels(id)) {
      df<-lixy[[i]]
      
      ## 1. Calcul de h
      varx<-var(df[,1])
      vary<-var(df[,2])
      sdxy<-sqrt(0.5*(varx+vary))
      n<-nrow(df)
      ex<-(-1/6)
      href<-sdxy*(n^ex)
      if (kern=="epa")
        href <- href*1.77
      
      if (h=="href") {
        htmp<-href
      }
      if (h=="LSCV") {
        hvec<-seq(hlim[1]*href, hlim[2]*href, length=100)
        CV<-.C("CVmise", as.integer(nrow(df)), as.double(df[,1]),
               as.double(df[,2]),
               as.double(hvec), double(length(hvec)),
               as.integer(length(hvec)), PACKAGE="adehabitat")[[5]]
        htmp<-hvec[CV==min(CV)]
        if ((CV[CV==min(CV)]==CV[1])|(CV[CV==min(CV)]==CV[length(CV)]))
          warning("The algorithm did not converge \nwithin the specified range of hlim: try to increase it")
      }
      
      
      ## 3. Construction de la grille
      if (length(as.vector(gr))==1) {
        if (!is.numeric(gr))
          stop("grid should be an object of class asc or a number")

        if (!same4all) {
          grid<-matrix(0, ncol=gr, nrow=gr)
          rgx<-range(df[,1])
          rgy<-range(df[,2])
          lx<-rgx[2]-rgx[1]
          ly<-rgy[2]-rgy[1]
          ref<-lx
          if (ly>lx)
            ref<-ly
          
          xll<-rgx[1]
          yll<-rgy[1]
          cellsize<-ref/ncol(grid)
          
          ## On rajoute des colonnes et des lignes
          xll<-xll-lx/2
          yll<-yll-ly/2
          arajlig<-ceiling((lx/2)/cellsize)
          arajcol<-ceiling((ly/2)/cellsize)
          mrajlig<-matrix(0, ncol=ncol(grid), nrow=arajlig)
          grid<-rbind(mrajlig, grid, mrajlig)
          mrajcol<-matrix(0, ncol=arajcol, nrow=nrow(grid))
          grid<-cbind(mrajcol, grid, mrajcol)
          
          ## rajout des attributs
          attr(grid, "xll")<-xll
          attr(grid, "yll")<-yll
          attr(grid, "cellsize")<-cellsize
          attr(grid, "type")<-"numeric"
          class(grid)<-"asc"
        }
      }
      grille<-grid
      xylo<-getXYcoords(grid)
      xg<-xylo$x
      yg<-xylo$y

      if (kern=="bivnorm") {
        toto<-.C("kernelhr", double(nrow(grid)*ncol(grid)),as.double(xg),
                 as.double(yg),
                 as.integer(ncol(grid)), as.integer(nrow(grid)),
                 as.integer(nrow(df)), as.double(htmp),
                 as.double(df[,1]), as.double(df[,2]), PACKAGE="adehabitat")
      }
      if (kern=="epa") {
        toto<-.C("kernepan", double(nrow(grid)*ncol(grid)),as.double(xg),
                 as.double(yg),
                 as.integer(ncol(grid)), as.integer(nrow(grid)),
                 as.integer(nrow(df)), as.double(htmp),
                 as.double(df[,1]), as.double(df[,2]), PACKAGE="adehabitat")
      }
      UD<-matrix(toto[[1]], nrow=nrow(grid), byrow=TRUE)
      UD<-getascattr(grid, UD)
      if (typh=="LSCV") {
        CV<-data.frame(h=hvec, CV=CV)
        convergence<-min(CV[,2])!=CV[1,2]
        htmp<-list(CV=CV, convergence=convergence, h=htmp)
      }
      sorties[[names(lixy)[i]]]<-list(UD=UD, h=htmp, locs=df, hmeth=typh)
    }
    class(sorties)<-c("khrud", "khr")
    return(sorties)
  }





print.khr<-function(x, ...)
  {
    if (!inherits(x, "khr"))
      stop("x should be an object of class khr")
    cat("********** Utilization distribution of Animals ************\n\n")
    if (inherits(x, "khrud"))
      cat("Type: probability density\n")
    if (inherits(x, "kbbhrud"))
      cat("Type: probability density estimated with the Brownian bridge approach\n")
    if (inherits(x, "khrvol"))
      cat("Type: volume under UD (only used to compute home ranges)\n")
    cat("\nUD have been estimated using the kernel method for the following animals:\n")
    
    print(names(x), quote=FALSE)
    th<-x[[1]]$hmeth
    if (th=="LSCV")
      cat("\nThe smoothing parameter was estimated by cross validation\n")
    if (th=="href")
      cat("\nThe smoothing parameter was estimated by the reference method (ad hoc)\n")
    if (is.numeric(th))
      cat("\nThe smoothing parameter was set to", th, "\n")
    
    cat("\nEach animal is a component of the list, and for each animal,\n")
    cat("the following elements are available:\n")
    cat("$UD       The utilization distribution (object of class \"asc\")\n")
    cat("$locs     The relocations of the animal\n")
    if (th=="LSCV") {
      cat("$h        A list with the following components:\n")
      cat("          $CV   The results of cross-validation\n")
      cat("          $h    The value of the smoothing parameter\n")
    }
    if (th=="href") {
      cat("$h        The value of the smoothing parameter\n")
    }
    if (th=="bb") {
      cat("$h        The values of the smoothing parameters\n")
    }

    if (th=="LSCV") {
      m<-0
      for (i in 1:length(x))
        m[i]<-x[[i]]$h$convergence
      names(m)<-names(x)
      if (!all(m)) {
        cat("\nWARNING!! No convergence in cross-validation for the following animals:\n")
        print(names(m)[!m], quote=FALSE)
        cat("Consider a new fit of UD using the ad hoc method for h.\n")
      }
    }
  }


image.khr<-function(x, axes=FALSE, mar=c(0,0,2,0),
                    addcontour=TRUE, addpoints=TRUE,...)
  {
    if (!inherits(x, "khr"))
      stop("x should be an object of class \"khr\"")
    if ((inherits(x,"khrud"))|(inherits(x,"kbbhrud")))
      col<-gray((256:1)/256)
    if (inherits(x,"khrvol"))
      col<-gray((1:256)/256)
    
    if (length(x) > 1) {
      opar<-par(mfrow=n2mfrow(length(x)), mar=mar)
      on.exit(par(opar))
    }
    
    for (i in 1:length(x)) {
      if (length(x)>1)
        image(x[[i]]$UD, main=names(x)[i], axes=axes, col=col, ...)
      if (length(x)==1)
        image(x[[i]]$UD, axes=axes, col=col, ...)
      if (addcontour)
        contour(x[[i]]$UD, add=TRUE)
      if (addpoints) {
        points(x[[i]]$locs, pch=21, col="black", bg="white")
      }
      box()
    }
  }

plotLSCV<-function(x)
  {
    if (!inherits(x, "khrud"))
      stop("x should be an object of class \"khrud\"")
    opar<-par(mfrow=n2mfrow(length(x)))
    for (i in 1:length(x)) {
      plot(x[[i]]$h$CV[,1], x[[i]]$h$CV[,2], pch=16, main=names(x)[i],
           xlab="h parameter", ylab="CV(h)", cex=0.5)
      lines(x[[i]]$h$CV[,1], x[[i]]$h$CV[,2])
    }
    par(opar)
  }


getvolumeUD<-function(x)
  {
    if ((!inherits(x, "khrud"))&(!inherits(x, "kbbhrud")))
      stop("x should be an object of class \"khrud\" or \"kbbhrud\"")

    for (i in 1:length(x)) {
      asc<-x[[i]]$UD
      cs<-attr(asc,"cellsize")
      v<-.C("calcvolume", as.double(t(asc)), as.integer(ncol(asc)),
            as.integer(nrow(asc)), as.double(cs), PACKAGE="adehabitat")[[1]]

      ##
      index<-1:length(v)
      vord<-v[order(v, decreasing=TRUE)]
      indord<-index[order(v, decreasing=TRUE)]
      vsu<-cumsum(vord)
      vreord<-vsu[order(indord)]*100
      u<-matrix(vreord, ncol=ncol(asc), byrow=TRUE)
      x[[i]]$UD<-getascattr(asc,u)
    }
    class(x)<-c("khrvol", "khr")
    return(x)
  }
      
      
###################################################################
###################################################################
###
### Calcul des tailles de domaines vitaux (mcp et kernel)

mcp.area <- function(xy, id, percent = seq(20,95, by=5),
                     unin=c("m", "km"),
                     unout=c("ha", "km2", "m2"))
  {
    xy <- xy[!is.na(xy[, 1]), ]
    xy <- xy[!is.na(xy[, 2]), ]
    id <- id[!is.na(xy[, 1])]
    id <- id[!is.na(xy[, 2])]
    unin<-match.arg(unin)
    unout<-match.arg(unout)
    if (length(id) != nrow(xy)) 
      stop("xy and id should be of the same length")
    if (!require(gpclib))
      stop("package gpclib required")
    
    lev<-percent
    res<-list()
    ar<-matrix(0,nrow=length(lev),
               ncol=nlevels(factor(id)))
    for (i in 1:length(lev)) {
      res[[i]]<-mcp(xy, id, percent=lev[i])
      class(res[[i]])<-"data.frame"
      res[[i]]<-split(res[[i]][,2:3], res[[i]][,1])
      for (j in 1:nlevels(factor(id)))
        ar[i,j]<-area.poly(as(res[[i]][[j]], "gpc.poly"))
    }
    ar <- as.data.frame(ar)
    names(ar)<-levels(factor(id))
    ## modif des unités
    if (unin=="m") {
      if (unout=="ha") 
        ar<-ar/10000
      if (unout=="km2")
        ar<-ar/1000000
    }
    if (unin=="km") {
      if (unout=="ha")
        ar<-ar*100
      if (unout=="m2")
        ar<-ar*1000000
    }
    row.names(ar)<-lev
    class(ar)<-c("hrsize", "data.frame")
    attr(ar, "units")<-unout
    return(ar)
  }





plot.hrsize<-function(x, ...)
  {
    if (!inherits(x, "hrsize"))
      stop("should be of class hrsize")
    opar<-par(mfrow=n2mfrow(ncol(x)))
    on.exit(par(opar))
    for (i in 1:ncol(x)) {
      plot(as.numeric(row.names(x)),
           x[,i],
           main=names(x)[i], pch=16, cex=0.5,
           xlab="Home-range level",
           ylab=paste("Home-range size (",attr(x, "units"),")",sep=""))
      lines(as.numeric(row.names(x)),
            x[,i])
    }
  }






kernel.area <- function (xy, id, h = "href", grid = 40,
                         same4all = FALSE, hlim = c(0.1, 1.5),kern = "bivnorm",
                         levels = seq(20, 95, by = 5), unin = c("m", "km"), 
                         unout = c("ha", "km2", "m2")) 
{
  unin <- match.arg(unin)
  unout <- match.arg(unout)
  x <- kernelUD(xy, id, h, grid, same4all, hlim, kern)
  x <- getvolumeUD(x)
  area <- list()
  contours <- list()
  for (j in names(x)) {
    tmpsurf <- rep(0, length(levels))
    for (i in 1:length(levels)) {
      asc <- x[[j]]$UD
      tmp <- asc < levels[i]
      cs <- attr(asc, "cellsize")
            tmpsurf[i] <- sum(as.numeric(tmp)) * cs * cs
    }
    area[[j]] <- tmpsurf
  }
  area <- data.frame(area)
  row.names(area) <- levels
  names(area) <- names(x)
  if (unin == "m") {
    if (unout == "ha") 
      area <- area/10000
    if (unout == "km2") 
      area <- area/1e+06
  }
  if (unin == "km") {
    if (unout == "ha") 
      area <- area * 100
    if (unout == "m2") 
      area <- area * 1e+06
  }
  class(area) <- c("hrsize", "data.frame")
  attr(area, "units") <- unout
  return(area)
}




getverticeshr<-function(x, lev=95)
  {
    if ((!inherits(x,"khr")))
      stop("non convenient data-type")
    if (inherits(x,"khrud"))
      x<-getvolumeUD(x)
    if (inherits(x,"kbbhrud"))
      x<-getvolumeUD(x)
    contour<-list()
    
    for (i in 1:length(x)) {
      ud<-x[[i]]$UD
      ud[ud>lev]<-NA
      ud[!is.na(ud)]<-1
      contour[[i]]<-getcontour(ud)
    }
    names(contour)<-names(x)
    class(contour) <- "kver"
    return(contour)
  }

plot.kver <- function(x, which = names(x), colpol=rainbow(length(which)),
                      colborder=rep("black", length(which)), lwd = 2,
                      add=FALSE, ...)
  {
    if (!inherits(x, "kver"))
      stop("x should be of class kver")
    x <- x[which]

    if (!add) {
      xc <- unlist(lapply(x, function(y) y[,2]))
      yc <- unlist(lapply(x, function(y) y[,3]))
      plot(xc, yc, asp=1, ty="n", ...)
    }
    
    lapply(1:length(x),
           function(i) plot.area(x[[i]],
                                 colpol = rep(colpol[i], nlevels(x[[i]][,1])),
                                 colborder = rep(colborder[i],
                                   nlevels(x[[i]][,1])),
                                 lwd = lwd, add = TRUE))
    invisible(NULL)    
  }





################################################################
################################################################
####
#### DOMAIN for fitting potential distribution maps

domain<-function(kasc, pts, type=c("value", "potential"),
                 thresh=0.95)
  {
    ## Vérifications
    if (!inherits(kasc, "kasc"))
      stop("should be an object of class \"kasc\"")
    if (ncol(pts)!=2)
      stop("pts should have 2 columns")
    typ<-""
    for (i in 1:length(kasc)) {
      if (is.factor(kasc[[i]])) {
            typ[i] <- "factor"
        }
        else {
            typ[i] <- "numeric"
        }
    }
    if (!all(typ=="numeric"))
      stop("All variables in kasc should be of mode numeric")
    type<-match.arg(type)

    ## Préparation des données pour passage à
    ## La fonction C
    ## 1. jointure spatiale des points
    ptsmod<-as.matrix(join.kasc(pts, kasc))
    ## 2. suppression des valeurs manquantes
    kasct<-kasc2df(kasc)
    kascmod<-as.matrix(kasct$tab)
    if (any(is.na(kascmod)))
      stop("the same area should be provided for all variables")
    ## 3. Calcul du range
    rg<-apply(kascmod, 2, function(x) range(x)[2] - range(x)[1])

    ## Fonction C
    toto<-.C("fctdomain", as.double(t(kascmod)), as.double(t(ptsmod)),
             as.double(rg), as.integer(nrow(ptsmod)),
             as.integer(nrow(kascmod)), as.integer(ncol(ptsmod)),
             double(nrow(kascmod)), PACKAGE="adehabitat")[[7]]

    ## Transfo du vecteur de sortie en Carte
    N <- nrow(kasc)
    indw <- c(1:N)
    n1 <- length(toto)
    compl <- rep(NA, N - n1)
    output <- c(toto, compl)
    indcompl <- indw[is.na(match(indw, kasct$index))]
    indtot <- c(kasct$index, indcompl)
    output <- output[sort(indtot, index.return = TRUE)$ix]
    output<-matrix(output, attr(kasc,"ncol"))

    ## Seuil ou carte ?
    if (type!="value") {
      output[output<=thresh]<-NA
      output[output>thresh]<-1
    }
    
    attr(output, "xll") <- attr(kasc, "xll")
    attr(output, "yll") <- attr(kasc, "yll")
    attr(output, "cellsize") <- attr(kasc, "cellsize")
    attr(output, "type") <- "numeric"
    class(output)<-"asc"
    return(output)
  }






###########################################################################
####
#### rapports de sélection pour designs III

widesIII<-function(u, a, avknown = TRUE, alpha = 0.05)
  {
    u<-as.matrix(u)
    a<-as.matrix(a)
    
    ## Vérifications de départ
    if (nrow(u) != nrow(a)) 
      stop("available and used matrix should have the same number of animals")
    if (ncol(u) != ncol(a)) 
      stop("available and used matrix should have the same number of habitats")
    
    ## Les transfos de base
    sorties<-list()
    pij<-as.matrix(a)

    ## Calcul de la disponibilité si pas fournie
    ## en pourcentages pij
    aip<-apply(a,2,sum)
    apj<-apply(a,1,sum)
    
    ## Calcul de l'utilisation (pourcentage)
    uij<-as.matrix(u)
    if (is.null(colnames(u))) 
      colnames(uij) <- paste("Habitat", 1:ncol(u), sep = "")
    if (is.null(colnames(a))) 
      colnames(pij) <- paste("Habitat", 1:ncol(a), sep = "")

    ## Les deux matrices
    pij<-as.matrix(a/apj)
    uij<-as.matrix(u)
    I<-ncol(uij)
    J<-nrow(uij)

    ## Calcul de l'IC de Bonferroni
    bonferroni <- alpha/(I * (I - 1)/2)
    upj<-apply(uij,1,sum)
    uip<-apply(uij,2,sum)
    wij<-uij/(upj*pij)
    wi<-uip/apply(pij*upj,2,sum)

    ## sorties
    sorties$used.prop <- t(t(uij)/uip)
    sorties$avail.prop <- pij
    sorties$wij<-wij
    sorties$wi<-wi

    ## Calcul des Khi2
    Khi2Lj<-matrix(0, nrow=J, ncol=3)
    colnames(Khi2Lj)<-c("Khi2Lj", "df", "pvalue")
    for (j in 1:J) {
      euij<-uij[j,]*log(uij[j,]/(upj[j]*pij[j,]))
      ddl<-length(euij[!is.na(euij)])-1
      euij<-euij[!is.na(euij)]
      Khi2Lj[j,1]<-sum(euij)
      Khi2Lj[j,2]<-ddl
      Khi2Lj[j,3]<-1 - pchisq(Khi2Lj[j,1], ddl)
    }

    rownames(Khi2Lj)<-rownames(u)
    
    sorties$Khi2Lj<-Khi2Lj
    Khi2L<-apply(Khi2Lj,2,sum)
    Khi2L[3]<-1 - pchisq(Khi2L[1],Khi2L[2])
    names(Khi2L)<-c("Khi2L", "df", "pvalue")
    sorties$Khi2L<-Khi2L

    ## Variance de wi
    vwi<-rep(0,I)
    for (i in 1:I) {
      yj<-uij[,i]
      xj<-pij[,i]*upj
      vwi[i]<-(sum((yj-wi[i]*xj)**2)/(J-1))*(1/(J*(mean(xj)**2)))
    }

    sewi<-sqrt(vwi)
    sorties$se.wi<-sewi
    sorties$ICwiupper<-round(wi+sewi*qnorm(1 - alpha/(2*I)), 4)
    sorties$ICwilower<-round(wi-sewi*qnorm(1 - alpha/(2*I)), 4)

    ## Différences des selection ratios
    diffwi<-outer(wi,wi,"-")
    vardif<-matrix(0, I, I)
    ICdiffupper<-matrix(0, I, I)
    ICdifflower<-matrix(0, I, I)
    sig<-matrix("0", I, I)
    
    for (i in 1:I) {
      for (j in 1:I) {
        if (avknown) {
          ## dispo connue
          spi<-sum(pij[,i]*upj)
          spj<-sum(pij[,j]*upj)
          
          vardif[i,j]<-sum(((uij[,i]-wi[i]*upj)/spi +
                            (uij[,j]-wi[j]*upj)/spj )**2 )*(J/(J-1))
        }
        else {
          ## dispo inconnue
          dftmp<-data.frame(y1=uij[,i],y2=uij[,j],
                            x1=pij[,i]*upj, x2=pij[,j]*upj)
          vc<-var(dftmp)
          y1<-uip[i]
          y2<-uip[j]
          x1<-sum(pij[,i]*upj)
          x2<-sum(pij[,j]*upj)
          vardif[i,j]<-(1/(y1**2))*vc["x1","x1"] +
            ((x1**2)/(y1**4))*vc["y1","y1"] +
              (1/(y2**2))*vc["x2","x2"] +
                ((x2**2)/(y2**4))*vc["y2","y2"] -
                  2*(x1/(y1**3))*vc["x1","y1"] -
                    2*(1/(y1*y2))*vc["x1","x2"] +
                      2*(x2/(y1*(y2**2)))*vc["x1","y2"] +
                        2*(x1/(y2*(y1**2)))*vc["y1","x2"] -
                          2*((x1*x2)/((y1**2)*(y2**2)))*vc["y1","y2"] -
                            2*(x2/(y2**3))*vc["x2","y2"]
        }
        vardif[row(vardif)==col(vardif)]<-0

        ## calcul des ic...
        ICdiffupper[i, j] <- round(diffwi[i, j] +
                                   sqrt(vardif[i,j]) *
                                   qnorm(1 - bonferroni/2), 4)
        ICdifflower[i, j] <- round(diffwi[i, j] -
                                   sqrt(vardif[i,j]) *
                                   qnorm(1 - bonferroni/2), 4)
        sig[i, j] <- ifelse(diffwi[i, j] < 0, "-", "+")

        ## ... et de la signification  des différences
        if (ICdiffupper[i, j] < 0)
          sig[i, j] <- "---"
        if (ICdifflower[i, j] > 0) 
          sig[i, j] <- "+++"
      }
    }
    
    rownames(diffwi) <- colnames(u)
    colnames(diffwi) <- colnames(u)
    rownames(ICdiffupper) <- colnames(u)
    colnames(ICdiffupper) <- colnames(u)
    rownames(ICdifflower) <- colnames(u)
    colnames(ICdifflower) <- colnames(u)
    rownames(sig) <- colnames(u)
    colnames(sig) <- colnames(u)
    sorties$avknown <- avknown
    sorties$comparisons$diffwi <- diffwi
    sorties$comparisons$ICdiffupper <- ICdiffupper
    sorties$comparisons$ICdifflower <- ICdifflower
    sorties$comparisons$signif <- sig
    sorties$profile <- profilehab(sig, wi)
    sorties$alpha <- alpha
    class(sorties) <- c("wiIII", "wi")
    return(sorties)
  }



print.wiIII<-function(x, ...)
  {
    if (!inherits(x,"wiIII"))
      stop("x should be of class \"wiIII\"")
    cat("\n\n************** Manly's Selection ratios for design III ********\n\n")
    cat("1. Test of habitat selection for each animal:\n\n")
    print(x$Khi2Lj)

    cat("\n\n2. Test of overall habitat selection:\n")
    print(x$Khi2L)
    cat("\n\nTable of selection ratios:\n")
    print(data.frame(Wi=x$wi,
                     SE=x$se.wi, IClower=x$ICwilower, ICupper=x$ICwiupper), ...)
    cat("\n\nBonferroni classement \nBased on", (1 - x$alpha) * 
        100, "% confidence intervals on the differences of Wi :\n")
    print(x$profile, quote = FALSE)
    cat("\n")
}




##############################################################################################
##############################################################################################
#####
##### Interface vers les objets im de spatstat

asc2im<-function(x)
  {
    if (!inherits(x, "asc"))
      stop("should be an object of class \"asc\"")
    if (attr(x, "type")=="factor")
      stop("function not yet implemented for factors")
    if (!require(spatstat))
      stop("the package spatstat should be available for this function")
    xy<-getXYcoords(x)
    sorties<-im(t(unclass(x)), xy$x,xy$y)
    return(sorties)
  }


im2asc<-function(x)
  {
    if (!inherits(x, "im"))
      stop("xshould be of class \"im\"")
    if (x$xstep!=x$ystep)
      stop("the grid cellsize should be identical for both X and Y directions.")
    mat<-x$v
    xll<-min(x$xcol)
    yll<-min(x$yrow)
    cellsize<-x$xstep
    attr(mat, "xll")<-xll
    attr(mat, "yll")<-yll
    attr(mat, "cellsize")<-cellsize
    attr(mat, "type")<-"numeric"
    class(mat)<-"asc"
    return(mat)
  }





##############################################################################################
##############################################################################################
#####
##### Fonction as.asc

as.asc<-function(x, xll=1, yll=1, cellsize=1, type=c("numeric", "factor"),
                 lev=levels(factor(x)))
  {
    type<-match.arg(type)
    if (!inherits(x, "matrix"))
      stop("x should be a matrix")
    mode(x)<-"numeric"
    attr(x, "xll")<-xll
    attr(x, "yll")<-yll
    attr(x, "cellsize")<-cellsize
    attr(x, "type")<-type
    if (type=="factor")
      attr(x, "levels")<-lev
    class(x)<-"asc"
    return(x)
  }


##############################################################################################
##############################################################################################
#####
##### les distances de Mahalanobis


mahasuhab<-function(kasc, pts, type=c("distance", "probability"))
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

    ## disponibilité
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



##############################################################################################
##############################################################################################
#####
##### Analyse compositionnelle

compana<-function(used, avail, test = c("randomisation", "parametric"),
                  rnv = 0.01, nrep = 500, alpha=0.1)
  {
    ### 1. Vérifications
    test<-match.arg(test)
    used<-as.matrix(used)
    avail<-as.matrix(avail)
    if ((any(avail==0))&(test=="parametric")) {
      warning("parametric tests not suitable with 0 in avail; test has been set to \"randomisation\"")
      test<-"randomisation"
    }
    if (ncol(used)!=ncol(avail))
      stop("the two matrices should have the same dimensions")
    if (nrow(used)!=nrow(avail))
      stop("the two matrices should have the same dimensions")
    if (!all(colnames(used)==colnames(avail)))
      stop("the two matrices should have the same habitat names")
    if (is.null(colnames(used))) 
      colnames(used) <- paste("Habitat", 1:ncol(u), sep = "")
    if (is.null(colnames(avail))) 
        colnames(avail) <- paste("Habitat", 1:ncol(a), sep = "")

    ## 2. Bases
    nh<-ncol(used)
    na<-nrow(used)
    proj1<-matrix(1, nrow=nrow(used), ncol=nrow(used))*(1/nrow(used))
    proj2<-matrix(0, nrow=nrow(used), ncol=nrow(used))
    if (test=="parametric")
      nrep=1
    sorties<-list()

    ## 3. Première partie: test global
    toto<-.C("aclambda", as.double(t(used)), as.double(t(avail)),
             as.integer(na), as.integer(nh),  
             as.double(proj1), as.double(proj2), as.double(rnv),
             double(nrep), as.integer(nrep), double(nh), double(nh),
             PACKAGE="adehabitat")
    
    vrand<-toto[[8]]
    sorties$used<-used
    sorties$avail<-avail
    sorties$type.test<-test
    if (test=="randomisation") {
      sorties$random.res<-list(sim=vrand, obs=vrand[1])
      sorties$test<-c(vrand[1], length(vrand[vrand<=vrand[1]])/nrep)
      names(sorties$test)<-c("Lambda", "P")
    }
    else {
      sorties$test<-c(vrand[1], ncol(used)-1, 1-pchisq(-na*log(vrand[1]), ncol(used)-1))
      names(sorties$test)<-c("Lambda", "df", "P")
    }
    
    ## Deuxième partie: ranking matrix
    if (test=="randomisation") {
      toto<-.C("rankma", as.double(t(used)), as.double(t(avail)),
               double(nh**2), double(nh**2), double(nh**2),
               double(nh**2), as.integer(nh), as.integer(na),
               as.integer(nrep), as.double(rnv), PACKAGE="adehabitat")
      
      rmp<-t(matrix(toto[[3]]/nrep, nh, nh))
      rmm<-t(matrix(toto[[4]]/nrep, nh, nh))
      rmv<-t(matrix(toto[[5]], nh, nh))
      rmnb<-t(matrix(toto[[6]], nh, nh))
    }
    else {
      used[used==0]<-rnv
      rmv<-matrix(0, nh, nh)
      rmse<-matrix(0, nh, nh)
      rmm<-matrix(0, nh, nh)
      rmp<-matrix(0, nh, nh)
      rmnb<-matrix(0, nh, nh)

      for (i in 1:nh) {
        for (j in 1:nh) {
          dlr<-log(used[,i]/used[,j])-log(avail[,i]/avail[,j])
          rmv[i,j]<-mean(dlr)
          rmse[i,j]<-sqrt(var(dlr)/na)
          if (i!=j)
            rmv[i,j]<-rmv[i,j]/rmse[i,j]
          rmp[i,j]<-pt(rmv[i,j], na-1)
          rmm[i,j]<-1-rmp[i,j]
          rmnb[i,j]<-na
        }
      }
    }
    
    rm<-matrix("0", nh, nh)
    

    ## ranking matrix: juste les signes
    for (i in 1:nh) {
      for (j in 1:nh) {
        if (rmv[i,j]<0)
          rm[i,j]<-"-"
        if (rmv[i,j]>0)
          rm[i,j]<-"+"
      }
    }

    for (i in 1:nh) {
      for (j in 1:nh) {
        if (rmp[i,j] < (alpha/2)) {
          rm[i,j]<-"---"
        }
        if (rmm[i,j] < (alpha/2)) {
          rm[i,j]<-"+++"
        }
        if (i==j)
          rm[i,j]<-"0"
      }
    }
  


    rank<-rep(0, nh)
    for (j in 1:nh) {
      for (i in 1:nh) {
        if (rmv[j,i]>0)
          rank[j]<-rank[j]+1
      }
    }
    
    names(rank)<-colnames(avail)
    rownames(rm)<-colnames(avail)
    colnames(rm)<-colnames(avail)
    rownames(rmv)<-colnames(avail)
    colnames(rmv)<-colnames(avail)
    rownames(rmp)<-colnames(avail)
    colnames(rmp)<-colnames(avail)
    rownames(rmm)<-colnames(avail)
    colnames(rmm)<-colnames(avail)
    rownames(rmnb)<-colnames(avail)
    colnames(rmnb)<-colnames(avail)
    sorties$rmnb<-rmnb
    sorties$rank<-rank
    sorties$rm<-rm
    sorties$rmv<-rmv
    
    sorties$profile<-profilehab(rm, rank)
    class(sorties)<-"compana"
    return(sorties)
  }




print.compana<-function(x, ...)
  {
    if (!inherits(x, "compana"))
      stop("should be an object of class \"compana\"")
    cat("************ Compositional analysis of habitat use ***************\n\n")
    cat("The analysis was carried out with", nrow(x$used),
        "animals and", ncol(x$used), "habitat types\n")
    cat("1. Test of the habitat selection:\n")
    cat("  ", x$type.test, "test\n")
    print(x$test)
    cat("\n2. Ranking of habitats (profile):\n")
    print(x$profile, quote=FALSE)
  }


##############################################################################################
##############################################################################################
#####
##### Création de fichiers DXF


area2dxf<-function(x, file, lay=1:nlevels(factor(x[,1])))
  {
    ## vérification du format du fichier
    if (!inherits(x, "area"))
      stop("x should be of class area")
    if (substr(file, nchar(file)-3, nchar(file))!=".dxf")
      file<-paste(file, ".dxf", sep="")

    ## Vérification que le premier et le dernier point de chaque polygone
    ## sont identiques. Sinon modifier de fichier de façon ad hoc
    lipol<-split(x, x[,1])
    for (i in 1:length(lipol)) {
      j<-lipol[[i]]
      if (!all(j[1,]==j[nrow(j),]))
        lipol[[i]]<-rbind.data.frame(lipol[[i]], lipol[[i]][1,])
    }
    x<-do.call("rbind.data.frame",lipol)
    
    ## header
    text<-"  0\nSECTION\n  2\nHEADER\n  9\n$EXTMIN\n 10\n"
    text<-paste(text, min(x[,2]),"\n", sep="")
    text<-paste(text, " 20\n", sep="")
    text<-paste(text, min(x[,3]),"\n", sep="")
    text<-paste(text, "  9\n$EXTMAX\n 10\n", sep="")
    text<-paste(text, max(x[,2]),"\n", sep="")
    text<-paste(text, " 20\n", sep="")
    text<-paste(text, max(x[,3]),"\n", sep="")
    text<-paste(text, "  0\nENDSEC\n  0\nSECTION\n", sep="")
    text<-paste(text, "2\nTABLES\n  0\nENDSEC\n  0\n", sep="")
    text<-paste(text, "SECTION\n  2\nBLOCKS\n  0\n", sep="")
    text<-paste(text, "ENDSEC\n  0\nSECTION\n  2\nENTITIES\n", sep="")
    
    ## création du corps du fichier: boucle
    lp<-split(x[,2:3], x[,1])
    for (i in 1:length(lp)) {
      text<-paste(text, "  0\nPOLYLINE\n  8\n", sep="")
      text<-paste(text, "  ",lay[i],"\n 66\n      1\n", sep="")
      for (j in 1:nrow(lp[[i]]))
        text<-paste(text, "  0\nVERTEX\n  8\n",
                    lay[[i]], "\n 10\n", lp[[i]][j,1],
                    "\n 20\n", lp[[i]][j,2], "\n", sep="")
      text<-paste(text, "  0\nSEQEND\n")
    }
    text<-paste(text, "      0\nENDSEC\n  0\nEOF\n")
    cat(text, file=file)
  }



##############################################################################################
##############################################################################################
#####
##### Morphologie mathématique: érosion et dilatation

morphology<-function(x, operation = c("erode", "dilate"), nt=5)
  {
    op<-match.arg(operation)
    if (nt<1)
      stop("nt should be > 0")
    if (op=="erode")
      ope<-0
    if (op=="dilate")
      ope<-1
    if (!inherits(x,"asc"))
      stop("should be of class asc")
    
    nc<-ncol(x)
    nr<-nrow(x)
    tmpc<-rep(NA,nc)
    u<-rbind(tmpc,x,tmpc)
    tmpl<-rep(NA,nr+2)
    u<-cbind(tmpl,u,tmpl)
    o<-as.vector(t(u))
    o[!is.na(o)]<-1
    o[is.na(o)]<-0
    
    res<-.C("erodil", as.double(o), as.integer(nr+2), as.integer(nc+2),
            as.integer(nt), as.integer(ope), PACKAGE="adehabitat")
    res[[1]][res[[1]]==0]<-NA
    gr<-matrix(res[[1]], nrow=(nr+2), byrow=TRUE)
    gr <- gr[-c(1, nrow(gr)), -c(1, ncol(gr))]
    if (all(is.na(gr)))
      stop("all the image has been erased\n Please consider a lower value for nt")
    gr<-getascattr(x,gr)
    return(gr)
  }


##############################################################################################
##############################################################################################
#####
##### Etiquetage séquentiel

labcon<-function(x)
  {
    if (!inherits(x, "asc")) 
      stop("should be an object of class asc")
    y<-x
    rajfond <- function(x) {
      nr <- nrow(x)
      nc <- ncol(x)
      f <- rep(0, nr)
      x <- cbind(f, x, f)
      f <- rep(0, nc + 2)
      x <- rbind(f, x, f)
    }
    x[!is.na(x)] <- 1
    x[is.na(x)] <- 0
    x <- rajfond(x)
    toto <- .C("seqeticorr", as.double(t(x)), as.integer(nrow(x)), 
               as.integer(ncol(x)), PACKAGE="adehabitat")
    etiquete <- matrix(toto[[1]], nrow = nrow(x), byrow = TRUE)
    etiquete <- etiquete[-c(1, nrow(etiquete)), -c(1, ncol(etiquete))]
    etiquete[etiquete==0]<-NA
    s<-getascattr(y, etiquete)
    attr(s, "type")<-"factor"
    attr(s, "levels")<-as.character(1:nlevels(factor(etiquete)))
    return(s)
  }



##############################################################################################
##############################################################################################
#####
##### Gestion des trajectoires

as.traj<-function(id, xy, date, burst=id, ...)
  {
    if (ncol(xy)!=2)
      stop("xy should have two columns")
    if (!inherits(date, "POSIXct"))
      stop("date should be of class \POSIXct\"")

  
    names(xy)<-c("x", "y")
    bas<-data.frame(id=id,
                    xy, date=date, burst=burst, ...)
    foo<-function(x) x[order(x$date),]
    li<-split(bas, bas$burst)
    nl <- unlist(lapply(li, nrow)) > 1
    li <- li[nl]
    if (any(!nl))
      warning(paste("At least two relocations are needed for a burst:\n",
                    sum(!nl), "circuits have been deleted"))
    li<-lapply(li, foo)
    
    ## Vérification que pas de doublons au niveau des dates
    foob<-function(x) {
      ind<-rep(0,nrow(x))
      for (i in 2:nrow(x)) {
        if ((as.numeric(x$date))[i]==(as.numeric(x$date))[i-1])
          ind[i]<-1
      }
      return(x[ind==0,])
    }
    
    li<-lapply(li, foob)
    bas<-do.call("rbind", li)
    row.names(bas)<-as.character(1:nrow(bas))
    class(bas)<-c("traj", "data.frame")
    return(bas)
  }


print.traj<-function(x, ...)
  {
    if (!inherits(x, "traj"))
      stop("x should be an object of class traj")
    levani<-levels(x$id)
    u<-split(x$burst, x$id)
    cat("******** Data frame of class traj *********\n\n")
    for (i in 1:length(u)) {
      cat("Animal ",names(u)[i],":   ",
          nlevels(factor(u[[i]])), " circuits")
      cat(" (",length(u[[i]])," relocations)\n", sep="")
    }
    cat("\nVariables measured for each relocation:\n\n")
    print(names(x), quote=FALSE, ...)
  }

summary.traj<-function(object, id=levels(object$id), date=NULL, ...)
  {
    x<-object
    if (!inherits(x, "traj"))
      stop("x should be an object of class traj")

    ## prévoir le cas où un objet vide est choisi
    ## sélection des dates
    if (!is.null(date)) 
      x<-x[(x$date>=date[1])&(x$date<date[2]),]
    
    ## sélection des animaux
    li<-split(x, factor(x$id))
    x<-do.call("rbind", li[id])
    x$id<-factor(x$id)
    x$burst<-factor(x$burst)
    
    ## sortie des circuits
    ll<-list()
    for (i in id) {
      if (!is.na(match(i, levels(x$id)))) {
        cat("Animal ", i, ": ",
            nlevels(factor(li[[i]]$burst)),
            "circuits. \nNumber of relocations per circuit:")
        o<-li[[i]]
        print(table(factor(o$burst)))
        ll[[i]]<-table(factor(o$burst))
        cat("\n")
      }
    }
    invisible(ll)
  }

traj2df<-function(x) {
  if (!inherits(x, "traj"))
    stop("x should be of class traj")
  class(x)<-"data.frame"
  row.names(x)<-as.character(1:nrow(x))
  return(x)
}

df2traj<-function(df) {
  x<-df
  if (!inherits(x, "data.frame"))
    stop("x should be of class data.frame")

  ## vérification du format:
  ok<-1
  if (is.null(x$id))
    ok<-0
  if (is.null(x$x))
    ok<-0
  if (is.null(x$y))
    ok<-0
  if (is.null(x$date))
    ok<-0
  if (is.null(x$burst))
    ok<-0
  if (!inherits(x$date, "POSIXct"))
    ok<-0
  if (!is.factor(x$id))
    ok<-0
  if (!is.factor(x$burst))
    ok<-0

  if (ok == 0)
    stop("non convenient format.\n please create the object with the function as.traj")
  class(x)<-c("traj", "data.frame")
  return(x)
}
  







plot.traj<-function(x, id=levels(x$id), burst=levels(x$burst), date=NULL,
                    asc=NULL, area=NULL,
                    xlim=range(x$x), ylim=range(x$y),
                    colasc=gray((256:1)/256), colpol="green",
                    addpoints=TRUE, addlines=TRUE,
                    perani=TRUE, final=TRUE,...)
  {
    polygon<-area
    if (!is.null(area)) {
      if (!inherits(area, "area"))
        stop("x should be an object of class area")
    }
    if (!inherits(x, "traj"))
      stop("x should be an object of class traj")
    
    ## sélection des dates
    if (!is.null(date)) 
      x<-x[(x$date>=date[1])&(x$date<date[2]),]
    
    ## sélection des animaux
    i<-split(x, x$id)
    x<-do.call("rbind", i[id])
        
    ## sélection des circuits
    i<-split(x, x$burst)
    x<-do.call("rbind", i[burst])
    x$burst<-factor(x$burst)
    x$id<-factor(x$id)
    
    if (!perani)
      idc<-"burst"
    else
      idc<-"id"
    li<-split(x, x[[idc]])
    id<-levels(x[[idc]])
    opar<-par(mar=c(0.1,0.1,2,0.1),
              mfrow=n2mfrow(length(li)))
    m<-unlist(lapply(li, function(x) mean(x$date)))
    nli<-names(li)
    nli<-nli[order(m)]
    
    ## boucle pour chaque graphe
    for (i in nli) {
      if (!is.null(asc))
        image(asc, col=colasc,
              xlim=xlim, ylim=ylim, main=i, axes=FALSE,...)
      else
        plot(x$x,x$y, type="n", asp=1,
             xlim=xlim, ylim=ylim, axes=FALSE,
             main=i, ...)
      box()
      if (!is.null(polygon)) {
        pol<-split(polygon[,2:3], factor(polygon[,1]))
        for (j in 1:length(pol))
          polygon(pol[[j]], col=colpol)
      }
      if (addlines) {
        for (j in levels(factor(li[[i]]$burst))) {
          lines(x$x[x$burst==j], x$y[x$burst==j])
        }
      }
      if (addpoints) {
        for (j in levels(factor(li[[i]]$burst))) {
          points(x$x[x$burst==j],x$y[x$burst==j],pch=21,
                 col="black", bg="white")
        }
      }
      if (final) {
        for (j in levels(factor(li[[i]]$burst))) {
          points(x$x[x$burst==j][c(1,length(x$x[x$burst==j]))],
                 x$y[x$burst==j][c(1,length(x$y[x$burst==j]))],
                 pch=14, col=c("blue", "red"))
        }
      }
    }
    par(opar)
  }

          
          

    
getburst<-function(x, burst=levels(x$burst),
                   id=levels(x$id), date=NULL)
{
  if (!inherits(x, "traj"))
    stop("should be an object of class traj")
  ## sélection des dates
  if (!is.null(date)) 
    x<-x[(x$date>=date[1])&(x$date<date[2]),]
  
  ## sélection des animaux
  i<-split(x, x$id)
  x<-do.call("rbind", i[id])
  
  ## sélection des circuits
  i<-split(x, x$burst)
  x<-do.call("rbind", i[burst])
  x$burst<-factor(x$burst)
  x$id<-factor(x$id)
  return(x)
}


speed<-function(x, id=levels(x$id), burst=levels(x$burst),
                date=NULL, units=c("seconds", "hours","days"))
  {
    if (!inherits(x, "traj"))
      stop("should be an object of class traj")
    units<-match.arg(units)
    
    ## sélection des dates
    x<-getburst(x, burst=burst, id=id, date=date)
    
    ## Calcul des distances entre locs successives
    li<-split(x, x$burst)
    foo<-function(x) {
      x1<-x[-1,]
      x2<-x[-nrow(x),]
      dist<-sqrt( (x1$x-x2$x)^2 + (x1$y-x2$y)^2)
      hour<-(unclass(x1$date)-unclass(x2$date))
      if (units=="hours")
        hour<-(unclass(x1$date)-unclass(x2$date))/3600
      if (units=="days")
        hour<-(unclass(x1$date)-unclass(x2$date))/(3600*24)
      disx<-(x1$x-x2$x)
      disy<-(x1$y-x2$y)
      so<-cbind.data.frame(id=x2$id,x=x2$x, y=x2$y, date=x2$date,
                           burst=x2$burst,
                           sp.x=disx/hour, sp.y=disy/hour,
                           speed=dist/hour, dt=hour)
      return(so)
    }
    lo<-do.call("rbind", lapply(li, foo))
    row.names(lo)<-1:nrow(lo)
    return(lo)
  }


convnum<-function(kasc) {
  if (!inherits(kasc, "kasc"))
    stop("should be of class kasc")
  litab<-kasc2df(kasc)
  dud<-dudi.mix(litab$tab, scannf=FALSE)
  cw<-dud$cw
  scores <- df2kasc(dud$tab, litab$index, kasc)
  return(list(kasc=scores, weight=cw))
}



###############################################################################
###############################################################################
####
#### Calcul des angles


angles<-function (x, id = levels(x$id), burst = levels(x$burst),
                  date = NULL, slsp = c("remove", "missing")) 
  {
    if (!inherits(x, "traj"))
      stop("x should be of class \"traj\"")
    slsp <- match.arg(slsp)
    
    prepangles <- function(x)
      {
        if (!inherits(x, "traj")) 
          stop("x should be of class \"traj\"")
        li <- split(x, x$burst)
        foo <- function(y) {
          oo <- unlist(lapply(2:nrow(y),
                              function(i) (!all(y[i,c("x","y")]==y[i-1,c("x","y")]))))
          oo <- c(TRUE,oo)
          y <- y[oo,]
        }
        res <- do.call("rbind", lapply(li, foo))
        return(res)
      }
    
    x <- getburst(x, burst = burst, id = id, date = date)
    if (slsp=="remove")
      x <- prepangles(x)
    li <- split(x, x$burst)
    
    foo <- function(x) {
      xy<-as.matrix(x[,c("x","y")])
      ang<-1:(nrow(xy)-2)
      for (i in 2:(nrow(xy)-1)) {
        na <- 0
        ref1<-xy[i-1,]
        xyb1<-t(t(xy)-ref1)
        ang1<--atan2(xyb1[i,2],xyb1[i,1])

        ## calcul de la position de x2 et x3 rotaté
        x2<-c(sqrt(sum(xyb1[i,]^2)), 0)
        if (sum(abs(x2)) < 1e-7)
          na<-1
        x3b<-x3<-xyb1[i+1,]
        x3b[1]= cos(ang1)*x3[1] - sin(ang1)*x3[2]
        x3b[2]= sin(ang1)*x3[1] + cos(ang1)*x3[2]
        x3<-x3b
        
        ## et recalcul de l'angle
        x3<-x3-x2
        if (sum(abs(x3)) < 1e-7)
          na<-1
        ang[i-1]<-atan2(x3[2],x3[1])
        if (na > 0.5)
          if (slsp == "missing")
            ang[i - 1] <- NA
      }
      so<-data.frame(id=x$id[-c(1,nrow(xy))],
                     x=xy[-c(1,nrow(xy)),1],
                     y=xy[-c(1,nrow(xy)),2],
                     date=x$date[-c(1,nrow(xy))],
                     burst=x$burst[-c(1,nrow(xy))],
                     angles=ang)
    }
    lo <- do.call("rbind", lapply(li, foo))
    row.names(lo) <- 1:nrow(lo)
    return(lo)    
  }




#######################################################################
#######################################################################
####
#### Diminution de la résolution d'une ou plusieurs cartes


### Fonction générique

lowres<-function(x, np = 2, ...)
{
  UseMethod("lowres")
}



### Fonction pour classe asc

lowres.asc<-function(x, np=2, ...)
  {
    if (!inherits(x, "asc"))
      stop("x sould be of class \"asc\"")
    nr<-nrow(x)
    nc<-ncol(x)
    xy<-getXYcoords(x)
    cs<-attr(x, "cellsize")
    if (attr(x, "type")=="factor") {
      typ <- "factor"
      lev <- levels(x)
    } else {
      typ <- "numeric"
    }
    x<-x[1:(nr-(((nr/np)-floor(nr/np)))*np),1:(nc-(((nc/np)-floor(nc/np)))*np)]
    nr<-nrow(x)
    nc<-ncol(x)

    if (typ=="factor") {
      repr<- as.numeric(levels(factor(as.vector(x))))
      lev <- lev[repr]
      x <- as.numeric(as.character(factor(x)))
      x <- matrix(x, nrow=nr, ncol=nc)
    }
    
    x[is.na(x)]<--9999
    xs<-matrix(0, nrow=nr/np, ncol=nc/np)
    if (typ == "numeric") {
      mat<-.C("regrouascnumr", as.double(t(x)), as.double(t(xs)),
              as.double(nrow(x)), as.double(ncol(x)),
              as.double(nrow(xs)), as.double(ncol(xs)), PACKAGE = "adehabitat")[[2]]
    } else {
      mat<-.C("regroufacascr", as.double(t(x)), as.double(t(xs)), as.integer(np),
              as.integer(length(lev)), as.integer(nrow(x)), as.integer(ncol(x)),
              as.integer(nrow(xs)), as.integer(ncol(xs)), PACKAGE = "adehabitat")[[2]]
    }
    mat<-matrix(mat,ncol=ncol(xs), byrow=TRUE)
    mat[mat==-9999]<-NA
    attr(mat, "xll")<-mean(xy$x[1:np])
    attr(mat, "yll")<-mean(xy$y[1:np])
    attr(mat, "cellsize")<-cs*np
    attr(mat, "type")<-typ
    if (typ == "factor")
      attr(mat, "levels") <- lev
    class(mat)<-"asc"
    return(mat)
  }



### Fonction pour classe kasc

lowres.kasc<-function(x, np=2, ...)
  {
    if (!inherits(x, "kasc"))
      stop("x sould be of class \"kasc\"")
    so <- list()
    for (i in names(x)) {
      so[[i]]<-lowres.asc(getkasc(x, i), np)
    }
    x<-as.kasc(so)
    return(x)
  }





#######################################################################
#######################################################################
####
#### Subset carte

subsetmap<-function(x, xlim=NULL, ylim=NULL, ...)
  {
    UseMethod("subsetmap")
  }

subsetmap.asc<-function(x, xlim=NULL, ylim=NULL, ...)
  {
    if (!inherits(x, "asc"))
      stop("x should be of class asc")
    if ((is.null(xlim))|(is.null(ylim))) {
      image(x, main="select the boundaries of the subset")
      ii<-locator(2)
      xlim<-ii$x
      ylim<-ii$y
    }
    xy<-getXYcoords(x)
    xlim<-xlim[order(xlim)]
    ylim<-ylim[order(ylim)]
    xll<-attr(x, "xll")
    yll<-attr(x, "yll")
    cs<-attr(x, "cellsize")
    posli1<-round((xlim[1]-xll)/cs, 0)+1
    posco1<-round((ylim[1]-yll)/cs, 0)+1
    posli2<-round((xlim[2]-xll)/cs, 0)+1
    posco2<-round((ylim[2]-yll)/cs, 0)+1
    o<-x[posli1:posli2,posco1:posco2]
    attr(o, "xll")<-xy$x[posli1]
    attr(o, "yll")<-xy$y[posco1]
    attr(o, "cellsize")<-cs
    attr(o, "type")<-attr(x, "type")
    if (attr(o, "type")=="factor")
      attr(o, "levels")<-attr(x, "levels")
    class(o)<-"asc"
    return(o)
  }

subsetmap.kasc<-function(x, xlim=NULL, ylim=NULL, ref=names(x)[1], ...)
  {
    if (!inherits(x, "kasc"))
      stop("x should be of class kasc")
    if ((is.null(xlim))|(is.null(ylim))) {
      image(getkasc(x, ref), main="select the boudaries of the subset")
      ii<-locator(2)
      xlim<-ii$x
      ylim<-ii$y
    }
    so<-list()
    for (i in names(x))
      so[[i]]<-subsetmap.asc(getkasc(x, i), xlim=xlim, ylim=ylim)
    so<-as.kasc(so)
    return(so)
  }



##########################################################################
##########################################################################
#####
#####   as.area définit la classe area

as.area<-function(x)
  {
    if (!inherits(x, "data.frame"))
      stop("x should be of class \"data.frame\"")
    if (ncol(x) != 3)
      stop("x should have three columns")
    if (!is.factor(x[,1]))
      x<-factor(x[,1])
    class(x)<-c("area", "data.frame")
    return(x)
  }

##########################################################################
##########################################################################
#####
#####   storemapattr permet d'éviter d'avoir à constamment se trimbaler
#####   des cartes monstrueuses juste pour avoir leurs attributs

storemapattr<-function(x)
  {
    if ((!inherits(x,"asc"))&(!inherits(x,"kasc")))
      stop("x should be a map of class asc or kasc")
    toto<-0
    if (inherits(x, "asc"))
      x<-as.kasc(list(x=x))
    toto<-getkascattr(x,toto)
    class(toto)<-"mapattr"
    return(toto)
  }


##########################################################################
##########################################################################
#####
#####   L'enfa
#####   

biv.test <- function(dfxy, point, br = 10, points = TRUE, density = TRUE, 
	kernel = TRUE, o.include = FALSE, pch, cex, col, Pcol, h, sub, 
	side = c("top", "bottom", "none"), ...)
{
  side <- match.arg(side)
  if (!inherits(dfxy, "data.frame")) 
    stop("dfxy should be a data frame")
  if (ncol(dfxy) < 2) 
    stop("dfxy should have at least two columns")
  if (!require(MASS) & kernel) 
    stop("This function needs the package MASS")
  if (missing(pch))
    pch <- 16
  if (missing(cex))
    cex <- 0.5
  if (missing(col))
    col <- grey(0.6)
  if (missing(Pcol))
    Pcol <- grey(0.6)
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  lay <- layout(matrix(c(2,4,1,3),2,2, byrow = TRUE), c(3,1), 
		c(1,3), TRUE)
  layout.show(lay)
  
  x <- dfxy[, 1]
  y <- dfxy[, 2]
  xr <- diff(range(x))
  yr <- diff(range(y))
  xby <- xr/(br-1)
  yby <- yr/(br-1)
  xp <- 0
  xn <- 0
  yp <- 0
  yn <- 0
  if (max(x)>0 | point[1]>0)
    xp <- seq(0, max(x, point[1])+xby, by = xby)
  if (max(y)>0 | point[2]>0)
    yp <- seq(0, max(y, point[2])+yby, by = yby)
  if (min(x)<0 | point[1]<0)
    xn <- seq(0, min(x, point[1])-xby, by = -xby)
  if (min(y)<0 | point[2]<0)
    yn <- seq(0, min(y, point[2])-yby, by = -yby)
  xbr <- c(rev(xn[-1]), xp)
  ybr <- c(rev(yn[-1]), yp)
  xhist <- hist(x, plot = FALSE, br = xbr, freq = FALSE)
  yhist <- hist(y, plot = FALSE, br = ybr, freq = FALSE)
  if (o.include) {
    xlim <- c(min(x, 0, point[1])-xr*0.05, max(x, 0, 
                                               point[1])+xr*0.05)
    ylim <- c(min(y, 0, point[2])-yr*0.05, max(y, 0, 
                                               point[2])+yr*0.05)
  }
  else {
    xlim <- c(min(x, point[1])-xr*0.05, max(x, 
                                            point[1])+xr*0.05)
    ylim <- c(min(y, point[2])-yr*0.05, max(y, 
                                            point[2])+yr*0.05)
  }
  xhistlim <- c(0, max(xhist$density)*1.05)
  yhistlim <- c(0, max(yhist$density)*1.05)
  
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  plot.default(min(x), min(y), type = "n", xlab = "", ylab = "", 
               xaxt = "n", yaxt = "n", xlim = xlim, ylim = ylim, 
               xaxs = "i", yaxs = "i", frame.plot = FALSE)
  abline(v = xbr, col = grey(0.9))
  abline(h = ybr, col = grey(0.9))
  abline(h = 0)
  abline(v = 0)
  if(points)
    points(x, y, pch = pch, cex = cex)
  if(kernel) {
    if (missing(h)) 
      h <- c(bandwidth.nrd(x), bandwidth.nrd(y))
    dens <- kde2d(x, y, h = h, lims = c(xlim, ylim))
    contour(dens, drawlabels = FALSE, col = col, add = TRUE)
  }
  lines(c(point[1], xlim[2]), rep(point[2], 2), lty = 3)
  lines(rep(point[1], 2), c(point[2], ylim[2]), lty = 3)
  if (side != "none") {
    tra <- paste(" dx = ", signif(xby, 2), " ", "\n", " dy = ", 
                 signif(yby, 2), " ", sep = "")
    wt <- strwidth(tra, cex = 1)
    ht <- strheight(tra, cex = 1) * 1.5
    xl <- par("usr")[1]
    yu <- par("usr")[4]
    yd <- par("usr")[3]
    if (side == "top") {
      rect(xl, yu - ht, xl + wt, yu, col = "white", border = 0)
      text(xl + wt/2, yu - ht/2, tra, cex = 1)
    }
    if (side == "bottom") {
      rect(xl, yd + ht, xl + wt, yd, col = "white", border = 0)
      text(xl + wt/2, yd + ht/2, tra, cex = 1)
    }
  }
  points(point[1], point[2], pch = 18, cex = cex*4, col = Pcol)
  box()
  
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  if(density) {
    xdens <- density(x)
    xhistlim <- c(0, max(xhist$density, xdens$y)*1.05)
  }
  plot.default(min(x), 0, type = "n", xlab = "", ylab = "", 
               xaxt = "n", yaxt = "n", xlim = xlim, ylim = xhistlim, 
               xaxs = "i", yaxs = "i", frame.plot = FALSE)
  rect(xbr[-length(xbr)], rep(0, br), xbr[-1], xhist$density)
  if(density)
    lines(xdens, col = col)
  abline(h = 0)
  lines(rep(point[1], 2), c(0, max(xhist$density*2/3)), 
        col = Pcol, lwd = 2)
  points(point[1], max(xhist$density*2/3), pch = 18, cex = 2, 
         col = Pcol)
  pX <- (sum(dfxy[,1] >= point[1]) + 1)/(length(dfxy[,1]) + 1)
  if (pX > 0.5)
    pX <- (sum(dfxy[,1] <= point[1]) + 1)/(length(dfxy[,1]) + 1)
  mtext(text = paste("p =", round(pX, 3)), side = 3, adj = 1, 
        line = -1)
  
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  if(density) {
    ydens <- density(y)
    yhistlim <- c(0, max(yhist$density, ydens$y)*1.05)
  }
  plot.default(min(x), 0, type = "n", xlab = "", ylab = "", 
               xaxt = "n", yaxt = "n", xlim = yhistlim, ylim = ylim, 
               xaxs = "i", yaxs = "i", frame.plot = FALSE)
  rect(rep(0, br), ybr[-length(ybr)], yhist$density, ybr[-1])
  if(density)		
    lines(ydens$y, ydens$x, col = col)
  abline(v = 0)
  lines(c(0, max(yhist$density*2/3)), rep(point[2], 2), 
        col = Pcol, lwd = 2)
  points(max(yhist$density*2/3), point[2], pch = 18, cex = 2, 
         col = Pcol)
  pY <- (sum(dfxy[,2] >= point[2]) + 1)/(length(dfxy[,2]) + 1)
  if (pY > 0.5)
    pY <- (sum(dfxy[,2] <= point[2]) + 1)/(length(dfxy[,2]) + 1)
  mtext(text = paste("p =", round(pY, 3)), side = 3, adj = 1, 
        line = -1, las = 0)
  
  plot.default(0, 0, type = "n", xlab = "", ylab = "", xaxt = "n", 
               yaxt = "n", xaxs = "i", yaxs = "i", frame.plot = FALSE)
  if (missing(sub))
    sub <- "Biplot and\n univariate\ntests"
  mtext(text = paste(sub), adj = 0.5, line = -8, cex = 1.5)
}



biv.plot <- function(dfxy, br = 10, points = TRUE, density = TRUE, 
	kernel = TRUE, o.include = FALSE, pch, cex, col, h, sub, 
	side = c("top", "bottom", "none"), ...)
{
    side <- match.arg(side)
    if (!inherits(dfxy, "data.frame")) 
      stop("dfxy should be a data frame")
    if (ncol(dfxy) < 2) 
      stop("dfxy should have at least two columns")
    if (!require(MASS) & kernel) 
      stop("This function needs the package MASS")
    if (missing(pch))
      pch <- 16
    if (missing(cex))
      cex <- 0.5
    if (missing(col))
      col <- grey(0.7)
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    lay <- layout(matrix(c(2,4,1,3),2,2, byrow = TRUE), c(3,1), 
                  c(1,3), TRUE)
    layout.show(lay)
	
    x <- dfxy[, 1]
    y <- dfxy[, 2]
    xr <- diff(range(x))
    yr <- diff(range(y))
    xby <- xr/(br-1)
    yby <- yr/(br-1)
    xp <- 0
    xn <- 0
    yp <- 0
    yn <- 0
    if (max(x)>0)
      xp <- seq(0, max(x)+xby, by = xby)
    if (max(y)>0)
      yp <- seq(0, max(y)+yby, by = yby)
    if (min(x)<0)
      xn <- seq(0, min(x)-xby, by = -xby)
    if (min(y)<0)
      yn <- seq(0, min(y)-yby, by = -yby)
    xbr <- c(rev(xn[-1]), xp)
    ybr <- c(rev(yn[-1]), yp)
    xhist <- hist(x, plot = FALSE, br = xbr, freq = FALSE)
    yhist <- hist(y, plot = FALSE, br = ybr, freq = FALSE)
    if (o.include) {
      xlim <- c(min(x, 0)-xr*0.05, max(x, 0)+xr*0.05)
      ylim <- c(min(y, 0)-yr*0.05, max(y, 0)+yr*0.05)
    }
    else {
      xlim <- c(min(x)-xr*0.05, max(x)+xr*0.05)
      ylim <- c(min(y)-yr*0.05, max(y)+yr*0.05)
    }
    xhistlim <- c(0, max(xhist$density)*1.05)
    yhistlim <- c(0, max(yhist$density)*1.05)
    
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    plot.default(min(x), min(y), type = "n", xlab = "", ylab = "", 
                 xaxt = "n", yaxt = "n", xlim = xlim, ylim = ylim, 
                 xaxs = "i", yaxs = "i", frame.plot = FALSE)
    abline(v = xbr, col = grey(0.9))
    abline(h = ybr, col = grey(0.9))
    abline(h = 0)
    abline(v = 0)
    if(points)
      points(x, y, pch = pch, cex = cex)
    if(kernel) {
      if (missing(h)) 
        h <- c(bandwidth.nrd(x), bandwidth.nrd(y))
      dens <- kde2d(x, y, h = h, lims = c(xlim, ylim))
      contour(dens, drawlabels = FALSE, col = col, add = TRUE)
    }
    if (side != "none") {
      tra <- paste(" dx = ", signif(xby, 2), " ", "\n", " dy = ", 
                   signif(yby, 2), " ", sep = "")
      wt <- strwidth(tra, cex = 1)
      ht <- strheight(tra, cex = 1) * 1.5
      xl <- par("usr")[1]
      yu <- par("usr")[4]
      yd <- par("usr")[3]
      if (side == "top") {
        rect(xl, yu - ht, xl + wt, yu, col = "white", border = 0)
        text(xl + wt/2, yu - ht/2, tra, cex = 1)
      }
      if (side == "bottom") {
        rect(xl, yd + ht, xl + wt, yd, col = "white", border = 0)
        text(xl + wt/2, yd + ht/2, tra, cex = 1)
      }
    }
    box()
    
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    if(density) {
      xdens <- density(x)
      xhistlim <- c(0, max(xhist$density, xdens$y)*1.05)
    }
    plot.default(min(x), 0, type = "n", xlab = "", ylab = "", 
                 xaxt = "n", yaxt = "n", xlim = xlim, ylim = xhistlim, 
                 xaxs = "i", yaxs = "i", frame.plot = FALSE)
    rect(xbr[-length(xbr)], rep(0, br), xbr[-1], xhist$density)
    if(density)
      lines(xdens, col = col)
    abline(h = 0)
    
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    if(density) {
		ydens <- density(y)
		yhistlim <- c(0, max(yhist$density, ydens$y)*1.05)
              }
    plot.default(min(x), 0, type = "n", xlab = "", ylab = "", 
                 xaxt = "n", yaxt = "n", xlim = yhistlim, ylim = ylim, 
                 xaxs = "i", yaxs = "i", frame.plot = FALSE)
    rect(rep(0, br), ybr[-length(ybr)], yhist$density, ybr[-1])
    if(density)		
      lines(ydens$y, ydens$x, col = col)

    plot.default(0, 0, type = "n", xlab = "", ylab = "", xaxt = "n", 
                 yaxt = "n", xaxs = "i", yaxs = "i", frame.plot = FALSE)
    if (missing(sub))
      sub <- "Biplot and \nmarginals\ndistributions"
    mtext(text = paste(sub), adj = 0.5, line = -8, cex = 1.5)
  }


enfa <- function (tab, pr, scannf = TRUE, nf = 1) 
{
    call <- match.call()
    if (any(is.na(tab))) 
        stop("na entries in table")
    if (!is.vector(pr))
        stop("pr should be a vector")
    row.w <- rep(1, nrow(tab))/nrow(tab)
    f1 <- function(v) sum(v * row.w)/sum(row.w)
    f2 <- function(v) sqrt(sum(v * v * row.w)/sum(row.w))
    center <- apply(tab, 2, f1)
    tab <- sweep(tab, 2, center)
    norm <- apply(tab, 2, f2)
    norm[norm < 1e-08] <- 1
    tab <- as.matrix(sweep(tab, 2, norm, "/"))
    lw <- pr/sum(pr)
    Rg <- crossprod(tab)/nrow(tab)
    ZtQ <- apply(tab, 2, function(x) x * lw)
    Rs <- crossprod(ZtQ, tab)
    mar <- colSums(ZtQ)
    m <- sum(mar^2)
    eRs <- eigen(Rs)
    Rs12 <- eRs$vectors %*% diag(eRs$values^(-1/2)) %*% t(eRs$vectors)
    z <- Rs12 %*% mar
    y <- z/as.numeric(sqrt(crossprod(z)))
    W <- Rs12 %*% Rg %*% Rs12
    H <- (diag(ncol(tab)) - y %*% t(y)) %*% W %*% (diag(ncol(tab)) - 
        y %*% t(y))
    s <- eigen(H)$values[-ncol(tab)]
    if (scannf) {
        barplot(s)
        cat("Select the number of specialization axes: ")
        nf <- as.integer(readLines(n = 1))
    }
    if (nf <= 0 | nf > (ncol(tab) - 1)) 
        nf <- 1
    co <- matrix(nrow = ncol(tab), ncol = nf + 1)
    co[, 1] <- mar
    co[, 2:(nf + 1)] <- (Rs12 %*% eigen(H)$vectors)[, 1:nf]
    f3 <- function(i) co[, i]/sqrt(crossprod(co[, i])/length(co[, 
        i]))
    c1 <- matrix(unlist(lapply(1:(nf + 1), f3)), ncol(tab))
    li <- data.frame(tab %*% c1[, 1:(nf + 1)])
    f3 <- function(i) li[, i]/sqrt(crossprod(li[, i])/length(li[, 
        i]))
    l1 <- matrix(unlist(lapply(1:(nf + 1), f3)), nrow(tab))
    co <- data.frame(co)
    c1 <- data.frame(c1)
    l1 <- data.frame(l1)
    names(co) <- c("Mar", paste("Spe", (1:nf), sep = ""))
    row.names(co) <- dimnames(tab)[[2]]
    names(c1) <- c("Mar", paste("Spe", (1:nf), sep = ""))
    row.names(c1) <- dimnames(tab)[[2]]
    names(li) <- c("Mar", paste("Spe", (1:nf), sep = ""))
    names(l1) <- c("Mar", paste("Spe", (1:nf), sep = ""))
    enfa <- list(call = call, tab = data.frame(tab), pr = pr, 
        nf = nf, m = m, s = s, lw = lw, li = li, l1 = l1, co = co, 
        c1 = c1, mar = mar)
    class(enfa) <- "enfa"
    return(invisible(enfa))
}

data2enfa <- function (kasc, pts) 
{
    if (!inherits(kasc, "kasc")) 
        stop("should be an object of class \"kasc\"")
    if (ncol(pts) != 2) 
        stop("pts should have 2 columns")
    attr <- storemapattr(kasc)
    tab <- kasc2df(kasc)
    index <- tab$index
    tab <- tab$tab
    pr <- as.vector(count.points(pts, kasc))[index]
    dataenfa <- list(tab = data.frame(tab), pr = pr, 
        index = index, attr = attr)
    class(dataenfa) <- "dataenfa"
    return(invisible(dataenfa))
}



hist.enfa <- function (x, scores = TRUE, type = c("h", "l"),
                       adjust = 1, Acol, Ucol, 
                       Aborder, Uborder, Alwd = 1, Ulwd = 1, ...)
{
  type <- match.arg(type)
  if (!inherits(x, "enfa")) 
    stop("Object of class 'enfa' expected")
  if (scores)
    tab <- x$l1
  else
    tab <- x$tab
  pr <- x$pr
  if (missing(Acol)) {
    Acol <- NULL
    Acolf <- "white"
    Acold <- "black"
  }
  else {
    Acold <- Acol
    Acolf <- Acol
  }
  if (missing(Aborder)) 
    Aborder <- "black"
  if (missing(Ucol)) {
    Ucol <- gray(0.8)
    Ucold <- gray(0.8)
  }
  else
    Ucold <- Ucol
  if (missing(Uborder)) 
    Uborder <- gray(0.8)
  clas <- rep("", ncol(tab))
  for (j in 1:ncol(tab)) {
    w1 <- "q"
    if (is.factor(tab[, j])) 
      w1 <- "f"
    clas[j] <- w1
  }
  if (any(clas == "f") & type == "l") 
    warning("Type = 'l' is not possible for factors, type = 'h' used instead.\n")
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar = c(0.5, 0.5, 2, 0.5))
  par(mfrow = rev(n2mfrow(ncol(tab))))
  f1 <- function(j) {
    tmpU <- rep(tab[, j], pr)
    tmpA <- tab[, j]
    name <- names(tab)[j]
    if (clas[j] == "f") {
      par(mar = c(3, 0.5, 2, 0.5))
      mat <- t(cbind(table(tmpA), table(tmpU)))
      mat <- lapply(1:2, function(i) mat[i, ]/sum(mat[i, 
                                                      ]))
      mat <- rbind(mat[[1]], mat[[2]])
      max <- max(mat)
      max <- max + max/20
      ylim <- c(0, max)
      barplot(mat, col = c(Acolf, Ucol), border = c(Aborder, Uborder), 
              ylim = ylim, main = name, ylab = NULL, axes = FALSE, 
              beside = TRUE, ...)
      par(mar = c(0.5, 0.5, 2, 0.5))
    }
    else {
      if (type == "h") {
        xrange <- range(tmpA)
        H <- hist(tmpU, plot = FALSE, br = seq(min(xrange), 
					max(xrange), length = 15))
        G <- hist(tmpA, plot = FALSE, br = seq(min(xrange), 
					max(xrange), length = 15))
        yrange <- c(0, max(H$density, G$density))
        plot(H, freq = FALSE, col = Ucol, border = Uborder, 
             xlim = xrange, ylim = yrange, main = name, xlab = NULL, 
             ylab = "Density", axes = FALSE, ...)
        plot(G, freq = FALSE, col = Acol, border = Aborder, add = TRUE)
      }
      if (type == "l") {
        densA <- density(tmpA, adjust = adjust)
        densU <- density(tmpU, adjust = adjust, from = min(densA$x), 
                         to = max(densA$x))
        max <- max(densU$y, densA$y)
        max <- max + max/20
        ylim <- c(0, max)
        plot(densU, col = Ucol, ylim = ylim, type = "l", 
             lwd = Ulwd, main = name, xlab = NULL, ylab = "Density", 
             axes = FALSE, ...)
        lines(rep(mean(tmpU), 2), c(0, densU$y[512 - sum(densU$x > 
                                                         mean(tmpU))]),
              col = Ucol, lty = 2, lwd = Ulwd)
        lines(densA, col = Acold, lwd = Alwd)
        lines(rep(mean(tmpA), 2), c(0, densA$y[512 - sum(densA$x > 
                                                         mean(tmpA))]),
              col = Acold, lty = 2, lwd = Alwd)
      }
    }
    box()
  }
  lapply(1:ncol(tab), f1)
  return(invisible(NULL))
}


hist.kasc <- function (x, type = c("h", "l"), adjust = 1, col = "blue", ...)
{
  type <- match.arg(type)
  if (!inherits(x, "kasc")) 
    stop("should be an object of class \"kasc\"")
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar = c(0.5,0.5,2,0.5))

  tab <- x
  clas <- rep("", ncol(tab))
  for (j in 1:ncol(tab)) {
    w1 <- "q"
    if (is.factor(tab[, j])) 
      w1 <- "f"
    clas[j] <- w1
  }

  par(mfrow = rev(n2mfrow(ncol(tab))))

  f1 <- function(j) {
    tmpZ <- tab[,j]
    name <- names(tab)[j]
    if (clas[j] == "f") {
      par(mar = c(3,0.5,2,0.5))
      max <- max(table(tmpZ))
      max <- max + max/20
      ylim <- c(0, max)
      
      barplot(unclass(summary(tmpZ[!is.na(tmpZ)])), ylim = ylim, border = col, 
              main = name, ylab = NULL, axes = FALSE, ...)
      par(mar = c(0.5,0.5,2,0.5))
    }
    else {
      xrange <- range(tmpZ)
      G <- hist(tmpZ, plot = FALSE)
      plot(G, freq = FALSE, border = col, main = name, 
           xlab = NULL, ylab = NULL, axes = FALSE, ...)
    }
    box()
  }

  f2 <- function(j) {
    tmpZ <- tab[,j]
    name <- names(tab)[j]
    if (clas[j] == "f") {
      par(mar = c(3,0.5,2,0.5))
      max <- max(table(tmpZ))
      max <- max + max/20
      ylim <- c(0, max)
      barplot(unclass(summary(tmpZ[!is.na(tmpZ)])), ylim = ylim, border = col, 
              main = name, ylab = NULL, axes = FALSE, ...)
      par(mar = c(0.5,0.5,2,0.5))
    }
    else {
      dens <- density(tmpZ, adjust = adjust, na.rm = TRUE)
      plot(dens,  col = col, type = "l", lwd = 2, 
           main = name, xlab = NULL, ylab = "Density", 
           axes = FALSE, ...)
      mean <- mean(tmpZ, na.rm = TRUE)
      lines(rep(mean,2),
            c(0,dens$y[512-sum(dens$x>mean)]),
            col = col, lty = 2, lwd = 2)
    }
    box()
  }

  if (type == "h")
    lapply (1:ncol(tab), f1)
  if (type == "l") {
    if (any(clas == "f"))
      warning("Type = 'l' is not possible for factors, type = 'h' used instead.\n")
    lapply (1:ncol(tab), f2)
  }
  return(invisible(NULL))
}





histniche <- function (kasc, pts, type = c("h", "l"), adjust = 1, Acol, Ucol, 
                       Aborder, Uborder, Alwd = 1, Ulwd = 1, ...)
{
  type <- match.arg(type)
  if (!inherits(kasc, "kasc")) 
    stop("should be an object of class \"kasc\"")
  if (ncol(pts) != 2) 
    stop("pts should have 2 columns")
  tab <- kasc2df(kasc)
  index <- tab$index
  tab <- tab$tab
  pr <- as.vector(count.points(pts, kasc))[index]
  if (missing(Acol)) {
    Acol <- NULL
    Acolf <- "white"
    Acold <- "black"
  }
  else {
    Acold <- Acol
    Acolf <- Acol
  }
  if (missing(Aborder)) 
    Aborder <- "black"
  if (missing(Ucol)) {
    Ucol <- gray(0.8)
    Ucold <- gray(0.8)
  }
  else
    Ucold <- Ucol
  if (missing(Uborder)) 
    Uborder <- gray(0.8)
  clas <- rep("", ncol(tab))
  for (j in 1:ncol(tab)) {
    w1 <- "q"
    if (is.factor(tab[, j])) 
      w1 <- "f"
    clas[j] <- w1
  }
  if (any(clas == "f") & type == "l") 
    warning("Type = 'l' is not possible for factors, type = 'h' used instead.\n")
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar = c(0.5, 0.5, 2, 0.5))
  par(mfrow = rev(n2mfrow(ncol(tab))))
  f1 <- function(j) {
    tmpU <- rep(tab[, j], pr)
    tmpA <- tab[, j]
    name <- names(tab)[j]
    if (clas[j] == "f") {
      par(mar = c(3, 0.5, 2, 0.5))
      mat <- t(cbind(table(tmpA), table(tmpU)))
      mat <- lapply(1:2, function(i) mat[i, ]/sum(mat[i,]))
      mat <- rbind(mat[[1]], mat[[2]])
      max <- max(mat)
      max <- max + max/20
      ylim <- c(0, max)
      barplot(mat, col = c(Acolf, Ucol), border = c(Aborder, Uborder), 
              ylim = ylim, main = name, ylab = NULL, axes = FALSE, 
              beside = TRUE, ...)
      par(mar = c(0.5, 0.5, 2, 0.5))
    }
    else {
      if (type == "h") {
        xrange <- range(tmpA)
        H <- hist(tmpU, plot = FALSE, br = seq(min(xrange), 
					max(xrange), length = 15))
        G <- hist(tmpA, plot = FALSE, br = seq(min(xrange), 
					max(xrange), length = 15))
        yrange <- c(0, max(H$density, G$density))
        plot(H, freq = FALSE, col = Ucol, border = Uborder, 
             xlim = xrange, ylim = yrange, main = name, xlab = NULL, 
             ylab = "Density", axes = FALSE, ...)
        plot(G, freq = FALSE, col = Acol, border = Aborder, add = TRUE)
      }
      if (type == "l") {
        densA <- density(tmpA, adjust = adjust)
        densU <- density(tmpU, adjust = adjust, from = min(densA$x), 
                         to = max(densA$x))
        max <- max(densU$y, densA$y)
        max <- max + max/20
        ylim <- c(0, max)
        plot(densU, col = Ucol, ylim = ylim, type = "l", 
             lwd = Ulwd, main = name, xlab = NULL, ylab = "Density", 
             axes = FALSE, ...)
        lines(rep(mean(tmpU), 2), c(0, densU$y[512 - sum(densU$x > 
                                                         mean(tmpU))]),
              col = Ucol, lty = 2, lwd = Ulwd)
        lines(densA, col = Acold, lwd = Alwd)
        lines(rep(mean(tmpA), 2), c(0, densA$y[512 - sum(densA$x > 
                                                         mean(tmpA))]),
              col = Acold, lty = 2, lwd = Alwd)
      }
    }
    box()
  }
  lapply(1:ncol(tab), f1)
  return(invisible(NULL))
}




niche.test <- function (kasc, points, nrep = 999, o.include = TRUE, ...) 
{
    if (!inherits(kasc, "kasc")) 
        stop("should be an object of class \"kasc\"")
    if (ncol(points) != 2) 
        stop("points should have 2 columns")
    nrep <- nrep + 1
    toto <- join.kasc(points, kasc)
    tutu <- apply(toto, 1, function(x) any(is.na(x)))
    if (sum(tutu) > 0) 
        stop("points outside the study area")
    litab <- kasc2df(kasc)
    dude <- dudi.mix(litab$tab, scannf = FALSE)
    cw <- dude$cw
    kasc <- df2kasc(dude$tab, litab$index, kasc)
    asc <- getkasc(kasc, names(kasc)[1])
    coo <- getXYcoords(kasc)
    rc <- lapply(coo, range)
    kasc <- as.matrix(kasc)
    kasc[is.na(kasc)] <- -9999
    asc[is.na(asc)] <- -9999
    xp <- as.matrix(points)
    toto <- .C("randmargtolpts", as.double(t(xp)), as.double(rc$x), 
        as.double(rc$y), as.double(t(asc)), as.double(cw), as.double(t(kasc)), 
        as.double(coo$x), as.double(coo$y), as.double(attr(asc, 
            "cellsize")), double(nrep), double(nrep), as.integer(nrep), 
        as.integer(nrow(asc)), as.integer(ncol(asc)), as.integer(ncol(kasc)), 
        as.integer(nrow(xp)), PACKAGE = "adehabitat")
    mar <- toto[[10]]
    tol <- toto[[11]]
    dfxy <- data.frame(marginalite = mar, tolerance = tol)[-1,]
    obs <- c(mar[1], tol[1])
    biv.test(dfxy, obs, sub = "Tests of\nmarginality\nand tolerance", 
             o.include = o.include, ...)
    return(invisible(list(dfxy = dfxy, obs = obs)))
}


predict.enfa <- function (object, index, attr, nf, ...) 
{
    if (!inherits(object, "enfa")) 
        stop("should be an object of class \"enfa\"")
    if ((missing(nf)) || (nf > object$nf)) 
        nf <- object$nf
    Zli <- object$li[, 1:(nf + 1)]
    f1 <- function(x) rep(x, object$pr)
    Sli <- apply(Zli, 2, f1)
    m <- apply(Sli, 2, mean)
    cov <- t(as.matrix(Sli)) %*% as.matrix(Sli)/nrow(Sli)
    maha <- mahalanobis(Zli, center = m, cov = cov)
    map <- getkasc(df2kasc(data.frame(toto = maha, tutu = maha), 
        index, attr), "toto")
    return(invisible(map))
}

print.dataenfa <- function (x, ...)
{
    if (!inherits(x, "dataenfa")) 
        stop("Object of class 'dataenfa' expected")
    cat("Data ENFA\n")
    cat("\n List of 4 elements:\n\n")
    sumry <- array("", c(1, 4), list(1, c("data.frame", "nrow", "ncol", 
        "content")))
    sumry[1, ] <- c("$tab", nrow(x$tab), ncol(x$tab), "table of pixels")
    class(sumry) <- "table"
    print(sumry)
    cat("\n")
    sumry <- array("", c(2, 3), list(1:2, c("vector", "length", "content")))
    sumry[1, ] <- c("$pr", length(x$pr), "vector of presence")
    sumry[2, ] <- c("$index", length(x$index), "position of the rows")
    class(sumry) <- "table"
    print(sumry)
    cat("\n$attr: attributes of the initial kasc\n")
}


print.enfa <- function (x, ...) 
{
  if (!inherits(x, "enfa")) 
    stop("Object of class 'enfa' expected")
  cat("ENFA")
  cat("\n$call: ")
  print(x$call)
  cat("\nmarginality: ")
  cat(signif(x$m, 4))
  cat("\neigen values of specialization: ")
  l0 <- length(x$s)
  cat(signif(x$s, 4)[1:(min(5, l0))])
  if (l0 > 5) 
    cat(" ...")
  cat("\n$nf:", x$nf, "axis of specialization saved")
  cat("\n")
  cat("\n")
  sumry <- array("", c(4, 4), list(1:4, c("vector", "length", 
                                          "mode", "content")))
  sumry[1, ] <- c("$pr", length(x$pr), mode(x$pr), "vector of presence")
  sumry[2, ] <- c("$lw", length(x$lw), mode(x$lw), "row weights")
  sumry[3, ] <- c("$mar", length(x$mar),
                  mode(x$mar), "coordinates of the marginality vector")
  sumry[4, ] <- c("$s", length(x$s),
                  mode(x$s), "eigen values of specialization")
  class(sumry) <- "table"
  print(sumry)
  cat("\n")
  sumry <- array("", c(5, 4), list(1:5, c("data.frame", "nrow", 
                                          "ncol", "content")))
  sumry[1, ] <- c("$tab", nrow(x$tab), ncol(x$tab), "modified array")
  sumry[2, ] <- c("$li", nrow(x$li), ncol(x$li), "row coordinates")
  sumry[3, ] <- c("$l1", nrow(x$l1), ncol(x$l1), "row normed scores")
  sumry[4, ] <- c("$co", nrow(x$co), ncol(x$co), "column coordinates")
  sumry[5, ] <- c("$c1", nrow(x$c1), ncol(x$c1), "column normed scores")
  class(sumry) <- "table"
  print(sumry)
  if (length(names(x)) > 12) {
    cat("\nother elements: ")
    cat(names(x)[13:(length(x))], "\n")
  }
}



randtest.enfa<-function(xtest, nrepet=999, ...)
  {
    if (!inherits(xtest,"enfa"))
      stop("should be an object of class \"enfa\"")
    tab<-as.matrix(xtest$tab)
    pr<-xtest$pr
    res<-.C("randenfar", as.double(t(tab)), as.double(pr),
            as.integer(ncol(tab)), as.integer(nrow(tab)),
            as.integer(nrepet), double(nrepet), PACKAGE="adehabitat")[[6]]
    return(as.randtest(res, xtest$s[1], call = match.call()))
  }



scatter.enfa <- function(x, xax = 1, yax = 2, pts = FALSE, nc = TRUE, 
                         percent = 95, clabel = 1,
                         side = c("top", "bottom", "none"),
                         Adensity, Udensity, Aangle, Uangle,
                         Aborder, Uborder, Acol, Ucol, Alty,
                         Ulty, Abg, Ubg, Ainch, Uinch, ...)
{
  side <- match.arg(side)
  if (!inherits(x, "enfa")) 
    stop("Object of class 'enfa' expected")
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  x1 <- x$li[, xax]
  x1 <- c(x1 - diff(range(x1)/50), x1 + diff(range(x1))/50)
  xlim <- range(x1)
  y1 <- x$li[, yax]
  y1 <- c(y1 - diff(range(y1)/50), y1 + diff(range(y1))/50)
  ylim <- range(y1)
  pmar <- t(x$mar) %*% as.matrix(x$c1[, 1:(x$nf + 1)])
  scatterutil.base(dfxy = x$li[, c(xax, yax)], xax = 1, yax = 2, 
                   xlim = xlim, ylim = ylim, grid = TRUE, addaxes = FALSE, 
                   cgrid = 1, include.origin = TRUE, origin = c(0, 0),
                   sub = "", csub = 1.25, possub = "bottomleft",
                   pixmap = NULL, contour = NULL, 
                   area = NULL, add.plot = FALSE)
  if (pts) {
    if (missing (Acol))
      Acol <- gray(0.8)
    if (missing (Ucol))
      Ucol <- "black"
    if (missing (Abg))
      Abg <- gray(0.8)
    if (missing (Ubg))
      Ubg <- "black"
    if (missing (Ainch))
      Ainch <- 0.03
    if (missing (Uinch))
      Uinch <- Ainch*max(x$pr)
    symbols(x$li[, c(xax, yax)], circles = rep(1, length(x$pr)),
            fg = Acol, bg = Abg, inches = Ainch, add = TRUE)
    symbols(x$li[x$pr>0, c(xax, yax)], circles = x$pr[x$pr>0],
            fg = Ucol, bg = Ubg, inches = Uinch, add = TRUE)
    abline(v=0)
    abline(h=0)
    if (nc)
      symbols(pmar, circles = 1, fg = "black", bg = "white",
              inches = Ainch*2, add = TRUE)
  }
  else {
    if (missing(Adensity))
      Adensity <- NULL
    if (missing(Udensity))
      Udensity <- NULL
    if (missing(Aangle))
      Aangle <- 45
    if (missing(Uangle))
      Uangle <- 45
    if (missing(Aborder))
      Aborder <- NULL
    if (missing(Uborder))
      Uborder <- NULL
    if (missing(Acol))
      Acol <- gray(0.95)
    if (missing(Ucol))
      Ucol <- gray(0.6)
    if (missing(Alty))
      Alty <- NULL
    if (missing(Ulty))
      Ulty <- NULL
                                        #		if (missing (Ucex))
                                        #			Ucex <- 1
    mcpA <- mcp(x$li[, c(xax, yax)], id = rep(1, dim(x$li)[1]),
                percent = percent)
    mcpU <- mcp(x$li[rep(1:length(x$pr), x$pr), c(xax, yax)],
                id = rep(1, sum(enfa1$pr)), percent = percent)
    polygon(mcpA[, 2:3], density = Adensity, angle = Aangle,
            border = Aborder, col = Acol, lty = Alty)
    polygon(mcpU[, 2:3], density = Udensity, angle = Uangle,
            border = Uborder, col = Ucol, lty = Ulty)
    abline(v=0)
    abline(h=0)
    if (nc)
      points(pmar, pch = 21, bg = "white", cex = 1.5)
  }
  dfarr <- x$c1[, c(xax, yax)]
  born <- par("usr")
  k1 <- min(dfarr[, 1])/born[1]
  k2 <- max(dfarr[, 1])/born[2]
  k3 <- min(dfarr[, 2])/born[3]
  k4 <- max(dfarr[, 2])/born[4]
  k <- c(k1, k2, k3, k4)
  dfarr <- 0.75 * dfarr/max(k)
  s.arrow(dfarr, clabel = clabel, addaxes = FALSE, add.plot = TRUE)
  if (xax == 1) 
    xax <- "mar"
  else xax <- paste("sp", xax - 1)
  if (yax == 1) 
    yax <- "mar"
  else yax <- paste("sp", yax - 1)
  if (side != "none") {
    tra <- paste(" xax =", xax, "\n yax =", yax)
    wt <- strwidth(tra, cex = 1)
    ht <- strheight(tra, cex = 1) * 1.5
    xl <- par("usr")[1]
    yu <- par("usr")[4]
    yd <- par("usr")[3]
    if (side == "top") {
      rect(xl, yu - ht, xl + wt, yu, col = "white", border = 0)
      text(xl + wt/2, yu - ht/2, tra, cex = 1)
    }
    if (side == "bottom") {
      rect(xl, yd + ht, xl + wt, yd, col = "white", border = 0)
      text(xl + wt/2, yd + ht/2, tra, cex = 1)
    }
    
  }
  box()
}




## fit the nearest-neighbor convex hull

NNCH<-function(xy, id=NULL, k=10, unin = c("m", "km"),
               unout = c("ha", "km2", "m2"))
  {
    if (ncol(xy)!=2)
      stop("xy should have two columns")
    if (is.null(id))
      id<-rep(1,nrow(xy))
    id<-factor(id)
    
    if (k>=min(table(id)))
      stop("too large number of neighbors")
    if (nrow(xy)!=length(id))
      stop("id should have the same length as xy")
      if (min(table(id))<5)
    stop("At least 5 relocations are required to fit an home range")

    id <- id[!is.na(xy[, 1])]
    xy <- xy[!is.na(xy[, 1]), ]
    id <- id[!is.na(xy[, 2])]
    xy <- xy[!is.na(xy[, 2]), ]
    unin <- match.arg(unin)
    unout <- match.arg(unout)
    class(xy[,1])<-"double"
    class(xy[,2])<-"double"
    if (!require(gpclib)) 
      stop("package gpclib required")
    res<-list()
    
    for (kk in 1:nlevels(id)) {
      xyt<-xy[id==levels(id)[kk],]
      
      li<-list()
      li2<-list()
      lin<-list()
      lin2<-list()
      ar<-0
      
      dij<-as.matrix(dist(xyt))
      idt<-1:nrow(dij)
      
      for (i in 1:nrow(xyt)) {
        iid<-idt[order(dij[i,])][1:k]
        xytmp<-xyt[iid,]
        ch<-chull(xytmp[,1], xytmp[,2])
        li[[i]]<-as(xytmp[ch,], "gpc.poly")
        lin[[i]]<-iid
      }
    
      aa<-unlist(lapply(li, area.poly))
      li<-li[order(aa)]
      lin<-lin[order(aa)]
      idbis<-idt[order(aa)]
      li2[[1]]<-li[[1]]
      lin2[[1]]<-lin[[1]]
      
      for (i in 2:length(li)) {
        li2[[i]]<-union(li2[[i-1]], li[[i]])
        lin2[[i]]<-unique(c(lin2[[i-1]], lin[[i]]))
      }
      
      n<-unlist(lapply(lin2, length))/nrow(xyt)
      ar<-unlist(lapply(li2, area.poly))

      cons<-1:length(ar)
      
      if (unin == "m") {
        if (unout == "ha") 
          ar <- ar/10000
        if (unout == "km2") 
          ar <- ar/1e+06
      }
      if (unin == "km") {
        if (unout == "ha") 
          ar <- ar * 100
        if (unout == "m2") 
          ar <- ar * 1e+06
      }

      
      names(li2)<-round(n*100)
      area<-data.frame(levels=round(n*100,2), area=ar)
      dup<-!duplicated(area)
      area=area[dup,]
      row.names(area)<-1:nrow(area)
      res[[levels(id)[kk]]]<-list(area=area,
                                  polygons=li2[dup], xy=xyt)
      

    }
    attr(res, "units") <- unout
    class(res)<-"NNCH"
    return(res)
  }

## print method

print.NNCH<-function(x, ...)
  {
    cat("***********************************************\n")
    cat("***\n")
    cat("***      Nearest-neighbor convex hull\n\n")
    cat(paste("Home range available for", length(x), "animals:\n"))
    print(names(x), ...)
    cat("\n\nEach animal is a component of the object. For each animal,")
    cat("\nthe following information is available:\n")
    cat("\n$area:       home-range size estimated at various levels")
    cat("\n$polygons:   objects of class \"gpc.poly\" storing the home-range limits")
    cat("\n$xy:         the relocations\n\n")
  }



## compute the home-range size
NNCH.area<-function(x, percent=c(95,90,80,70,60,50,40,30,20,10))
  {
    if (!inherits(x, "NNCH"))
      stop("x should be of class \"NNCH\"")

    res<-matrix(0,nrow=length(percent), ncol=length(x))

    for (kk in 1:length(x)) {
      for (i in 1:length(percent))
        res[i,kk]<-x[[kk]]$area[max(which(x[[kk]]$area$levels<=percent[i])),2]
    }
    res<-as.data.frame(res)
    row.names(res)<-percent
    names(res)<-names(x)
    class(res) <- c("hrsize", "data.frame")
    attr(res, "units") <- attr(x, "units")
    return(res)
  }


## display the home ranges

plot.NNCH<-function(x, which = names(x), add.points=TRUE, pch=21,
                    bgpts="white", colpts="black",
                    cex=0.7, add=FALSE, same4all=TRUE, border=NA, ...)
  {
    if (!inherits(x, "NNCH"))
      stop("x should be of class \"NNCH\"")
    x<-x[which]
    if (length(x)>1) {
      opar<-par(mfrow=n2mfrow(length(x)))
      on.exit(par(opar))
    }
    
    if (same4all) {
      xxx<-do.call("rbind", lapply(x, function(x) x$xy))
      rx<-range(xxx[,1])
      ry<-range(xxx[,2])
    }
    
    for (kk in names(x)) {
      if (!same4all) {
        rx<-range(x[[kk]]$xy[,1])
        ry<-range(x[[kk]]$xy[,2])
      }
      if(!add) {
        if (length(x)>1) 
          plot(x[[kk]]$xy, ty="n", asp=1, main=kk,
               xlim=rx, ylim=ry,...)
        if (length(x)==1) 
          plot(x[[kk]]$xy, ty="n", asp=1, xlim=rx,
               ylim=ry,...)
      }
      
      gr<-grey(x[[kk]]$area$levels/100)
      li2<-x[[kk]]$polygons
      for (i in length(li2):1)
        plot(li2[[i]], poly.args=list(col=gr[i], border=border), add=TRUE)
      if (add.points)
        points(x[[kk]]$xy, pch=pch, bg=bgpts, col=colpts, cex=cex)
    }
  }


neighNNCH <- function(xy, id=NULL, rangek, percent=95,
                      unin = c("m", "km"),
                      unout = c("ha", "km2", "m2"))
  {
    if (max(rangek)>=nrow(xy))
      stop("too large number of neighbors")
    if (ncol(xy)!=2)
      stop("xy should have two columns")
    kk <- do.call("rbind", lapply(rangek, function(x) {
      unlist(NNCH.area(NNCH(xy=xy, id=id, k=x, unin, unout), percent)[1,])
    }))
    rownames(kk) <- rangek
    colnames(kk) <- levels(id)
    class(kk) <- c("ngNNCH")
    return(kk)
  }


plot.ngNNCH <- function(x, ...)
  {
    if (!inherits(x, "ngNNCH"))
      stop("x should be of class \"ngNNCH\"")
    xx<-as.numeric(row.names(x))
    if (ncol(x)!=1) {
      opar <- par(mfrow=n2mfrow(ncol(x)))
      on.exit(par(opar))
    }
    if (ncol(x)>1)
      lapply(1:ncol(x), function(y) plot(as.numeric(rownames(x)), x[,y],
                                         main=colnames(x)[y],
                                         xlab="Number of neighbors",
                                         ylab="Home-range size",
                                         pch = 16, ty="b",...))
    if (ncol(x)==1)
      plot(as.numeric(rownames(x)), x[,1], xlab="Number of neighbors",
           ylab="Home-range size", pch = 16, ty="b",...)
    
  }


getverticesNNCH <- function(x, percent = 95)
  {
    if (!inherits(x, "NNCH"))
      stop("x should be of class \"NNCH\"")
    res<-list()
    for (kk in 1:length(x))
      res[[kk]]<-x[[kk]]$polygons[[max(which(x[[kk]]$area$levels<=percent))]]
    names(res)<-names(x)
    class(res)<-"NNCHver"
    return(res)
  }

plot.NNCHver<-function(x, which = names(x),
                       colpol = NA,
                       colborder = rep("black", length(x)),
                       lwd = 2, add = FALSE, ...)
  {
    if (!inherits(x, "NNCHver"))
      stop("y should be of class NNCHver")
    xt<-unlist(lapply(x, function(x) lapply(attr(x, "pts"), function(i) i$x)))
    yt<-unlist(lapply(x, function(x) lapply(attr(x, "pts"), function(i) i$y)))

    if (!add)
      plot(xt, yt, asp=1, ty = "n", ...)

    res<-x[which]
    
    lapply(1:length(res), function(x) plot(res[[x]],
                                           poly.args = list(col = colpol[x],
                                           border = colborder[x], lwd = lwd,
                                           ...), add=TRUE))
    invisible(NULL)
    
  }

NNCH.rast<-function(y, w)
  {
    x <- y
    if (!inherits(x, "NNCHver"))
      stop("x should be of class NNCHver")
    if (inherits(w, "kasc"))
      w <- getkasc(w, names(w)[1])
    if (!inherits(w, "asc"))
      stop("w should be of class asc or kasc")

    ## rastérisation des polygones
    hol<-lapply(lapply(x, function(y) attr(y, "pts")),
                function(y) unlist(lapply(y, function(z) z$hole)))
    xt<-lapply(lapply(x, function(y) attr(y, "pts")),
                function(y) lapply(y, function(z) z$x))
    yt<-lapply(lapply(x, function(y) attr(y, "pts")),
                function(y) lapply(y, function(z) z$y))
    res <- list()
    
    for (i in 1:length(hol)) {
      rr<-lapply(1:length(xt[[i]]),
                 function(j) mcp.rast(data.frame(x=xt[[i]][[j]],
                                                 y = yt[[i]][[j]]), w))
      rr <- lapply(rr, function(o) {o[is.na(o)] <- 0; return(o)})
      if (hol[[i]][1]) {
        ee <- -rr[[1]]
      } else {
        ee <- rr[[1]]
      }
      if (length(rr) >1){
        for (j in 2:length(rr)) {
          if (hol[[i]][j])
            ee <- ee - rr[[j]]
          if (!hol[[i]][j])
            ee <- ee + rr[[j]]
        }
      }
      ee[ee==0] <- NA
      res[[i]] <- getascattr(w, ee)
    }
    names(res) <- names(x)
    if (length(res)==1)  {
      res<-res[[1]]
    } else {
      res <- as.kasc(res)
    }
    return(res)
  }


### A better display of areas

plot.area <- function(x, which=levels(x[,1]),
                      colpol = rep("green", nlevels(x[,1])),
                      colborder = rep("black", nlevels(x[,1])),
                      lwd = 2, add = FALSE, ...)
  {
    if (!inherits(x, "area"))
      stop("x should be of class \"area\"")
    if (!add)
      plot.default(x[,2:3], type = "n", asp = 1,...)
    li <- split(x[,2:3], x[,1])
    lapply(1:length(which), function(i) polygon(li[[which[i]]],
                                                col = colpol[i],
                                                border = colborder[i],
                                                lwd = lwd))
    invisible(NULL)
  }



perarea <- function(x)
  {
    if (!inherits(x, "area"))
      stop("x should be of class \"area\"")
    uu <- split(x[,2:3], x[,1])
    foo <- function(x) {
      if (!all(x[1,]==x[nrow(x),]))
        x <- rbind(x,x[nrow(x),])
      x1 <- x[-1,]
      x2 <- x[-nrow(x),]
      di <- sum(sqrt(((x2[,1]-x1[,1])^2)+((x2[,2]-x1[,2])^2)))
      return(di)
    }
    res <- unlist(lapply(uu, foo))
    names(res) <- names(uu)
    return(res)
  }
  
ararea <- function(x)
  {
    if (!inherits(x, "area"))
      stop("x should be of class \"area\"")
    if (!require(gpclib))
      stop("package gpclib needed for this function")
    uu <- split(x[,2:3], x[,1])
    foo <- function(y) {
      class(y) <- "data.frame"
      u <- area.poly(as(y, "gpc.poly"))
    }
    res <- unlist(lapply(uu, foo))
    names(res) <- names(uu)
    return(res)
  }



### kernel brownian bridge

kernelbb<- function(tr, sig1, sig2, grid = 40, same4all=FALSE, byburst=FALSE)
  {
    x <- tr
    if (!inherits(x, "traj"))
      stop("x should be of class \"traj\"")
    sorties <- list()
    gr <- grid
    xy<-x[,c("x","y")]
    sig12<-sig1^2
    sig22<-sig2^2
    h<-c(sig1, sig2)
    names(h)<-c("sig1","sig2")
    fac<-x$burst
    if (!byburst)
      fac<-x$id
    fac<-factor(fac)
    lixy<-split(x,fac)
    
    if (same4all) {
      if (length(as.vector(gr)) == 1) {
        if (!is.numeric(gr)) 
          stop("grid should be an object of class asc or a number")
        xli <- range(xy[, 1])
        yli <- range(xy[, 2])
        xli <- c(xli[1] - 0.3 * abs(xli[2] - xli[1]), xli[2] + 
                 0.3 * abs(xli[2] - xli[1]))
        yli <- c(yli[1] - 0.3 * abs(yli[2] - yli[1]), yli[2] + 
                 0.3 * abs(yli[2] - yli[1]))
        xygg <- data.frame(x = xli, y = yli)
        grid <- ascgen(xygg, nrcol = grid)
        cellsize <- attr(grid, "cellsize")
        lx <- nrow(grid) * cellsize
        ly <- ncol(grid) * cellsize
        ref <- lx
        if (ly > lx) 
          ref <- ly
        xll <- attr(grid, "xll")
        yll <- attr(grid, "yll")
        xll <- xll - lx/2
        yll <- yll - ly/2
        arajlig <- ceiling((lx/2)/cellsize)
        arajcol <- ceiling((ly/2)/cellsize)
        mrajlig <- matrix(0, ncol = ncol(grid), nrow = arajlig)
        grid <- rbind(mrajlig, grid, mrajlig)
        mrajcol <- matrix(0, ncol = arajcol, nrow = nrow(grid))
        grid <- cbind(mrajcol, grid, mrajcol)
        attr(grid, "xll") <- xll
        attr(grid, "yll") <- yll
        attr(grid, "cellsize") <- cellsize
        attr(grid, "type") <- "numeric"
        class(grid) <- "asc"
      }
    }

    for (i in 1:length(lixy)) {
      dft<-lixy[[i]]
      df<-dft[,c("x","y")]
      if (length(as.vector(gr)) == 1) {
        if (!is.numeric(gr)) 
          stop("grid should be an object of class asc or a number")
        if (!same4all) {
          grid <- matrix(0, ncol = gr, nrow = gr)
          rgx <- range(df[, 1])
          rgy <- range(df[, 2])
          lx <- rgx[2] - rgx[1]
          ly <- rgy[2] - rgy[1]
          ref <- lx
          if (ly > lx) 
            ref <- ly
          xll <- rgx[1]
          yll <- rgy[1]
          cellsize <- ref/ncol(grid)
          xll <- xll - lx/2
          yll <- yll - ly/2
          arajlig <- ceiling((lx/2)/cellsize)
          arajcol <- ceiling((ly/2)/cellsize)
          mrajlig <- matrix(0, ncol = ncol(grid), nrow = arajlig)
          grid <- rbind(mrajlig, grid, mrajlig)
          mrajcol <- matrix(0, ncol = arajcol, nrow = nrow(grid))
          grid <- cbind(mrajcol, grid, mrajcol)
          attr(grid, "xll") <- xll
          attr(grid, "yll") <- yll
          attr(grid, "cellsize") <- cellsize
          attr(grid, "type") <- "numeric"
          class(grid) <- "asc"
        }
      }
      
      xyg<-getXYcoords(grid)
      date<-as.double(dft$date)-min(as.double(dft$date))
      toto<-.C("kernelbb", as.double(t(grid)), as.double(xyg$x),
               as.double(xyg$y), as.integer(ncol(grid)),as.integer(nrow(grid)),
               as.integer(nrow(x)), as.double(sig12), as.double (sig22), 
               as.double(df$x), as.double(df$y), as.double(date),
               PACKAGE="adehabitat")
      UD <- matrix(toto[[1]], nrow = nrow(grid), byrow = TRUE)
      UD <- getascattr(grid, UD)
      sorties[[names(lixy)[i]]] <- list(UD = UD, locs = lixy[[i]],
                                        h = h, hmeth = "bb")
    }
    class(sorties) <- c("kbbhrud", "khr")
    return(sorties)
  }



schoener <- function(tr, keep, byburst=TRUE)
  {
    if (!inherits(tr, "traj"))
      stop("tr should be of class traj")
    li <- split(tr[,c("x","y")], tr$id)
    if (byburst)
      li <- split(tr, tr$burst)
    
    foo <- function(tr) {
      d <- unclass(tr$date)
      x <- tr[,c("x","y")]
      r2 <- sum(((x[,1]-mean(x[,1]))^2) +
                ((x[,2]-mean(x[,2]))^2))/(nrow(x) -1)
      diffd <- outer(d,d,"-")
      t2tmp <- as.matrix(dist(x)^2)
      cons <- diffd>keep[1]&diffd<keep[2]
      t2 <- sum(t2tmp[cons])/sum(cons)
      rat <- t2/r2
      n <- nrow(x)
      m <- sum(cons)
      return(c(rat, n, m))
    }
    
    rr <- do.call("rbind", lapply(li, foo))
    rr <- as.data.frame(rr)
    row.names(rr) <- names(li)
    names(rr) <- c("value","n","m")
    return(rr)
  }

schoener.rtest <- function(tr, keep, byburst=TRUE, nrep=500)
  {
    obs <- schoener(tr, keep, byburst)
    res <- list()
    trbis <- tr
    li <- split(tr[,c("x","y")], tr$id)
    if (byburst)
      li <- split(tr, tr$burst)
    
    foo <- function(trb) {
      x <- trb[,c("x","y")]
      r2 <- sum(((x[,1]-mean(x[,1]))^2) +
                ((x[,2]-mean(x[,2]))^2))/(nrow(x) -1)
      d <- unclass(trb$date)
      diffd <- outer(d,d,"-")
      cons <- diffd>keep[1]&diffd<keep[2]
      m <- sum(cons)
      
      foobis <- function(i) {
        x <- trb[sample(1:nrow(trb), replace=FALSE),c("x","y")]
        t2tmp <- as.matrix(dist(x)^2)
        t2 <- sum(t2tmp[cons])/m
        rat <- t2/r2
        return(rat)
      }
      res <- unlist(lapply(1:nrep, foobis))
      return(res)
    }
    
    rr <- lapply(li, foo)
    rr <- lapply(1:length(rr),
                 function(x) as.randtest(rr[[x]], obs[x,1]))
    
    opar <- par(mfrow=n2mfrow(length(rr)))
    on.exit(par(opar))
    rr <- lapply(rr, function(x) { x$pvalue <- 1-x$pvalue; return(x)})
    names(rr) <- names(li)
    lapply(1:length(rr),
           function(x) plot(rr[[x]],
                            main = paste(names(rr)[x], ": P = ",
                              round(rr[[x]]$pvalue, 2), sep="")))
    return(rr)
  }



### Buffer autour des lignes

buffer.line <- function(xy, x, dist)
  {
    if (inherits(x, "kasc"))
      x <- getkasc(x, 1)
    if (!inherits(x, "asc"))
      stop("x should be an object of class asc")
    ra<- attr(x, "cellsize")/100
    xy[,1]<-jitter(xy[,1], amount=ra)
    xy[,2]<-jitter(xy[,2], amount=ra)
    bu <- buffer(xy, x, dist)
    bu[is.na(bu)]<-0
    
    carter <- matrix(0, nrow=nrow(x), ncol = ncol(x))
    xyg <- getXYcoords(x)
    xgr<-xyg$x
    ygr<-xyg$y
    
    toto <- .C("bufligr", as.double(t(xy)), as.double(dist),
               as.double(t(carter)), as.double(xgr),
               as.double(ygr), as.integer(nrow(x)),
               as.integer(ncol(x)), as.integer(nrow(xy)),
               PACKAGE="adehabitat")[[3]]

    output <- matrix(toto, nrow = nrow(x), byrow = TRUE)
    output <- output + bu
    output[output>0]<-1
    
    output[output == 0] <- NA
    attr(output, "xll") <- attr(x, "xll")
    attr(output, "yll") <- attr(x, "yll")
    attr(output, "cellsize") <- attr(x, "cellsize")
    attr(output, "type") <- "numeric"
    class(output) <- "asc"
    return(output)
}



##############################################################################################
##############################################################################################
#####
##### Interface vers "sp"


kasc2spixdf <- function(ka)
  {
    if (!inherits(ka, "kasc"))
      stop("ka should be of class \"kasc\"")
    if (!require(sp))
      stop("the package sp is required for this function")
    xyc <- getXYcoords(ka)
    xc <- rep(xyc$x, times=length(xyc$y))
    yc <- rep(xyc$y, each=length(xyc$x))
    xyc<-data.frame(x=xc,y=yc)
    ka <- managNAkasc(ka)
    cons <- (1:nrow(ka))[!is.na(ka[,1])]
    df <- ka[cons,]
    class(df) <- "data.frame"
    xyc <- xyc[cons,]
    names(xyc) <- c("x","y")
    df1 <- data.frame(xyc, df)
    coordinates(df1) <- c("x","y")
    gridded(df1) <- TRUE
    return(df1)
  }

asc2spixdf <- function(a)
  {
    if (!inherits(a, "asc"))
      stop("a should be of class \"asc\"")
    if (!require(sp))
      stop("the package sp is required for this function")
    xyc <- getXYcoords(a)
    xc <- rep(xyc$x, times=length(xyc$y))
    yc <- rep(xyc$y, each=length(xyc$x))
    xyc<-data.frame(x=xc,y=yc)
    cons <- (1:length(c(a)))[!is.na(c(a))]
    var <- c(a)[cons]
    xyc <- xyc[cons,]
    names(xyc) <- c("x","y")
    df1 <- data.frame(xyc, var)
    coordinates(df1) <- c("x","y")
    gridded(df1) <- TRUE
    return(df1)
  }

spixdf2kasc <- function(sg)
  {
    if (!require(sp))
      stop("the package sp is required for this function")
    if (inherits(sg, "SpatialPixelsDataFrame"))
      sg <- as(sg, "SpatialGridDataFrame")
    if (!inherits(sg, "SpatialGridDataFrame"))
      stop(paste("sg should be of class \"SpatialPixelsDataFrame\"",
                 "\nor \"SpatialGridDataFrame\""))
    gr <- gridparameters(sg)
    if (nrow(gr)>2)
      stop("sg should be defined in two dimensions")
    if (gr[1,2]!=gr[2,2])
      stop("the cellsize should be the same in x and y directions")
    fullgrid(sg) <- TRUE
    xy <- coordinates(sg)
    ka <- sg@data
    ka <- ka[order(xy[,1]),]
    xy <- xy[order(xy[,1]),]
    ka <- ka[order(xy[,2]),]
    xy <- xy[order(xy[,2]),]
    nxy <- colnames(xy)
    ka <- ka[,is.na(match(names(ka), nxy))]
    attr(ka, "cellsize") <- gr[2,2]
    attr(ka, "xll") <- gr[1,1]
    attr(ka, "yll") <- gr[2,1]
    attr(ka,"ncol") <- gr[1,3]
    attr(ka,"nrow") <- gr[2,3]
    class(ka) <- c("kasc", "data.frame")
    if (ncol(ka)==1) {
      v <- ka[,1]
      if ((is.numeric(v)) | (is.logical(v))) {
        e <- matrix(v, ncol = attr(ka, "nrow"))
        attr(e, "type") <- "numeric"
      }
      else {
        tc2 <- levels(v)
        v <- as.numeric(v)
        e <- matrix(v, ncol = attr(ka, "nrow"))
        attr(e, "type") <- "factor"
        attr(e, "levels") <- tc2
      }
      attr(e, "cellsize") <- attr(ka, "cellsize")
      attr(e, "xll") <- attr(ka, "xll")
      attr(e, "yll") <- attr(ka, "yll")
      class(e) <- "asc"
      ka <- e
    }
    return(ka)
  }

area2sr <- function(ar)
  {
    if (!inherits(ar, "area"))
      stop("ka should be of class \"area\"")
    if (!require(sp))
      stop("the package sp is required for this function")
    class(ar) <- "data.frame"
    li <- split(ar[,2:3],ar[,1])
    res <- lapply(li, function(x) {
      if (!all(unlist(x[1,]==x[nrow(x),])))
        x <- rbind(x,x[1,])
      x <- as.matrix(x)
      y <- Sring(x, hole=FALSE)
      if (y@ringDir<0)
        y <- Sring(x[nrow(x):1,], hole=FALSE)
      return(y)
    })
    resb <- SpatialRings(lapply(1:length(res),
                               function(i) Srings(list(res[[i]]),
                                                  names(res)[i])))
    return(resb)
  }


sr2area <- function(sr)
  {
    if (!require(sp))
      stop("the package sp is required for this function")
    if (inherits(sr, "SpatialRingsDataFrame"))
      sr <- rings(sr)    
    if (!inherits(sr, "SpatialRings"))
      stop("sr should be of class \"SpatialRings\" or \"SpatialRingsDataFrame\"")
    pol <- sr@polygons
    warh <- 0
    warh2 <- 0
    warz <- 0
    res <- lapply(pol, function(x) {
      y <- x@Srings
      nom <- x@ID
      ll <- length(y)
      hh <- unlist(lapply(y, function(o) o@hole))
      hol <- sum(hh)
      ll <- ll-hol
      if (ll == 1) {
        if (hol == 0) {
          re <- as.data.frame(y[[1]]@coords)
          re <- data.frame( fac = factor(rep(nom,length(re[,1]))), re)
          names(re) <- c("fac", "x", "y")
        }
        if (hol != 0) {
          warh <- warh+hol
          warh2 <- warh2+1
          re <- as.data.frame(y[!hh][[1]]@coords)
          re <- data.frame( fac = factor(rep(nom,length(re[,1]))), re)
          names(re) <- c("fac", "x", "y")
        }
      }
      if (ll > 1) {
        warz <- warz+1
        if (hol == 0) {
          nom <- paste(nom, 1:ll, sep=".")
          re1 <- lapply(y, function(o) as.data.frame(o@coords))
          re <- do.call("rbind.data.frame", lapply(1:length(re1), function(i) {
            u <- data.frame(fac=factor(rep(nom[i], length(re1[[i]][,1]))), re1[[i]])
            names(u) <- c("fac", "x", "y")
            return(u)
          }))
        }
        if (hol!=0) {
          warh <- warh+hol
          warh2 <- warh2+1
          nom <- paste(nom, 1:ll, sep=".")
          y <- y[!hh]
          re1 <- lapply(y, function(o) as.data.frame(o@coords))
          re <- do.call("rbind.data.frame", lapply(1:length(re1), function(i) {
            u <- data.frame(fac=factor(rep(nom[i], length(re1[[i]][,1]))), re1[[i]])
            names(u) <- c("fac", "x", "y")
            return(u)
          }))
        }
      }
      return(list(re,warh2, warh, warz))
    })
    warh2 <- sum(unlist(lapply(res, function(x) x[[2]])))
    warh <- sum(unlist(lapply(res, function(x) x[[3]])))
    warz <- sum(unlist(lapply(res, function(x) x[[4]])))
    res <- lapply(res, function(x) x[[1]])
    res <- do.call("rbind.data.frame", res)
    res <- as.area(res)
    if (warh2>0) {
      warning(paste("Area objects do not take into account holes in polygon.\n",
                    warh, "holes have been deleted from the data, belonging to\n",
                    warh2, "polygons"))
    }
##    if (warz>0) {
##      warning(paste("Some spatial rings contained several polygons.\n",
##                    "Labels have therefore been changed for", warz, "objects"))
##    }
    return(res)
  }


attsr2area <- function(srdf)
  {
    if (!inherits(srdf, "SpatialRingsDataFrame"))
      stop("sr should be of class \"SpatialRingsDataFrame\"")
    dat <- srdf@data
    sr <- rings(srdf)
    
    res <- lapply(1:length(sr@polygons), function(i) {
      x <- sr@polygons[[i]]
      y <- x@Srings
      nom <- x@ID
      ll <- length(y)
      hh <- unlist(lapply(y, function(o) o@hole))
      hol <- sum(hh)
      ll <- ll-hol
      if (ll == 1) {
        re <- data.frame(nom=nom,dat[i,])
      }
      if (ll > 1) {
        nom <- paste(nom, 1:ll, sep=".")
        re <- data.frame(nom=nom, dat[rep(i,ll),])
      }
      return(re)
    })
    res <- do.call("rbind.data.frame", res)
    row.names(res) <- 1:nrow(res)
    return(res)
  }



traj2spdf <- function(tr)
  {
    if (!inherits(tr, "traj"))
      stop("tr should be of class \"traj\"")
    class(tr) <- "data.frame"
    xy <- tr[,c("x","y")]
    tr$y <- tr$x <- NULL
    res <- SpatialPointsDataFrame(xy, tr)
    return(res)
  }


traj2sldf <- function(tr, byid = FALSE)
  {
    if (!inherits(tr, "traj"))
      stop("tr should be of class \"traj\"")
    class(tr) <- "data.frame"
    lixy <- lapply(split(tr[,c("x","y")], tr$burst), function(x) Sline(as.matrix(x)))
    id <- unlist(lapply(split(tr$id, tr$burst), function(x) x[1]))
    bu <- unlist(lapply(split(tr$burst, tr$burst), function(x) x[1]))
    
    if (byid) {
      lev <- as.numeric(levels(factor(id)))
      re1 <- lapply(lev, function(x) Slines(lixy[id==x]))
      res <- SpatialLines(re1)
      df <- data.frame(id=lev)
    } else {
      res <- lapply(lixy, function(x) Slines(list(x)))
      res <- SpatialLines(res)
      df <- data.frame(id=id, burst=bu)
    }
    res <- SpatialLinesDataFrame(res, data=df)
    return(res)
  }



##############################################################################################
##############################################################################################
#####
##### Calcul de distances aux patchs d'habitat

distfacmap <- function(x)
  {
    if (!inherits(x, "asc"))
      stop("x should be of class \"asc\"")
    if (attr(x, "type")!="factor")
      stop("x should be of type \"factor\"")
    xyc <- getXYcoords(x)
    xc <- rep(xyc$x, times=length(xyc$y))
    yc <- rep(xyc$y, each=length(xyc$x))
    xyc<-data.frame(x=xc,y=yc)
    lev <- as.numeric(levels(factor(c(x))))
    li <- list()
    
    for (i in lev) {
      tmp <- x
      tmp[x!=i] <- NA
      tmp[x==i] <- 1
      ptsoui <- xyc[!is.na(c(tmp)),]
      toto <- .C("distxyr", as.double(t(as.matrix(xyc))),
                 as.double(t(as.matrix(ptsoui))),
                 as.integer(nrow(xyc)), as.integer(nrow(ptsoui)),
                 double(nrow(xyc)), PACKAGE="adehabitat")
      li[[i]] <- toto[[5]]
    }
    names(li) <- levels(x)
    ka <- as.kasc(list(x1=x))
    li <- as.data.frame(li)
    li <- getkascattr(ka,li)
    li <- setmask(li, x)
    return(li)
  }


