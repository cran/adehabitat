"mcp" <-
function(xy, id, percent=95)
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

