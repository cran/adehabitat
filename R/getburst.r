"getburst" <-
function(x, burst=levels(x$burst),
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

