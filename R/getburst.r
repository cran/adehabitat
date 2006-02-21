"getburst" <-
function(x, burst=levels(x$burst),
                   id=levels(x$id), date=NULL)
{
  if (!inherits(x, "traj"))
    stop("should be an object of class traj")
  ## s�lection des dates
  if (!is.null(date)) 
    x<-x[(x$date>=date[1])&(x$date<date[2]),]
  
  ## s�lection des animaux
  i<-split(x, x$id)
  x<-do.call("rbind", i[id])
  
  ## s�lection des circuits
  i<-split(x, x$burst)
  x<-do.call("rbind", i[burst])
  x$burst<-factor(x$burst)
  x$id<-factor(x$id)
  return(x)
}

