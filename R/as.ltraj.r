as.ltraj <- function(xy, date, id, burst=id, slsp =  c("remove", "missing"))
{
  if (!inherits(date,"POSIXct"))
    stop("date should be of class \"POSIXct\"")
  if (length(date) != nrow(xy))
    stop("date should be of the same length as xy")
  id <- as.character(id)
  burst <- as.character(burst)
  if (length(id)>1)
    stop("id should be of length 1")
  x <- xy[,1]
  y <- xy[,2]
  res <- data.frame(x=x,y=y, date=date)
  slsp <- match.arg(slsp)
  
  foo <- function(x) {
    x1 <- x[-1, ]
    x2 <- x[-nrow(x), ]
    dist <- c(sqrt((x1$x - x2$x)^2 + (x1$y - x2$y)^2),NA)
    R2n <- (x$x - x$x[1])^2 + (x$y - x$y[1])^2
    dt <- c(unclass(x1$date) - unclass(x2$date), NA)
    dx <- c(x1$x - x2$x, NA)
    dy <- c(x1$y - x2$y, NA)
    abs.angle <- ifelse(dist<1e-07,NA,atan2(dy,dx))
                                        # angle absolu est NA si dx==dy==0
    so <- cbind.data.frame(dx=dx, dy=dy, dist=dist,
                           dt=dt, R2n=R2n, abs.angle=abs.angle)
    return(so)
  }
  speed <- foo(res)
  res <- cbind(res,speed)
  
  ang.rel <- function(df,slspi=slsp) {
    ang <- NA   
    for(i in 2:(nrow(df)-1)){
      if(df$dist[i]>1e-07){ # deplacement en i
        if(df$dist[i-1]>1e-07){ # deplacement en i-1
          x <- (df$dx[i]*df$dx[i-1]+df$dy[i]*df$dy[i-1])/
            sqrt(df$dx[i-1]^2+df$dy[i-1]^2)
          y <- sqrt(df$dx[i]^2*df$dy[i-1]^2+df$dy[i]^2*
                    df$dx[i-1]^2-2*df$dx[i]*df$dx[i-1]*
                    df$dy[i-1]*df$dy[i])/sqrt(df$dx[i-1]^2+df$dy[i-1]^2)
          deter <- df$dx[i-1]*df$dy[i]-df$dy[i-1]*df$dx[i]
          angi <- atan2(sign(deter)*y,x)
        }
        else {#pas de deplacement en i-1
          if(slspi=="missing") {
                                        # on met NA
            angi <- NA
          }
          else{ # on recherche le premier vrai deplacement anterieur
            j <- (i-1)
            
            while(df$dist[j]<1e-07){
              if (j<1){
                j <- NA
                break
              }
              j <- (j-1)
            }
            if(is.na(j)){
              angi <- NA}
            else{
              x <- (df$dx[i]*df$dx[j]+df$dy[i]*df$dy[j])/
                sqrt(df$dx[j]^2+df$dy[j]^2)
              y <- sqrt(df$dx[i]^2*df$dy[j]^2+df$dy[i]^2*
                        df$dx[j]^2-2*df$dx[i]*df$dx[j]*df$dy[j]*df$dy[i])/
                          sqrt(df$dx[j]^2+df$dy[j]^2)
              deter <- df$dx[j]*df$dy[i]-df$dy[j]*df$dx[i]
              angi <- atan2(sign(deter)*y,x)
            }
          }
        }
      }
      else {angi<-NA} # pas de deplacement en i
      ang <- c(ang,angi)
    }
    ang <- c(ang,NA)
    return(ang)
  } 
  rel.angle <- ang.rel(res)
  res <- data.frame(res, rel.angle=rel.angle)
  res <- list(data.frame(res))
  attr(res[[1]],"id") <- id
  attr(res[[1]],"burst") <- burst
  class(res) <- c("ltraj","list")
  return(res)
}



"[.ltraj" <- function(x, i, id, burst)
  {
    if (!inherits(x, "ltraj"))
      stop("x should be of class \"ltraj\"")
    if (sum((!missing(i))+(!missing(id))+(!missing(burst)))!=1)
      stop("non convenient subset")
    x <- unclass(x)
    
    if (!missing(i))
      y <- x[i]
    if (!missing(id)) {
      idb <- unlist(lapply(x,function(z) attr(z,"id")))
      y <- x[idb%in%id]
    }
    if (!missing(burst)) {
      idb <- unlist(lapply(x,function(z) attr(z,"burst")))
      y <- x[idb%in%burst]
    }
    class(y) <- c("ltraj","list")
    return(y)
  }



"[<-.ltraj" <- function(x, i, id, burst, value)
  {
    if (!inherits(x, "ltraj"))
      stop("x should be of class \"ltraj\"")
    if (sum((!missing(i))+(!missing(id))+(!missing(burst)))!=1)
      stop("non convenient subset")
    x <- unclass(x)

    if (!missing(i))
      x[i] <- value
    if (!missing(id)) {
      idb <- unlist(lapply(x,function(z) attr(z,"id")))
      x[idb%in%id] <-  value
    }
    if (!missing(burst)) {
      idb <- unlist(lapply(x,function(z) attr(z,"burst")))
      x[idb%in%burst] <- value
    }
    class(x) <- c("ltraj","list")
    bu <- unlist(lapply(x, function(y) attr(y, "burst")))
    if (length(unique(bu))!=length(bu))
      stop("attribute \"burst\" should be unique for a burst of relocations")
    return(x)
  }


summary.ltraj <- function(object,...)
  {
    if (!inherits(object, "ltraj"))
      stop("object should be of class \"ltraj\"")
    id <- factor(unlist(lapply(object, function(x) attr(x, "id"))))
    burst <- unlist(lapply(object, function(x) attr(x, "burst")))
    nr <- unlist(lapply(object, nrow))
    pr <- data.frame(id=id, burst=burst, number.of.relocations=nr)
    return(pr)
  }

print.ltraj <- function(x,...)
  {
    if (!inherits(x, "ltraj"))
      stop("x should be of class \"ltraj\"")
    pr <- summary(x)
    cat("*********** List of class ltraj ***********\n\n")
    cat("characteristics of the bursts:\n")
    print(pr)
  }


traj2ltraj <- function(traj,slsp =  c("remove", "missing"))
  {
    if (!inherits(traj, "traj"))
      stop("traj should be of class \"traj\"")
    slsp <- match.arg(slsp)
    traj <- traj2df(traj)
    res <- split(traj, traj$burst)
    foo <- function(x) {
      x1 <- x[-1, ]
      x2 <- x[-nrow(x), ]
      dist <- c(sqrt((x1$x - x2$x)^2 + (x1$y - x2$y)^2),NA)
      dt <- c(unclass(x1$date) - unclass(x2$date), NA)
      dx <- c(x1$x - x2$x, NA)
      dy <- c(x1$y - x2$y, NA)
      abs.angle <- ifelse((abs(dx)<1e-07)&(abs(dy)<1e-07),
                          NA,atan2(dy,dx))
                                        # angle absolu est NA si dx==dy==0
      so <- cbind.data.frame(dx=dx, dy=dy, dist=dist,
                             dt=dt,abs.angle=abs.angle)
      return(so)
    }
    speed <- lapply(res, foo)
    res <- lapply(1:length(res),
                  function(i) cbind(res[[i]],speed[[i]]))
    
    ang.rel <- function(df,slspi=slsp) {
      ang <- NA   
      for(i in 2:(nrow(df)-1)){
        if(df$dist[i]>1e-07){ # deplacement en i
          if(df$dist[i-1]>1e-07){ # deplacement en i-1
            x <- (df$dx[i]*df$dx[i-1]+df$dy[i]*df$dy[i-1])/
              sqrt(df$dx[i-1]^2+df$dy[i-1]^2)
            y <- sqrt(df$dx[i]^2*df$dy[i-1]^2+df$dy[i]^2*df$dx[i-1]^2-
                      2*df$dx[i]*df$dx[i-1]*df$dy[i-1]*df$dy[i])/
                        sqrt(df$dx[i-1]^2+df$dy[i-1]^2)
            deter <- df$dx[i-1]*df$dy[i]-df$dy[i-1]*df$dx[i]
            angi <- atan2(sign(deter)*y,x)
          }
          else {#pas de deplacement en i-1
            if(slspi=="missing") {
                                        # on met NA
              angi <- NA
            }
            else{ # on recherche le premier vrai deplacement anterieur
              j <- (i-1)
              
              while(df$dist[j]<1e-07){
                if (j<1){
                  j <- NA
                  break
                }
                j <- (j-1)
              }
              if(is.na(j)){
                angi <- NA}
              else{
                x <- (df$dx[i]*df$dx[j]+df$dy[i]*df$dy[j])/
                  sqrt(df$dx[j]^2+df$dy[j]^2)
                y <- sqrt(df$dx[i]^2*df$dy[j]^2+df$dy[i]^2*
                          df$dx[j]^2-2*df$dx[i]*df$dx[j]*
                          df$dy[j]*df$dy[i])/sqrt(df$dx[j]^2+df$dy[j]^2)
                deter <- df$dx[j]*df$dy[i]-df$dy[j]*df$dx[i]
                angi <- atan2(sign(deter)*y,x)
              }
            }
          }       
        }
        else {angi<-NA} # pas de deplacement en i
        ang <- c(ang,angi)
      }
      ang <- c(ang,NA)
      return(ang)
    } 
    
    rel.angle <- lapply(res, ang.rel)
    res <- lapply(1:length(res),
                  function(i) data.frame(res[[i]],
                                         rel.angle=rel.angle[[i]]))
    res <- lapply(res, function(x) {
      attr(x,"id") <- as.character(x$id[1])
      attr(x,"burst") <- as.character(x$burst[1])
      x$id <- NULL
      x$burst <- NULL
      return(x)
    })
    class(res) <- c("ltraj","list")
    return(res)
  }


ltraj2traj <- function(x)
  {
    if (!inherits(x, "ltraj"))
      stop("x should be of class \"traj\"")
    id <- factor(unlist(lapply(x, function(y) 
                               id <- rep(attr(y,"id"), nrow(y)))))
    burst <- factor(unlist(lapply(x, function(y) 
                                  id <- rep(attr(y,"burst"), nrow(y)))))
    res <- do.call("rbind", x)
    res <- cbind(id,burst,res)
    class(res) <- c("traj","data.frame")
    return(res)
  }

c.ltraj <- function(...)
  {
    uu <- list(...)
    if (!all(unlist(lapply(uu, function(x) inherits(x,"ltraj")))))
      stop("all objects should be of class \"ltraj\"")
    bu <- unlist(lapply(uu, function(x) unlist(lapply(x, function(y) attr(y, "burst")))))
    if (length(unique(bu))!=length(bu))
      stop("attribute \"burst\" should be unique for a burst of relocations")
    uu <- lapply(uu, unclass)
    uu <- do.call("c",uu)
    class(uu) <- c("ltraj","list")
    return(uu)
  }
