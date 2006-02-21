as.ltraj <- function(xy, date, id, burst=id, slsp =  c("remove", "missing"))
{
  if (!inherits(date,"POSIXct"))
    stop("date should be of class \"POSIXct\"")
  if (length(date) != nrow(xy))
    stop("date should be of the same length as xy")
  
  slsp <- match.arg(slsp)
  
  ## longueur de id
  if (length(id)==1)
    id <- rep(as.character(id), nrow(xy))
  if (length(id)!=nrow(xy))
    stop("id should be of the same length as xy, or of length 1")
  id <- as.character(id)
  
  ## longueur de burst
  if (length(burst)==1)
    burst <- rep(as.character(burst), nrow(xy))
  if (length(burst)!=nrow(xy))
    stop("burst should be of the same length as xy, or of length 1")
  burst <- as.character(burst)
  
  ## Vérification de l'unicité des bursts pour chaque id
  id1 <- factor(id)
  burst1 <- factor(burst)
  if (!all(apply(table(id1,burst1)>0,2,sum)==1))
    stop("one burst level should belong to only one id level")
  
  x <- xy[,1]
  y <- xy[,2]
  res <- split(data.frame(x=x,y=y, date=date), burst)
  liid <- split(id, burst)
  
  ## Tri des dates
  res <- lapply(res, function(y) y[order(y$date),])

  ## Vérification que pas de doublons des dates
  rr <- any(unlist(lapply(res,
                          function(x) (length(unique(x$date))!=length(x$date)))))
  if (rr)
    stop("non unique dates for a given burst")
  
         
  
  ## Calcul des descripteurs
  foo <- function(x) {
    x1 <- x[-1, ]
    x2 <- x[-nrow(x), ]
    dist <- c(sqrt((x1$x - x2$x)^2 + (x1$y - x2$y)^2),NA)
    R2n <- (x$x - x$x[1])^2 + (x$y - x$y[1])^2
    dt <- c(unclass(x1$date) - unclass(x2$date), NA)
    dx <- c(x1$x - x2$x, NA)
    dy <- c(x1$y - x2$y, NA)
    abs.angle <- ifelse(dist<1e-07,NA,atan2(dy,dx))
    ## angle absolu est NA si dx==dy==0
    so <- cbind.data.frame(dx=dx, dy=dy, dist=dist,
                           dt=dt, R2n=R2n, abs.angle=abs.angle)
    return(so)
  }
  speed <- lapply(res, foo)
  res <- lapply(1:length(res), function(i) cbind(res[[i]],speed[[i]]))
  
  ang.rel <- function(df,slspi=slsp) {
    ang1 <- df$abs.angle[-nrow(df)] # angle i-1
    ang2 <- df$abs.angle[-1] # angle i
    
    if(slspi=="remove"){
      dist <- c(sqrt((df[-nrow(df),"x"] - df[-1,"x"])^2 + (df[-nrow(df),"y"] - df[-1,"y"])^2),NA)
      wh.na <- which(dist<1e-7)
      if(length(wh.na)>0){
        no.na <- (1:length(ang1))[!(1:length(ang1)) %in% wh.na]
        for (i in wh.na){
          indx <- no.na[no.na<i]
          ang1[i] <- ifelse(length(indx)==0,NA,ang1[max(indx)])
        }
      }
    }
    res <- ang2-ang1
    res <- ifelse(res <= (-pi), 2*pi+res,res)
    res <- ifelse(res > pi, res -2*pi,res)
    return(c(NA,res))
  }
  rel.angle <- lapply(res, ang.rel)
  res <- lapply(1:length(res),
                function(i) data.frame(res[[i]], rel.angle=rel.angle[[i]]))
  res <- lapply(1:length(res), function(i) {
    x <- res[[i]]
    attr(x, "id") <- as.character(liid[[i]][1])
    attr(x,"burst") <- levels(factor(burst))[i]
    return(x)
  })
  class(res) <- c("ltraj","list")
  return(res)
}





traj2ltraj <- function(traj,slsp =  c("remove", "missing"))
  {
    if (!inherits(traj, "traj"))
      stop("traj should be of class \"traj\"")
    slsp <- match.arg(slsp)
    traj <- traj2df(traj)
    res <- as.ltraj(xy=traj[,c("x","y")], date=traj$date, id=traj$id,
                    burst=traj$burst, slsp)
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
    na <- unlist(lapply(object, function(i) sum(is.na(i[,1]))))
    pr <- data.frame(id=id, burst=burst, number.of.relocations=nr, missing.values=na)
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



ltraj2traj <- function(x)
  {
    if (!inherits(x, "ltraj"))
      stop("x should be of class \"ltraj\"")
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
