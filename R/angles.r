"angles" <-
function (x, id = levels(x$id), burst = levels(x$burst),
                  date = NULL, slsp = c("remove", "missing"))
  {
    .Deprecated("as.ltraj")
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

