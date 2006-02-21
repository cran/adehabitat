rec <- function(x, slsp=c("remove","missing"))
  {
    if (!inherits(x, "ltraj"))
      stop("x should be of class \"ltraj\"")
    slsp <- match.arg(slsp)
    y <- traj2df(ltraj2traj(x))
    return(as.ltraj(xy=y[,c("x","y")], date=y$date,
                    id=y$id, burst=y$burst, slsp=slsp))
  }
