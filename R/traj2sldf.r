"traj2sldf" <-
function(tr, byid = FALSE)
  {
    if (!inherits(tr, "traj"))
      stop("tr should be of class \"traj\"")
    class(tr) <- "data.frame"
    lixy <- lapply(split(tr[,c("x","y")], tr$burst), function(x) Line(as.matrix(x)))
    id <- unlist(lapply(split(tr$id, tr$burst), function(x) x[1]))
    bu <- unlist(lapply(split(tr$burst, tr$burst), function(x) x[1]))
    
    if (byid) {
      lev <- levels(factor(id))
      re1 <- lapply(lev, function(x) Lines(lixy[id==x], ID=x))
      res <- SpatialLines(re1)
      df <- data.frame(id=lev)
    } else {
      res <- lapply(1:length(lixy),
                    function(i) Lines(list(lixy[[i]]), ID=names(lixy)[i]))
      res <- SpatialLines(res)
      df <- data.frame(id=id, burst=bu)
    }
    res <- SpatialLinesDataFrame(res, data=df)
    return(res)
  }

