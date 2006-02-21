"NNCH.rast" <-
function(y, w)
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

