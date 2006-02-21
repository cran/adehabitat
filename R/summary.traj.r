"summary.traj" <-
function(object, id=levels(object$id), date=NULL, ...)
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

