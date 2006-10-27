"fpt" <-
function (lt, radii, units = c("seconds", "hours", "days"))
  {
    if (!inherits(lt, "ltraj"))
      stop("should be an object of class 'ltraj'")
    ## On supprime les données manquantes:
    lt <- lapply(lt, function(i) {
      jj <- i[!is.na(i$x),]
      attr(jj, "id") <- attr(i,"id")
      attr(jj, "burst") <- attr(i,"burst")
      return(jj)
    })
    units <- match.arg(units)

    foo <- function(x) {
      toto <- .C("fipatir", as.double(x$x), as.double(x$y),
                 as.double(x$date), as.integer(nrow(x)),
                 as.double(radii), as.integer(length(radii)),
                 double(nrow(x)*length(radii)), PACKAGE = "adehabitat")
      mat <- matrix(toto[[7]], ncol = length(radii), byrow = TRUE)
      mat[mat==-1] <- NA
      mat <- as.data.frame(mat)
      names(mat) <- paste("r",1:length(radii),sep="")
      row.names(mat)=row.names(x)
      return(mat)
    }

    lo <- lapply(lt, foo)
    names(lo) <- unlist(lapply(lt, function(x) attr(x,"burst")))
    if (units == "hours") lo <- lapply(lo, function(x) x/3600)
    if (units == "days") lo <- lapply(lo, function(x) x/(3600*24))
    lo <- lapply(1:length(lo), function(i) {
      attr(lo[[i]], "date") <- lt[[i]]$date
      attr(lo[[i]], "id") <- attr(lt[[i]], "id")
      attr(lo[[i]], "burst") <- attr(lt[[i]], "burst")
      return(lo[[i]])
    })
    attr(lo, "radii") <- radii
    class(lo) <- "fipati"
    return(lo)
  }

