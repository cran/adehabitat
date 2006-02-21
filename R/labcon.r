"labcon" <-
function(x)
  {
    if (!inherits(x, "asc")) 
      stop("should be an object of class asc")
    y<-x
    rajfond <- function(x) {
      nr <- nrow(x)
      nc <- ncol(x)
      f <- rep(0, nr)
      x <- cbind(f, x, f)
      f <- rep(0, nc + 2)
      x <- rbind(f, x, f)
    }
    x[!is.na(x)] <- 1
    x[is.na(x)] <- 0
    x <- rajfond(x)
    toto <- .C("seqeticorr", as.double(t(x)), as.integer(nrow(x)), 
               as.integer(ncol(x)), PACKAGE="adehabitat")
    etiquete <- matrix(toto[[1]], nrow = nrow(x), byrow = TRUE)
    etiquete <- etiquete[-c(1, nrow(etiquete)), -c(1, ncol(etiquete))]
    etiquete[etiquete==0]<-NA
    s<-getascattr(y, etiquete)
    attr(s, "type")<-"factor"
    attr(s, "levels")<-as.character(1:nlevels(factor(etiquete)))
    return(s)
  }

