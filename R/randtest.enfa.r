"randtest.enfa" <-
function(xtest, nrepet=999, ...)
  {
    if (!inherits(xtest,"enfa"))
      stop("should be an object of class \"enfa\"")
    tab<-as.matrix(xtest$tab)
    pr<-xtest$pr
    res<-.C("randenfar", as.double(t(tab)), as.double(pr),
            as.integer(ncol(tab)), as.integer(nrow(tab)),
            as.integer(nrepet), double(nrepet), PACKAGE="adehabitat")[[6]]
    return(as.randtest(res, xtest$s[1], call = match.call()))
  }

