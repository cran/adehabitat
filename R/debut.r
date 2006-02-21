##### Chargement de base

.First.lib <- function(lib, pkg) {
  cat("This package requires ade4 to be installed\n\n")
  require(ade4)
  require(gpclib)
  library.dynam("adehabitat", pkg, lib)
}

