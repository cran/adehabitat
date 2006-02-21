##### Chargement de base

cat("This package requires ade4 to be installed\n\n")
require(ade4)
require(gpclib)

.First.lib <- function(lib, pkg) {
  library.dynam("adehabitat", pkg, lib)
}

