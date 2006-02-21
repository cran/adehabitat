"print.NNCH" <-
function(x, ...)
  {
    cat("***********************************************\n")
    cat("***\n")
    cat("***      Nearest-neighbor convex hull\n\n")
    cat(paste("Home range available for", length(x), "animals:\n"))
    print(names(x), ...)
    cat("\n\nEach animal is a component of the object. For each animal,")
    cat("\nthe following information is available:\n")
    cat("\n$area:       home-range size estimated at various levels")
    cat("\n$polygons:   objects of class \"gpc.poly\" storing the home-range limits")
    cat("\n$xy:         the relocations\n\n")
  }

