##### Chargement de base

.First.lib <- function(lib, pkg) {
    msg <- paste("This package requires ade4 to be installed\n\n", "Type:\n",
                 "demo(rastermaps) for demonstration of raster map analysis\n",
                 "demo(homerange) for demonstration of home-range estimation\n",
                 "demo(managltraj) for demonstration of animals trajectory management\n",
                 "demo(analysisltraj) for demonstration of animals trajectory analysis\n",
                 "demo(nichehs) for demonstration of niche/habitat selection analysis\n\n", sep="")
    cat(msg)
    require(ade4)
    require(gpclib)
    library.dynam("adehabitat", pkg, lib)
}

