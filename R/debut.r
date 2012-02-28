##### Chargement de base
.onAttach <- function(libname, pkgname)
{
    msg <- paste("Be careful: it is now recommended to use the new",
                 "packages adehabitatMA, adehabitatLT, adehabitatHR, and adehabitatHS.\n",
                 "These 4 packages are intended to become the future of adehabitat.\n",
                 "The \"classical\" version of adehabitat will still be maintained for some\n",
                 "time, but no new method will be added to the package.\n",
                 sep="")
    packageStartupMessage(msg)
}
