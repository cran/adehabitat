\name{getXYcoords}
\alias{getXYcoords}
\title{Computes the X and Y Coordinates of the Pixels of a Raster Map}
\description{
  \code{getXYcoords} computes the geographical coordinates of the rows
  and columns of pixels of a raster map.
}
\usage{
getXYcoords(w)
}
\arguments{
  \item{w}{an object of class \code{asc}, \code{kasc}, \code{sahrlocs},
    or \code{mapattr}.}
}
\value{
  Returns a list with two components:
  \item{x}{the x coordinates of the columns of pixels of the map}
  \item{y}{the y coordinates of the rows of pixels of the map}
}
\author{Clément Calenge \email{calenge@biomserv.univ-lyon1.fr}}
\seealso{ \code{\link{import.asc}}, \code{\link{as.kasc}},
  \code{\link{as.sahrlocs}}, \code{\link{storemapattr}}}

\examples{
data(puechabon)
(elev <- getkasc(puechabon$kasc, "Elevation"))
(coords <- getXYcoords(elev))
nrow(elev) == length(coords$x)
ncol(elev) == length(coords$y)
}
\keyword{programming}


