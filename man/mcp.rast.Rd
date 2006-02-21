\name{mcp.rast}
\alias{mcp.rast}
\title{Converts a Polygon to Raster}
\description{
  \code{mcp.rast} converts a polygon map to a raster map of class
  \code{asc}.
}
\usage{
  mcp.rast(poly, w)
}
\arguments{
  \item{poly}{a data frame with 2 columns giving the coordinates
              of a polygon object}
  \item{w}{an object of class \code{kasc}, or of class \code{asc}}
}
\value{
Returns an object of class \code{asc}.
}
\author{ Clément Calenge \email{calenge@biomserv.univ-lyon1.fr} }
\seealso{ \code{\link{hr.rast}}}
\examples{
data(puechabon)
toto <- puechabon$kasc
loc <- puechabon$locs

## gets the coordinates of the relocations for the wild boar #1
wb1 <- loc[loc$Name == "Chou",]
wb1 <- cbind(wb1$X, wb1$Y)
nbpol <- chull(wb1)
xycoord <- wb1[nbpol,]

## rasterization of wb1
tutu <- mcp.rast(xycoord, toto)
image(tutu)

polygon(xycoord, lwd = 2)
}
\keyword{spatial}




