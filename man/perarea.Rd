\name{perarea}
\alias{perarea}
\alias{ararea}
\title{Compute Areas and Perimeters of Objects of Class "area" }
\description{
  \code{perarea} computes the perimeters of polygons in objects of class
  \code{area}.\cr
  \code{ararea} computes the areas of polygons in objects of class
  \code{area}.\cr
}
\usage{
perarea(x)
ararea(x)
}
\arguments{
  \item{x}{object of class \code{area} }
}
\value{
  a vector.
}
\section{Warning }{
  \code{ararea} requires the package \code{gpclib}.
}
\author{Clément Calenge \email{calenge@biomserv.univ-lyon1.fr}}
\seealso{\code{\link{as.area}}, \code{\link{plot.area}}}
\examples{

data(puechabon)
locs <-  puechabon$locs

cp <- mcp(locs[,4:5], locs[,1])

perarea(cp)

if (require(gpclib))
   ararea(cp)

}
\keyword{hplot}

