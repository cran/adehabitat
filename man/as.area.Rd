\name{as.area}
\alias{as.area}
\title{Objects of Class "area"}
\description{
  Objects of class \code{area} are used to store the information on
  the shape of vectorised objects.\cr
  An object of class \code{area} is a data frame with three variables.
  The first variable is a factor defining the polygons.\cr
  The second and third variables are the xy coordinates of the
  polygon vertices in the order where they are found.
}
\usage{
as.area(x)
}
\arguments{
  \item{x}{a data frame with three columns}
}
\value{
  Returns an object of class \code{area}
}
\seealso{ \code{\link[ade4]{area.plot}} for other operations on
  objects of class \code{area}, \code{\link{area2dxf}} for exportation
  of the objects \code{area} toward a GIS. }
\examples{
data(elec88)
ar <- as.area(elec88$area)
area.plot(ar)
}
\keyword{spatial}
