\name{distfacmap}
\alias{distfacmap}
\title{Compute distances to the different levels of a factor map }
\description{
  This function computes map of distances to patches belonging to the
  different levels of a map of class \code{asc} and of type
  \code{factor}.
}
\usage{
distfacmap(x)
}
\arguments{
  \item{x}{an object of class \code{asc} and of type \code{factor} }
}
\value{
  An object of class \code{kasc}.
}
\author{  Clément Calenge \email{calenge@biomserv.univ-lyon1.fr} }
\seealso{ \code{\link{import.asc}} for further info on objects of class
  \code{asc}. }
\examples{
\dontrun{
data(puechabon)
asp <- getkasc(puechabon$kasc, "Aspect")
image(asp)
sor <- distfacmap(asp)
image(sor)
}
}
\keyword{spatial}
