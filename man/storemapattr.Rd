\name{storemapattr}
\alias{storemapattr}
\title{Store attributes of maps of class asc and kasc}
\description{
  \code{storemapattr} stores attributes of maps of class \code{asc} and
  \code{kasc} in an object of class \code{mapattr}. 
}
\usage{
storemapattr(x)
}
\arguments{
  \item{x}{ An object of class \code{asc} or \code{kasc} }
}
\details{
  This function is essentially used by programmers in functions dealing with
  maps of class \code{asc} or \code{kasc}.  The function
  \code{getXYcoords} is an example of function using \code{storemapattr}.
}
\value{
  Returns an object of class \code{mapattr}.
}
\author{ Clément Calenge \email{calenge@biomserv.univ-lyon1.fr} }
\seealso{ \code{\link{as.kasc}} and \code{\link{import.asc}} for
  additional information on the classes \code{kasc} and \code{asc}. }
\examples{
data(puechabon)
(kasc <- puechabon$kasc)

(toto <- storemapattr(kasc))

}
\keyword{programming}
