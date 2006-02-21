\name{Extract.ltraj}
\alias{Extract.ltraj}
\alias{[.ltraj}
\alias{[<-.ltraj}
\title{Extract or Replace Parts of an Object of Class ltraj}
\description{
  Extract or replace subsets of objects of class \code{ltraj}.
}
\usage{
  x[i, id, burst]
  x[i, id, burst] <- value
}
\arguments{
\item{x}{an object of class \code{ltraj} }
\item{i}{the elements to extract or replace. \code{i} is
  \code{numeric}}
\item{id}{a vector indicating the identity of the animals to extract
  or replace}
\item{burst}{a vector indicating the identity of the bursts of
  relocations to extract or replace}
\item{value}{an object of class \code{ltraj}}
}
\details{
  Objects of class \code{ltraj} contain several bursts of
  relocations. This function subsets or replaces these bursts, based
  on their indices or on the attributes \code{id} and \code{burst}.
}
\value{
  An object of class \code{ltraj}.
}
\author{ Clément Calenge \email{calenge@biomserv.univ-lyon1.fr} }
\seealso{ \code{\link{as.ltraj}}, \code{\link{gdltraj}} }
\examples{
  data(puechcirc)
  puechcirc

  ## Extract the second and third bursts
  (toto <- puechcirc[2:3])

  ## Extracts all bursts collected on the animal JE
  puechcirc[id = "JE93"]

  
  ## Replace one burst
  (toto[2] <- puechcirc[1:2])

}
\keyword{spatial}
