\name{mindistkeep}
\alias{mindistkeep}
\title{Detecting Absence of Movement in an Object of Class 'ltraj'}
\description{
  Objects of class \code{ltraj} are often created with data collected
  using some form of telemetry (radio-tracking, G.P.S., etc.).  However,
  the relocations of the monitored animals are always somewhat
  imprecise.  The function mindistkeep considers that when
  the distance between two successive relocations is lower than a given
  threshold distance, the animal actually does not move.
}
\usage{
mindistkeep(x, threshold)
}
\arguments{
  \item{x}{An object of class \code{ltraj}}
  \item{threshold}{The minimum distance under which is is considered
    that the animal does not move}
}
\value{
  An object of class \code{ltraj}
}
\author{Clement Calenge \email{clement.calenge@oncfs.gouv.fr}}
\seealso{\code{\link{ltraj}}}
\examples{
data(puechcirc)
plot(puechcirc)

i <- mindistkeep(puechcirc, 10)
plot(i)

}
\keyword{manip}
