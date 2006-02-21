\name{gdltraj}
\alias{gdltraj}
\title{Working with Trajectories: Specify a Time Period}
\description{
  Gets the parts of the trajectories stored in an object of class
  \code{ltraj}, corresponding to a specified time period.
}
\usage{
gdltraj(x, min, max, type = c("POSIXct", "sec", "min", "hour", "mday",
        "mon", "year", "wday", "yday")) 
}
\arguments{
  \item{x}{an object of class \code{ltraj} }
  \item{min}{numeric.  The beginning of the period to consider}
  \item{max}{numeric.  The end of the period to consider}
  \item{type}{character.  The time units of \code{min} and
    \code{max}}
}
\details{
  The limits of the period to consider may correspond to any of the
  components of the list of class \code{POSIXlt} (hour, day, month,
  etc.; see \code{help(POSIXlt)}), or to dates stored in objects of
  class \code{POSIXct} (see examples).
}
\value{
  an object of class \code{ltraj}.
}
\author{Clément Calenge \email{calenge@biomserv.univ-lyon1.fr}}
\seealso{\code{\link{as.ltraj}} for further information about objects of
  class \code{ltraj}, \code{\link{POSIXlt}} for further information
  about objects of class \code{POSIXlt}}
\examples{
data(puechcirc)
plot(puechcirc, perani = FALSE)

## gets all the relocations collected
## between midnight and 3H AM 
toto <- gdltraj(puechcirc, min = 0, max = 3, type="hour")
plot(toto, perani = FALSE)

## gets all relocations collected between the 15th
## and the 25th august 1993
lim <- as.POSIXct(strptime(c("15/08/1993", "25/08/1993"),
                  "\%d/\%m/\%Y"))
tutu <- gdltraj(puechcirc, min = lim[1],
                max = lim[2], type="POSIXct")
plot(tutu, perani = FALSE)

}
\keyword{spatial}
