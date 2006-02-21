\name{histniche}
\alias{histniche}
\title{Histograms of the Ecological Niche}
\description{
  \code{histniche} draws histograms of the variables mapped in an
  object of class \code{kasc} (habitat available for the species). The
  histograms of the habitat used by a species (i.e. the niche) are
  computed from the locations of the focus species, and are added to the
  plot of available habitat for each variable, for comparison.
}
\usage{
histniche(kasc, pts, type = c("h", "l"), adjust = 1,
          Acol, Ucol, Aborder, Uborder, Alwd = 1, Ulwd = 1, \dots)
}
\arguments{
  \item{kasc}{a raster map of class \code{kasc}}
  \item{pts}{a data frame with two columns, giving the coordinates of
    the species locations}
  \item{type}{what type of plot should be drawn.  Possible types are:\cr
    * \code{"h"} for histograms,\cr
    * \code{"l"} for kernel density estimates (see \code{?density}).\cr
    By default, \code{type = "h"} is used.  If \code{type = "l"} is used,
    the position of the mean of each distribution is indicated by dotted
    lines}
  \item{adjust}{if \code{type = "l"}, a parameter used to control the
    bandwidth of the density estimate (see \code{?density})}
  \item{Acol}{color for the histograms of the available pixels}
  \item{Ucol}{color for the histograms of the used pixels}
  \item{Aborder}{if \code{type = "h"}, color for the border of the
    histograms of the available pixels (see \code{help(hist.default)})}
  \item{Uborder}{if \code{type = "h"}, color for the border of the
    histograms of the used pixels (see \code{help(hist.default)})} 
  \item{Alwd}{if \code{type = "l"}, line width for the density estimate
    of the available pixels} 
  \item{Ulwd}{if \code{type = "l"}, line width for the density estimate
    of the used pixels} 
  \item{\dots}{further arguments passed to or from other methods}
}
\author{Mathieu Basille \email{basille@biomserv.univ-lyon1.fr} }
\examples{
\dontrun{
## Example with factors and numeric variables
data(puechabon)
histniche(puechabon$kasc, puechabon$locs[, c("X", "Y")])

## Aspect is a factor, then it's not possible to use 
## kernel density estimates for it :
histniche(puechabon$kasc, puechabon$locs[, c("X", "Y")], type = "l")
}
}
\keyword{multivariate}
