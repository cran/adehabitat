\name{scatter.enfa}
\alias{scatter.enfa}
\title{Scatter Plot of the Results of the ENFA}
\description{
Performs the scatter diagrams of objects of class \code{enfa}.
}
\usage{
scatter.enfa(x, xax = 1, yax = 2, h, pts = FALSE,
             ncont = 1, clabel = 1, colZ = "blue",
             colS = "orange", lwdZ = 2, lwdS = 3,
             side = c("top", "bottom", "none"),
             csub = 1, ...)
}
\arguments{
  \item{x}{an object of class \code{enfa}}
  \item{xax}{the column number for the x-axis}
  \item{yax}{the column number for the y-axis}
  \item{h}{vector of bandwidths for x and y directions, used in the
    function \code{kde2d} of the package \code{MASS}.  Defaults to
    normal reference bandwidth (see \code{?kde2d})}
  \item{pts}{logical. Whether the points should be drawn.  If
    \code{FALSE}, contours of kernel densities estimates are displayed}
  \item{ncont}{if \code{pts == FALSE}, an integer indicating the number
    of contour levels desired for the kernel densities estimates.  By
    default, only the external one is displayed}
  \item{clabel}{a character size for the columns}
  \item{colZ}{color for the kernel density estimates of the available
    pixels}
  \item{colS}{color for the kernel density estimates of the used
    pixels}
  \item{lwdZ}{if \code{pts==FALSE}, the line width for the kernel
    density estimates of the available pixels}
  \item{lwdS}{if \code{pts==FALSE}, the line width for the kernel
    density estimates of the used pixels}
  \item{side}{if \code{"top"}, the legend of the kept axis is upside, if
    \code{"bottom"} it is downside, if \code{"none"} no legend}
  \item{csub}{a character size for the legend}
  \item{\dots}{further arguments passed to or from other methods}
}
\details{
\code{scatter.enfa} displays a factorial map of pixels, as well as the
projection of the vectors of the canonical basis multiplied by a
constant of rescaling.
The kept axes for the plot are specified in a corner.
}
\author{Mathieu Basille \email{basille@biomserv.univ-lyon1.fr}}
\section{Warning}{
  \code{biv.test} uses the function \code{kde2d} of the package \code{MASS}.
}
\seealso{\code{\link{enfa}}, \code{\link[ade4]{scatter}}}
\examples{
\dontrun{
data("lynxjura")

## We keep only "wild" indices.
tmp=lynxjura$loc[,4]!="D"

if (require(MASS))
    scatter(enfa(lynxjura$map, lynxjura$locs[tmp, c("X", "Y")],
            scannf=FALSE, nf=3))
}
}
\keyword{multivariate}
\keyword{hplot}
