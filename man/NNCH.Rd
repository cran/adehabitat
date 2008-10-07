\name{NNCH}
\alias{NNCH}
\title{Nearest Neighbor Convex Hulls (LoCoH)}
\description{
	\code{NNCH} computes the home range of several animals using the
	LoCoH family of methods.
}
\usage{
NNCH(xy, id = NULL, k = c(10), unin = c("m", "km"),
     unout = c("m2", "ha", "km2"), status = FALSE,
     duplicates = 1, hog.limit = 500, r = NULL, a = NULL,
     min.k = NULL, max.k = NULL)
}
\arguments{
  \item{xy}{a data frame containing the coordinates of the relocations
    of the monitored animals}
  \item{id}{a factor giving the identity of the animal for each
    relocation}
  \item{k}{if defined, the Fixed k LoCoH algorithm (k-NNCH) is used; the
    number of nearest neighbors minus one out of which to create convex
    hulls}
  \item{r}{if defined, the Fixed r LoCoH algorithm is used; the convex
    hulls are created out of all points within r distance from the root
    points}
 \item{a}{if defined, the Adaptive LoCoH algorithm is used; create
   convex hulls from the maximum number of nearest neighbors such that
   the sum of their distances is less than or equal to a} 
 \item{unin}{the units of the relocations coordinates.  Either
   \code{"m"} for meters or \code{"km"} for kilometers}
 \item{unout}{the units of the output areas.  Either \code{"m2"} for
   square meters, \code{"km2"} for square kilometers or \code{"ha"} for
   hectares}
  \item{status}{if \code{TRUE} print out occasional progress messages as
    we analyze the data}
  \item{duplicates}{a setting to determine how duplicated points are
    handled. If a number, duplicated points, are displaces
    \code{duplicates} amount in a random direction, if \code{"delete"}
    all but one copy of duplicated points are deleted, if
    \code{"ignore"} no special handling of duplicated points (could
    create zero area hulls)}
  \item{hog.limit}{ if less than the number of points, a slow but memory
  efficient algorithm is used}
  \item{min.k}{ for use with Fixed r LoCoH and Adaptive LoCoH a "floor"
  for the value of k (ie. if the value of k found using the algorithm is
  less than min.k, set k equal to min.k)}
  \item{max.k}{ for use with Fixed r LoCoH and Adaptive LoCoH a "ceiling"
  for the value of k (ie. if the value of k found using the algorithm is
  more than max.k, set k equal to max.k)}
}
\value{
  \code{NNCH} returns a list of class \code{NNCH}.\cr
}
\note{
 The LoCoH family of methods for locating Utilization Distributions
 consists of three algorithms: Fixed k LoCoH, Fixed r LoCoH, and
 Adaptive LoCoH.  All the algorithms work by constructing a small convex
 hull for each point, and then incrementally merging the hulls together
 from smallest to largest into isopleths.  The 10\% isopleth contains
 10\% of the points and represents a higher utilization the the 100\%
 isopleth that contains all the points.
 \cr
 \cr
 \bold{Fixed k LoCoH:} Also known as k-NNCH, Fixed k LoCoH is described
 in Getz and Willmers (2004).  The convex hull for each point is
 constructed from the (k-1) nearest neighbors to that point.  Hulls are
 merged together from smallest to largest based on the area of the
 hull.
 \cr
 \bold{Fixed r LoCoH:} In this case, hulls are created from all points
 within \code{r} distance of the root point.  When merging hulls, the
 hulls are primarily sorted by the value of k generated for each hull
 (the number of points contained in the hull), and secondly by the area
 of the hull.
 \cr
 \bold{Adaptive LoCoH:}  Here, hulls are created out of the maximum
 nearest neighbors such that the sum of the distances from the nearest
 neighbors is less than or equal to d.  Use the same hull sorting as
 Fixed r LoCoH.
 \cr
 Fixed r LoCoH and Adaptive LoCoH are discussed in a forthcoming paper
 (Getz et al).
 \cr
 All of these algorithms can take a signifigant amount of time.  Time
 taken increases exponentially with the size of the data set.
}
\author{Scott Fortmann-Roe \email{scottfr@gmail.com}\cr
  Clement Calenge \email{clement.calenge@oncfs.gouv.fr}}
\seealso{\code{\link{NNCH.select}} for plotting, rasterization,
  conversion to shapefiles, and management of the objects of class
  \code{NNCH}.  \code{\link{NNCH.area}} for functions computing the
  homerange area.}
\section{Warning}{
 These functions require the package \code{gpclib}.
}
\references{
Getz, W.M. & Wilmers, C.C. (2004). A local nearest-neighbor convex-hull
construction of home ranges and utilization
distributions. \emph{Ecography}, \bold{in press}.
\cr
Getz, W.M., Fortmann-Roe, S.B, Lyons, A., Ryan, S., Cross, P. (in
preparation). LoCoH methods for the construction of home
ranges and utilization distributions. \bold{in preparation}.
}
\examples{
\dontrun{
data(chamois)
xy <- chamois$locs

(nn <- NNCH(xy, k=c(6,7)))
summary(nn)
NNCH.select(nn, k=7)

## Graphical exploration
plot(nn, k=7)

## rasterization:
asc <- ascgen(chamois$locs,nrcol=100)
asc <- NNCH.asciigrid(nn, k=7, asc=asc)  
image(asc)
}

}
\keyword{spatial}
