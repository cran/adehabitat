\name{NNCH}
\alias{NNCH}
\alias{print.NNCH}
\alias{plot.NNCH}
\alias{NNCH.area}
\alias{plot.NNCHver}
\alias{neighNNCH}
\alias{plot.ngNNCH}
\alias{getverticesNNCH}
\alias{NNCH.rast}
\title{Nearest Neighbor Convex hull }
\description{
  \code{NNCH} computes the home range of several animals using the
  nearest neighbor convex hull method (Getz and Wilmers, 2004).\cr
  \code{plot.NNCH} gives a graphical display of the estimation.\cr
  \code{NNCH.area} is used for home-range size estimation.\cr
  \code{neighNNCH} is used to determine the optimum number of neighbors
  (plots the home-range size estimated to a given percentage level
  according to the number neighbors used for the estimation).\cr
  \code{getverticesNNCH} is used to store the limits of the home
  ranges.\cr
  \code{plot.NNCHver} displays the limits of the home ranges (but see
  the section "note").\cr
  \code{NNCH.rast} is used to convert the home range to raster.
}
\usage{
NNCH(xy, id = NULL, k = 10, unin = c("m", "km"), unout = c("ha", "km2", "m2"))
plot.NNCH(x, which = names(x), add.points = TRUE, pch = 21,
          bgpts = "white", colpts = "black",
          cex = 0.7, add = FALSE, same4all = TRUE,
          border = NA, \dots)
NNCH.area(x, percent = c(95,90,80,70,60,50,40,30,20,10))
neighNNCH(xy, id = NULL, rangek, percent = 95,
          unin = c("m", "km"), unout = c("ha", "km2", "m2"))
plot.ngNNCH(x, ...)
getverticesNNCH(x, percent = 95)
plot.NNCHver<-function(x, which = names(x),
                       colpol = NA,
                       colborder = rep("black", length(x)),
                       lwd = 2, add = FALSE, ...)
NNCH.rast(y, w)
}
\arguments{
  \item{xy}{a data frame containing the coordinates of the relocation
    of the monitored animals}
  \item{id}{a factor giving the identity of the animal for each relocation}
  \item{k}{the number of neighbors to take into account in the estimation}
  \item{unin}{the units of the relocations coordinates.  Either
    \code{"m"} (default) for
    meters or \code{"km"} for kilometers}
  \item{unout}{the units of the output areas.  Either \code{"m2"} for
    square meters, \code{"km2"} for square kilometers or \code{"ha"} for
    hectares (default)}
  \item{x}{an object of class "\code{NNCH}".  For \code{plot.NNCHver},
    an object of class \code{NNCHver} returned by the function 
    \code{getverticesNNCH}.  For \code{plot.ngNNCH}, an object of class
    \code{ngNNCH} returned by the function \code{neighNNCH}}
  \item{which}{a vector of character strings, indicating the names of
    the animals to be plotted}
  \item{add.points}{logical.  \code{TRUE} if the relocations are to be
    plotted}
  \item{pch}{if \code{add.points} is \code{TRUE}, the size of the points
    to be used for the plot of the relocations (see \code{\link{par}})}
  \item{bgpts}{if \code{add.points} is \code{TRUE}, the background color to
    be used for the plot of the relocations (see \code{\link{par}})}
  \item{colpts}{if \code{add.points} is \code{TRUE}, the color of the
    points to be used for the plot of the relocations (see \code{\link{par}})}
  \item{cex}{if \code{add.points} is \code{TRUE}, the size of the
    points to be used for the plot of the relocations (see \code{\link{par}})}
  \item{add}{logical.  Whether the home ranges should be displayed on
    a previous plot}
  \item{border}{The color of the border of the polygons. \code{NA} if a
  border is not required}
  \item{same4all}{logical.  If \code{TRUE}, the plot displaying the home
    ranges has the same X and Y limits for all animals}
  \item{\dots}{additional arguments to be passed to the generic function
    \code{plot} }
  \item{percent}{for \code{NNCH.area}, a vector of percentage levels for
    home-range size estimation.  For \code{getverticesNNCH}, one value
    giving the percentage level for the home range estimation. }
  \item{colpol}{a vector of the color for filling the polygon.  The default,
    \code{NA}, is to leave polygons unfilled }
  \item{colborder}{a vector of the color to draw the border.  The
    default.  Use \code{border = NA} to omit borders}
  \item{lwd}{the border width, a \strong{positive} number}
  \item{w}{an object of class \code{asc} or \code{kasc}}
  \item{rangek}{a vector giving several numbers of neighbors to test}
}
\value{
  \code{NNCH} returns a list of class \code{NNCH}.\cr
  \code{NNCH.area} returns an object of class \code{hrsize}.\cr
  \code{neighNNCH} returns a matrix of class \code{ngNNCH}.\cr
  \code{getverticesNNCH} returns an object of class \code{NNCHver}.\cr
  \code{NNCH.rast} returns a raster map of class \code{asc} or \code{kasc}.
}
\section{Warning }{
  These functions require the package \code{gpclib}.
}
\references{
Getz, W.M. & Wilmers, C.C. (2004). A local nearest-neighbor convex-hull
construction of home ranges and utilization
distributions. \emph{Ecography}, \bold{in press}.
}
\author{Clément Calenge \email{calenge@biomserv.univ-lyon1.fr}}
\note{
  The NNCH method can lead to home ranges with holes.  In other words,
  inside the outer limits of the home ranges there are some areas that
  are not used by the animals.  Due to the presence of these holes, it
  is not possible to stores the home ranges with the class "area", as
  for the minimum convex polygon (see \code{?mcp}) or the kernel
  estimator (see \code{?kernelUD}).  Therefore, the functions of the
  package gpclib are used here to deal with this kind of home ranges.
  Note, however, that when the home ranges contain holes, the function
  \code{plot.NNCHver} does not displays the holes correctly (filled in
  with the specified color).\cr

  The home range can be estimated at different percentage levels.  These
  levels correspond the percentage of relocations included within the limits
  of the home range.  Note, however, that a given percentage level can
  correspond to a collection of different home ranges (see example
  below).  Here, we define the NNCH home-range as the \emph{larger}
  area including the specified percentage.\cr

  Finally, note that this function is very long with large datasets.
}
\seealso{\code{\link{plot.hrsize}} to display the home-range size
  according to the percentage level of the estimation.}
\examples{
\dontrun{

## Can also be used to analyse a point pattern
## For example, locations of chamois in
## the Chartreuse mountains (French Alps)

data(chamois)
xy <- chamois$locs

## choice of the number of neighbors
## Warning: this command can be very long (more than two minutes)
u <- neighNNCH(xy, rangek = 5:30)
plot(u)

## 13 and 20 neighbours seem to be a good choice
## Estimates the home range levels
(aa <- NNCH(xy, k = 13))
plot(aa)

## plot of the home-range size
plot(NNCH.area(aa))

## gets the home range contour
cont <- getverticesNNCH(aa)
plot(cont)

## rasterizes these contours
sl <- getkasc(chamois$map, "Slope")
mm <- NNCH.rast(cont, sl)
image(mm)

## Note that two different sets of polygons can include
## the same percentage of relocation
aa[[1]]$area[97:105,]

## For example, the same percentage of relocations is
## included within the limits of the two home ranges
## (91.92 percent of the localisations) 
opar <- par(mfrow=c(1,2))
plot(aa[[1]]$polygons[[103]],
     poly.args = list(lwd = 2))
points(xy, pch = 16, cex = 0.5)

plot(aa[[1]]$polygons[[98]],
     poly.args = list(lwd = 2))
points(xy, pch=16, cex = 0.5)
par(opar)
## only the left-hand plot is considered as an
## home range estimation (the larger one)


}

}
\keyword{hplot}
