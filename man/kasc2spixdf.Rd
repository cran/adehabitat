\name{kasc2spixdf}
\alias{kasc2spixdf}
\alias{asc2spixdf}
\alias{spixdf2kasc}
\alias{area2sr}
\alias{sr2area}
\alias{attsr2area}
\alias{traj2spdf}
\alias{traj2sldf}
\title{ Conversion of maps from/to the package "sp" }
\description{
  These functions convert maps of classes available in adehabitat toward
  classes available in the package \code{sp} and conversely.\cr
  \cr
  \code{kasc2spixdf} converts an object of class \code{kasc} into an
  object of class \code{SpatialPixelsDataFrame}.\cr
  \cr
  \code{asc2spixdf} converts an object of class \code{asc} into an
  object of class \code{SpatialGridDataFrame}.\cr
  \cr
  \code{spixdf2kasc} converts an object of class
  \code{SpatialPixelsDataFrame} or \code{SpatialGridDataFrame} into an
  object of class \code{asc} or \code{kasc}.\cr 
  \cr
  \code{area2sr} converts an object of class \code{area} into an
  object of class \code{SpatialRings}.\cr
  \cr
  \code{sr2area} converts an object of class \code{SpatialRings} or
  \code{SpatialRingsDataFrame} into anobject of class
  \code{area}.\cr
  \cr
  \code{attsr2area} gets the data attribute of an object of class
  \code{SpatialRingsDataFrame} and stores is into a data frame.\cr
  \cr
  \code{traj2spdf} converts an object of class \code{traj} into an
  object of class \code{SpatialPointsDataFrame}.\cr
  \cr
  \code{traj2sldf} converts an object of class \code{traj} into an
  object of class \code{SpatialLinesDataFrame}.\cr
}
\usage{
kasc2spixdf(ka)
asc2spixdf(a)
spixdf2kasc(sg)
area2sr(ar)
sr2area(sr)
attsr2area(srdf)
traj2spdf(tr)
traj2sldf(tr, byid = FALSE)
}
\arguments{
  \item{ka}{an object of class \code{kasc}.}
  \item{a}{an object of class \code{asc}.}
  \item{sg}{an object of class \code{SpatialPixelsDataFrame} or
    \code{SpatialGridDataFrame}.}
  \item{ar}{an object of class \code{area}.}
  \item{sr}{an object of class \code{SpatialRings} or
    \code{SpatialRingsDataFrame}.}
  \item{srdf}{an object of class \code{SpatialRingsDataFrame}.}
  \item{tr}{an object of class \code{traj}.}
  \item{byid}{logical.  If \code{TRUE}, one objects of class
    \code{Slines} correspond to one animal.  if \code{FALSE},
    one object of class \code{Slines} correspond to one burst.}
}
\details{
  We describe here more in detail the functions \code{sr2area} and
  \code{attsr2area}.  Objects of class \code{area} do not deal with
  holes in the polygons, whereas the objects of class
  \code{SpatialRings} do.  Therefore, when holes are present in the
  \code{SpatialRings} object passed as argument, the function
  \code{sr2area} ignore them and returns only the external contour of
  the polygon (though a warning is returned, see example).
}
\author{ Clément Calenge \email{calenge@biomserv.univ-lyon1.fr} }
\seealso{ \code{\link{import.asc}} for information on objects of
  class \code{asc}, \code{\link{as.kasc}} for info on objects of
  class \code{kasc}, \code{\link{as.area}} for info on objects of
  class \code{area}, \code{\link{as.traj}} for objects of class \code{traj}.}
\examples{
\dontrun{
if (require(sp) {

#########################################
##
## Conversion kasc -> SpatialPixelsDataFrame
##

data(puechabon)
toto <- kasc2spixdf(puechabon$kasc)
image(toto)
summary(toto)

#### and conversely
toto <- spixdf2kasc(toto)
image(toto)
hist(toto)

data(meuse.grid)
m <- SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")],
                           data = meuse.grid)
i <- spixdf2kasc(m)
image(i)


### conversion asc -> SpatialPixelsDataFrame
cuicui <- asc2spixdf(getkasc(toto,1))
image(cuicui)


#########################################
##
## Conversion area -> SpatialRings
##

data(elec88)
ar <- as.area(elec88$area)
plot(ar)
toto <- area2sr(ar)
plot(toto)


#########################################
##
## Conversion SpatialRings -> area
##

## First create an object of class "SpatialRingsDataFrame"
data(ncshp)
nc1 <- as.SpatialRings.Shapes(nc.shp$Shapes, as.character(nc.shp$att.data$FIPS))
plotSpatialRings(nc1)
df <- nc.shp$att.data
rownames(df) <- as.character(nc.shp$att.data$FIPS)
ncSRDF <- SpatialRingsDataFrame(nc1, df)


## and then conversion:
coincoin <- sr2area(ncSRDF)
## please note the warnings

plot(coincoin)
## gets the attributes
haha <- attsr2area(ncSRDF)
area.plot(coincoin, values = df$SID74/df$BIR74)


#########################################
##
## Conversion traj -> SpatialPointsDataFrame
##

data(puechcirc)
plot(puechcirc)

toto <- traj2spdf(puechcirc)
plot(toto)


#########################################
##
## Conversion traj -> SpatialLinesDataFrame
##

data(puechcirc)
plot(puechcirc)

toto <- traj2sldf(puechcirc)
plot(toto)

}
}
}
\keyword{hplot}
