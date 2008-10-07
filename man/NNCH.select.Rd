\name{NNCH.select}
\alias{NNCH.select}
\alias{summary.NNCH}
\alias{print.NNCH}
\alias{plot.NNCH}
\alias{NNCH.shapefile}
\alias{NNCH.asciigrid}
\title{LoCoH (NNCH): Useful Functions}
\description{
  \code{NNCH.select} is to be used when a home range has been analyzed
  for multiple k's, r's, a's, and/or id's.  This function will select a
  subset based on user defined conditions of the the original home
  range and return a new home range object containing the subset.  Most
  NNCH functions will accept \code{NNCH.select} parameters and call the
  function before analyzing data.\cr
  \code{summary.NNCH} prints out a lengthy data summary containing
  rudimentary analysis of the data. Can be saved to a text file.\cr
  \code{plot.NNCH} generates a plot of the home range.\cr
  \code{NNCH.asciigrid} creates a raster map of the home range which can
  be exported for use by ArcMap and other GIS programs.\cr
  \code{NNCH.shapefile} creates a representation of the home range as a
  polygon shapefile object (can be saved to a shape file thanks to the
  package \code{shapefile}).
}
\usage{
NNCH.select(x,id = NULL, k = NULL, r = NULL, a = NULL)
\method{print}{NNCH}(x, \dots)
\method{summary}{NNCH}(object,file='', id = NULL, k = NULL, r = NULL,
             a = NULL, \dots)
\method{plot}{NNCH}(x, add.points = TRUE, pch = 21, bgpts = "white",
          colpts = "black", cex = 0.7, add = FALSE,
          same4all = TRUE, border = NA, percent = rev(seq(10,100, by=10)),
          gr = rev(grey(vectorize.parameter(percent)/100*.9)),
          id = NULL, k = NULL, r = NULL, a = NULL, \dots)
NNCH.asciigrid(x, asc = NULL, cellsize = 1,
               percent = rev(seq(10,100, by = 10)),
               id = NULL, k = NULL, r = NULL, a = NULL)
NNCH.shapefile(x, percent= rev(seq(10,100, by = 10)),
               k = NULL, id = NULL, r = NULL, a = NULL)
}
\arguments{
 \item{x,object}{a home range object of class NNCH generated using the method
   \code{NNCH}}
 \item{id}{character string specifying a subset of \code{x}}
 \item{k,r,a}{numeric values specifying a subset of \code{x}}
 \item{file}{a character string specifying the name of a textfile the
   summary of the homerange is to be saved.  If not specified, summary
   is printed to the screen}
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
 \item{same4all}{logical.  If \code{TRUE}, the plot displaying the home
   ranges has the same X and Y limits for all animals}
 \item{border}{The color of the border of the polygons. \code{NA} if a
   border is not required}
 \item{percent}{A vector of the isopleths to use when plotting or saving
   the homerange.}
 \item{gr}{A vector of colors the same length as the vector of
   isopleths. Used to color the isopleths on the plot.} 
 \item{asc}{an object of class \code{asc}}
 \item{cellsize}{the size of the pixel of the raster map.}
 \item{\dots}{additional arguments to be passed to the generic functions
   \code{plot}, \code{print} and \code{summary}. }
}
\value{
 \code{NNCH.select} returns an object of class \code{NNCH} containing a
 subset of the original data.\cr 
 \code{NNCH.asciigrid} returns an object of class \code{asc}.\cr
 \code{NNCH.shapefile} returns an object of class \code{shapefile}.\cr
}
\author{Scott Fortmann-Roe \email{scottfr@gmail.com}\cr
  Clement Calenge \email{clement.calenge@oncfs.gouv.fr}}
\seealso{ \code{\link{NNCH}} for further information about the NNCH
  method}
\section{Warning}{
  The function \code{NNCH.shapefile} relies on the package
  \code{shapefile}.
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

if (require(shapefiles)) {
  (nns <- NNCH.shapefile(nn))
  }
}

}
\keyword{spatial}
