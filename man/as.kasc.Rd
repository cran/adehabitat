\name{as.kasc}
\alias{as.kasc}
\alias{kasc}
\alias{getkasc}
\alias{image.kasc}
\alias{print.kasc}
\title{Working with Several Raster Maps}
\description{
  \code{as.kasc} converts a list of matrices of class \code{asc} into a
  data frame of class \code{kasc}.\cr
  \code{getkasc} converts one of the components of a data frame of class
  \code{kasc} into a matrix of class \code{asc}.\cr
  \code{image.kasc} displays a image of maps of
  class \code{kasc}.
}
\usage{
as.kasc(l)
getkasc(x, var)
image.kasc(x, mar = c(0, 0, 2, 0), var = names(x), 
           axes = FALSE, clfac = NULL, col = gray((240:1)/256),
           mfrow = NULL, \dots)
print.kasc(x, \dots)
}

\arguments{
  \item{l}{a list of objects of class \code{asc}}
  \item{x}{an object of class \code{kasc}}
  \item{var}{for \code{getkasc}, a character string and for
    \code{image.kasc}, a vector of character strings, giving the name
    of one (or several) variable(s) in x }
  \item{mar}{this parameter is passed as the parameter \code{mar} of the
    function \code{par} (see \code{help(par)}).}
  \item{axes}{logical.  If \code{TRUE}, axes are drawn on the maps.}
  \item{clfac}{an optional list of vectors of character strings. Each
    element of the list is a vector corresponding to one factor in \code{x}.
    Each element of the vector gives the color name associated with each
    level of the corresponding factor (see Examples).}
  \item{col}{a character vector.  For numeric maps, the
    colors to be used}
  \item{mfrow}{A vector of the form \code{c(nr, nc)}. Subsequent
    figures will be drawn in an \code{nr}-by-\code{nc} array on the
    device by rows}
  \item{\dots}{additional parameters to be passed to the generic 
    functions \code{image} and \code{print}}
}
\value{
  \code{as.kasc} returns an object of class \code{kasc}.  The matrices
  of class \code{asc} are converted into vectors and coerced into a data
  frame of class \code{kasc}.  This data frame has the following
  attributes:
  
  \item{xll}{the X-coordinate of the center of the lower left
    pixel of the map}
  \item{yll}{the Y-coordinate of the center of the lower left
    pixel of the map}
  \item{cellsize}{the size of a pixel}
  \item{nrow}{the number of rows of the map.  Caution ! the number of
    rows of the raster map is the number of columns of the matrix of
    class \code{asc}}
  \item{ncol}{the number of columns of the map.  Caution ! the number of
    columns of the raster map is the number of rows of the matrix of
    class \code{asc}}

  The different maps in the input list of objects of class \code{asc}
  should describe the same area (same attributes: same \code{cellsize},
  same \code{xll}, same \code{yll}, and same dimensions). 
}
\author{ Clément Calenge \email{calenge@biomserv.univ-lyon1.fr} }
\seealso{ \code{\link{import.asc}} for additionnal information on
  objects of class \code{asc}}

\examples{

data(puechabon)

## kasc is a list of class kasc
(kasc <- puechabon$kasc)


## Stores two elements of the kasc in objects
## of class asc
(asp <- getkasc(kasc, "Aspect"))
(sl <- getkasc(kasc, "Slope"))


## Displays the slopes on the area (numeric)
image(sl, main = "Aspect", xlab = "Lambert X", ylab = "Lambert Y")


## Aspect is a factor:
## cl is the list of color associated with the levels
## of asp
cl <- colasc(asp, NorthEast = "blue", SouthEast = "red", 
             SouthWest = "orange", NorthWest = "green")
image(asp, clfac = cl, main = "Aspect", xlab = "Lambert X",
      ylab = "Lambert Y")
legend(706500, 3162000, legend=levels(asp), fill=cl, cex=0.7)



## Creation of a new kasc with elevation 
## and slopes
cuicui <- as.kasc(list(Slope = sl, Aspect = asp))

## Displays the kasc object
## with random colors for aspect and grey levels for slopes
image(cuicui)

## with cm.colors for slopes (numeric)
## and cl for aspect (factor)
image(cuicui, col = cm.colors(256), clfac = list(Aspect = cl))
}
\keyword{programming}
\keyword{hplot}
