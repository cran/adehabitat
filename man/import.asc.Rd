\name{import.asc}
\alias{import.asc}
\alias{export.asc}
\alias{as.asc}
\alias{asc}
\alias{print.asc}
\title{ Arcview ASCII Raster File Importation And Exportation }
\description{
  \code{import.asc} imports Arcview ASCII raster file
  created with the Arcview command \code{"File -> Export data
    source"}.\cr
    Conversely,  \code{export.asc} allows to export an \code{asc}
    object toward Arcview.\cr
    ASCII raster files may also be created using the free program landserf
    (\url{http://www.soi.city.ac.uk/~jwo/landserf/}).
}
\usage{
import.asc(file, type = c("numeric", "factor"), lev = NULL,
           levnb = 1, labnb = 3)
export.asc(x, file)
as.asc(x, xll = 1, yll = 1, cellsize = 1, type = c("numeric", "factor"),
       lev = levels(factor(x)))
print.asc(x, \dots)
}

\arguments{
  \item{file}{a character string. The name of an Arcview ASCII Raster file}
  \item{type}{a character string. Either \code{"numeric"} or \code{"factor"}}
  \item{lev}{if \code{type == "factor"}, either a vector giving the
    labels of the factor levels, or the name of a file giving the
    correspondence table of the map (see details)}
  \item{levnb}{if \code{lev} is the name of a file containing a
    correspondence table exported from Arcview, the column number in
    this table where the factor levels are stored (i.e. the numbers indicating
    the levels of the factor)}
  \item{labnb}{if \code{lev} is the name of a file containing a
    correspondence table exported from Arcview, the column number in
    this table where the factor labels are stored (i.e. the character
    strings indicating the labels associated with each level of the factor)}
  \item{x}{an object of class \code{asc}.  For the function \code{as.asc}, a
    matrix}
  \item{xll}{the x coordinate of the center of the lower left pixel of the map}
  \item{yll}{the y coordinate of the center of the lower left pixel of the map}
  \item{cellsize}{the size of a pixel on the studied map}
  \item{\dots}{additionnal arguments to be passed to the function
    \code{print}}
}
\details{
The raster maps may be of type \code{"numeric"} (e.g. elevation on an area) or
\code{"factor"} (e.g. the type of vegetation
on an area).  If the map is of type \code{factor}, the levels should be
indicated.  The \code{".asc"} files store the values of the mapped variable 
with numeric values.  Each level of the factor is coded on the map by a
number.  The argument \code{lev} of \code{import.asc} or \code{as.asc}
gives the labels corresponding to each
number. Alternatively, these levels may be specified using a
correspondence table exported from Arcview (with this software, command
\code{"Theme -> table"}, then \code{"File -> Export"},
and finally export in delimited text format). An example of such file is
provided in the directory "ascfiles" of the package, see the examples below.
\code{export.asc} allows only exportation of numeric maps.
}
\value{
  Returns a raster matrix of the class \code{asc}, with the following
  attributes : 
  \item{xll}{the x coordinate of the center of the lower left pixel of
    the map}
  \item{yll}{the y coordinate of the center of the lower left pixel of
    the map}
  \item{cellsize}{the size of a pixel on the studied map}
  \item{type}{either \code{"numeric"} or \code{"factor"}.}
  \item{levels}{if \code{type == "factor"}, the levels of the factor.}
}
\references{Arcview: \url{http://www.esri.com}\cr
Landserf: \url{http://www.soi.city.ac.uk/~jwo/landserf/landserf170/}
}
\author{Clément Calenge \email{calenge@biomserv.univ-lyon1.fr} }

\seealso{ \code{\link{image.asc}} }

\examples{
\dontrun{
## Importation of asc files: numeric
## Path of the file to be imported
(file1 <-  paste(system.file(package = "adehabitat"),
               "ascfiles/Elevation.asc", sep = "/"))

el <- import.asc(file1)
image(el)
el


## Importation of asc files: factor
(file2 <- paste(system.file(package = "adehabitat"),
              "ascfiles/Aspect.asc", sep = "/"))
(levelfile <- paste(system.file(package = "adehabitat"),
              "ascfiles/Aspect.txt", sep = "/"))
asp <- import.asc(file2, lev = levelfile, type = "factor")
image(asp)
asp


## map of white noise
wafwaf <- matrix(rnorm(10000), 100, 100)
wafwaf <- as.asc(wafwaf)
image(wafwaf)

## exportation of a map
export.asc(wafwaf, "foo.asc")

## remove the created file:
file.remove("foo.asc")
}
}
\keyword{file}
