\name{kselect}
\alias{kselect}
\alias{print.kselect}
\alias{hist.kselect}
\alias{kplot.kselect}
\alias{plot.kselect}
\title{ K-Select Analysis: a Method to Analyse the Habitat Selection by
  Animals  }
\description{
Performs a multivariate analysis of ecological data (K-select analysis).
}
\usage{
kselect(dudi, factor, weight, scannf = TRUE, nf = 2, ewa = FALSE)
print.kselect(x, \dots)
kplot.kselect(object, xax = 1, yax = 2, csub = 2, possub = c("topleft",
              "bottomleft", "bottomright", "topright"),
              addval = TRUE, cpoint = 1, csize = 1, clegend = 2, \dots)
hist.kselect(x, xax = 1, mar = c(0, 0, 0, 0), ampl = 1, 
             col.out = gray(0.75), col.in = gray(0.75), ncell = TRUE,
             denout = NULL, denin = NULL, lwdout = 1, lwdin = 1,
             maxy = 1, csub = 2, possub =
             c("bottomleft", "topleft", "bottomright", "topright"),
             ncla = 15, \dots) 
plot.kselect(x, xax = 1, yax = 2, \dots)
}
\arguments{
  \item{dudi}{an object of class \code{dudi}}
  \item{factor}{a factor with the same length as \code{nrow(dudi$tab)}}
  \item{weight}{a numeric vector giving the weight associated to the
    rows of \code{dudi$tab}}
  \item{scannf}{logical. Whether the eigenvalues bar plot
    should be displayed}
  \item{nf}{if \code{scannf = FALSE}, an integer indicating the number
    of kept axes}
  \item{ewa}{logical. If \code{TRUE}, uniform weights are given
    to all animals in the analysis. If \code{FALSE}, animal weights are
    given by the proportion of relocations of each animal (i.e. an
    animal with 10 relocations has a weight 10 times lower than an
    animal with 100 relocations)}
  \item{x}{an object of class \code{kselect}}
  \item{object}{an object of class \code{kselect}}
  \item{xax}{the column number for the x-axis}
  \item{yax}{the column number for the y-axis}
  \item{addval}{logical.  If \code{TRUE}, the frequency of the
    relocations per animal is displayed (see examples)}
  \item{cpoint}{the size of the points (if 0, the points where no
    relocations are found are not displayed)}
  \item{mar}{the margin parameter (see \code{help(par)}).}
  \item{ampl}{the amplification factor (i.e. \code{ylim = c(-1 , 1) /
      ampl})} 
  \item{col.out}{character string.  The color of the upper histogram}
  \item{col.in}{character string.  The color of the lower histogram}
  \item{ncell}{logical. Parameter to be set in the case where the
    distribution of the relocations is to be drawn (either \code{type =
      "locs.in.hr"} or \code{type = "locs.in.sa"}).
    If \code{TRUE}, the histogram shows the distribution of the cells of
    the raster map where at least one relocation is found. If
    \code{FALSE}, the histogram shows the distribution of the
    relocations}
  \item{denout}{the density of shading lines for the
    upper histogram, in lines per inch (see
    \code{help(hist)} for further informations)}
  \item{denin}{the density of shading lines for the
    lower histogram, in lines per inch}
  \item{lwdout}{the line width for the upper histogram}
  \item{lwdin}{the line width for the lower histogram}
  \item{maxy}{the maximum Y coordinate (since the histogram draws
    frequencies, default value of \code{maxy} is 1)}
  \item{csub}{the character size for the legend, used with
    \code{par("cex")*csub}}
  \item{csize}{the size coefficient for the points}
  \item{clegend}{the character size for the legend used by
    \code{par("cex")*clegend}}
  \item{possub}{a character string indicating the sub-title position
    \code{("topleft", "topright", "bottomleft", "bottomright")}}
  \item{ncla}{the number of classes of the histograms}
  \item{\dots}{additional arguments to be passed to the generic function
  \code{hist}, \code{print} or, in the case of \code{plot.kselect},
  \code{s.distri}}
}
\value{
  Returns a list of the class \code{kselect} and \code{dudi} (see
  \code{\link[ade4]{dudi}}). 
}
\references{
  Calenge, C., Dufour, A.B. and Maillard,
  D. (\emph{submitted}). K-select analysis, a new method to analyse
  habitat selection in radio-tracking studies.
}
\author{Clément Calenge \email{calenge@biomserv.univ-lyon1.fr} }

\seealso{\code{\link{sahrlocs2kselect}} for
  conversion of objects class \code{sahrlocs} to objects suitable for a
  K-select analysis, \code{\link[ade4]{s.distri}}, and
  \code{\link[ade4]{dudi}} for class \code{dudi}.}

\examples{

\dontrun{
## Loads the data
data(puechabon)
sahr <- puechabon$sahr

## prepares the data for the kselect analysis
x <- sahrlocs2kselect(sahr)
tab <- x$tab

## Example of analysis with two variables: the slope and the elevation.
## Have a look at the use and availability of the two variables
## for the 4 animals
tab <- tab[,((names(tab) == "Slope")|(names(tab) == "Elevation"))]
tab <- scale(tab)
tmp <- split.data.frame(tab, x$factor)
wg <- split(x$weight, x$factor)
opar <- par(mfrow = n2mfrow(nlevels(x$factor)))
for (i in names(tmp))
  s.distri(scale(tmp[[i]]), wg[[i]])
par(opar)

x11()
## A K-select analysis
acp <- dudi.pca(tab, scannf = FALSE, nf = 2)
kn <- kselect(acp, x$factor, x$weight,
 scannf = FALSE, nf = 2)

# use of the generic function scatter
scatter(kn)

# Displays the first factorial plane
kplot(kn)
kplot(kn, cellipse = 0, cpoint = 0)
kplot(kn, addval = FALSE, cstar = 0)

# this factorial plane can be compared with
# the other graph to see the rotation proposed by
# the analysis
graphics.off()

# Displays the first factorial axis
hist(kn)

# Displays the second factorial axis
hist(kn, xax = 2)

# Summary of the analysis
plot(kn)
}
}
\keyword{multivariate}
\keyword{hplot}
