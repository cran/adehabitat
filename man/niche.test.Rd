\name{niche.test}
\alias{niche.test}
\title{Monte-Carlo Test on Parameters of the Ecological Niche}
\description{
  \code{niche.test} tests for the significance of two parameters of the
  ecological niche of a species (marginality and tolerance), using
  Monte-Carlo methods.  This is a bivariate test.
}
\usage{
niche.test(kasc, points, nrep = 999, h, o.include = FALSE,
           colZ = "blue", colS = "orange", \dots) 
}
\arguments{
  \item{kasc}{a raster map of class \code{kasc}}
  \item{points}{a data frame with two columns, giving the coordinates of
    the species locations}
  \item{nrep}{the number of permutations}
  \item{h}{vector of bandwidths for x and y directions passed to
    \code{biv.test}, used in the function \code{kde2d} of the package
    \code{MASS}.  Defaults to normal reference bandwidth (see \code{?kde2d}).}
  \item{o.include}{logical, passed to \code{biv.test}.  If \code{TRUE},
    the origin is included in the plot}
  \item{colZ}{a color passed to \code{biv.test} for the color of the kernel
    density estimation of the randomized values}
  \item{colS}{a color passed to \code{biv.test} for the actual values of
    marginality and tolerance of the object of class \code{enfa}}
  \item{\dots}{further arguments passed to or from other methods}
}
\details{
  \code{niche.test} tests the significance of two parameters describing
  the ecological niche: the marginality
  (squared length of the vector linking the average available habitat
  conditions to the average used habitat conditions in the ecological
  space defined by the habitat variables), and the tolerance (inertia of
  the niche in the ecological space, i.e. the sum over all variables of
  the variance of used pixels). \cr
  
  At each step of the randomisation procedure, 
  the test randomly allocates the n points in the pixels of the map.
  The marginality and the tolerance are then recomputed on this
  randomised data set.\cr
  
  Actual values are compared to random values with the help of the
  function \code{biv.test}.
}
\value{
Returns a list containing the following
components: 
\item{dfxy}{a data frame with the randomized values of marginality
  (first column) and tolerance (second column).}
\item{obs}{the actual value of marginality and tolerance.}
}
\author{
  Mathieu Basille \email{basille@biomserv.univ-lyon1.fr}\cr
  Clément Calenge \email{calenge@biomserv.univ-lyon1.fr}
}
\section{Warning}{
  \code{biv.test} uses the function \code{kde2d} of the package \code{MASS}.
}
\seealso{\code{\link{biv.test}} for more details on bivariate tests. }
\examples{
\dontrun{
data(lynxjura)

## We keep only "wild" indices.
tmp=lynxjura$loc[,4]!="D"
if (require(MASS))
   niche=niche.test(lynxjura$map,
                    lynxjura$locs[tmp, c("X", "Y")])
}
}
\keyword{multivariate}
