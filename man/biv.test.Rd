\name{biv.test}
\alias{biv.test}
\title{Bivariate Test}
\description{
  \code{biv.test} displays the results of a bivariate randomisation test.
}
\usage{
biv.test(dfxy, point, cbreaks = 8, h, colD = "blue", colP = "orange",
         o.include = FALSE, rem = NULL, \dots) 
}
\arguments{
  \item{dfxy}{a data frame with N lines (couples of values) and two
    columns}
  \item{point}{a vector of length 2, representing
    the observation to be compared with the simulated values of the
    randomisation test}
  \item{cbreaks}{a parameter used to define the numbers of breaks of the
    histograms.  A larger value leads to a larger number of breaks}
  \item{h}{vector of bandwidths for x and y directions, used in the
    function \code{kde2d} of the package \code{MASS}.  Defaults to normal
    reference bandwidth (see \code{?kde2d})}
  \item{colD}{a color used for the contours}
  \item{colP}{a color used for the observation}
  \item{o.include}{logical.  If \code{TRUE}, the origin is included in
    the plot}
  \item{rem}{a character string to be inserted in the plot}
  \item{\dots}{further arguments passed to or from other methods}
}
\details{
  \code{biv.test} is used to display the results of a bivariate
  randomisation test.  An example of use of the function is provided in
  the function \code{niche.test}.
  
  The x-axis of the main window corresponds to the first column of
  \code{dfxy}; the y-axis corresponds to the second column.  Kernel
  density is estimated to indicate the contours of the distribution of
  randomised values.  The two marginal histograms correspond to the
  univariate tests on each axis, for which the p-values are computed with
  \code{as.randtest} (one-sided tests).
}
\author{Mathieu Basille \email{basille@biomserv.univ-lyon1.fr}}
\section{Warning}{
  \code{biv.test} uses the function \code{kde2d} of the package
  \code{MASS}.
}
\seealso{
  \code{\link[ade4]{as.randtest}}, \code{\link{niche.test}}
}
\examples{
\dontrun{
if (require(MASS)) {
  dfxy <- data.frame(x = rnorm(1000,5), y = rnorm(1000,5))
  p <- c(3.5, 3.5)
  biv.test(dfxy, p, rem = "Bivariate\nnormal distribution")
}
}
}
\keyword{multivariate}
