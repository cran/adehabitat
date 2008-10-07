\name{simm.bb}
\alias{simm.bb}
\title{Brownian bridge motion}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simm.bb(date = 1:100, begin = c(0, 0), end = begin, id = "A1", burst = id)
}
\arguments{
  \item{date}{ ~~Describe \code{date} here~~ }
  \item{begin}{ ~~Describe \code{begin} here~~ }
  \item{end}{ ~~Describe \code{end} here~~ }
  \item{id}{ ~~Describe \code{id} here~~ }
  \item{burst}{ ~~Describe \code{burst} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(date=1:100, begin = c(0,0),
                     end=begin, id="A1", burst=id)
  {
    class(date) <- c("POSIX","POSIXct")
    n <- length(date)
    dt <- c(diff(unclass(date)),NA)
    
    dx <- c(rnorm(n-1,0,sqrt(dt[-n])),NA)
    dy <- c(rnorm(n-1,0,sqrt(dt[-n])),NA)
    W <- cbind(dx,dy)
    
    xtmp <- begin
    x <- begin            
    for (i in 2:(n-1)) {
      a <- diag(1/(date[n]-date[i-1]),2)
      dX <- as.numeric((a\%*\%(end - xtmp)*dt[i-1] + W[i-1,]))
      x <- rbind(x,xtmp+dX)
      xtmp <- xtmp+dX
    }
    x <- rbind(x,end)

    res <- as.ltraj(data.frame(x[,1],x[,2]),date, id, burst)
    return(res)
    
  }
}
\keyword{spatial}

