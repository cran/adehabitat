\name{as.ltraj}
\alias{as.ltraj}
\alias{ltraj}
\alias{print.ltraj}
\alias{summary.ltraj}
\alias{traj2ltraj}
\alias{ltraj2traj}
\title{Working with Trajectories in 2D Space: the Class ltraj}
\description{
  The class \code{ltraj} is intended to store trajectories of
  animals monitored using radio-tracking.\cr
  \code{as.ltraj} creates an object of this class.\cr
  \code{summary.ltraj} returns the number of relocations for each "burst"
  of relocations and each animal.\cr
  \code{traj2ltraj}, and the reciprocal function \code{ltraj2traj}
  respectively converts an object of class \code{ltraj} to an object of
  class \code{traj}, and conversely.
}
\usage{
as.ltraj(xy, date, id, burst = id, slsp = c("remove", "missing"))
print.ltraj(x, \dots)
summary.ltraj(object, \dots)
traj2ltraj(traj, slsp =  c("remove", "missing"))
ltraj2traj(x)
}
\arguments{
  \item{x, object}{an object of class \code{ltraj}}
  \item{xy}{a data.frame containing the x and y coordinates of the
  relocations} 
  \item{date}{a vector of class \code{POSIXct} giving the date for
  each relocation}
  \item{id}{a character string indicating the identity of the animal}
  \item{burst}{a character string indicating the identity of the burst
  of relocations}
  \item{slsp}{a character string used for the computation of the
    turning angles (see details)}
  \item{traj}{an object of class \code{traj}}
  \item{\dots}{For other functions, arguments to be passed to the generic
    functions \code{summary} and \code{print}}
}
\details{ 
  Objects of class \code{ltraj} allow the analysis of animal
  movements.  They contain the information generally used in such
  studies (angles, length of moves, increases in the x and y
  direction, etc., See below).\cr 
  
  For a given individual, trajectories are often sampled as "bursts"
  of relocations (Dunn and Gipson, 1977).  For example,
  when an animal is monitored using radio-tracking, the data may consist
  of several circuits of activity (two successive relocations on one
  circuit are often highly autocorrelated, but the data from two
  circuits may be sampled at long intervals in time).  These bursts are
  indicated by the attribute \code{burst}. Note that the bursts should
  be unique: do not use the same burst id for bursts collected on
  different animals.\cr

  The computation of turning angles may be problematic when successive
  relocations are located at the same place.  In such cases, at least
  one missing value is returned.  For example, let r1, r2, r3 and r4 be
  4 successive relocations of a given animal (with coordinates (x1,y1),
  (x2,y2), etc.).  The turning angle in r2 is computed between the moves
  r1-r2 and r2-r3.  If r2 = r3, then a missing value is returned for the
  turning angle at relocation r2.  The argument \code{slsp} controls the
  value returned for relocation r3 in such cases.  If \code{slsp ==
    "missing"}, a missing value is returned also for the relocation r3.
  If \code{slsp == "remove"}, the turning angle computed in r3 is the
  angle between the moves r1-r2 and r3-r4.\cr
}
\value{
  \code{summary.ltraj} returns a data frame.\cr
  \code{ltraj2traj} returns an object of class \code{traj}.\cr
  All other functions return objects of class \code{ltraj}.  An object
  of class \code{ltraj} is a list with one component per burst of
  relocations.  Each component is a data frame with two attributes:
  the attribute \code{"id"} indicates the identity of the animal, and
  the attribute \code{"burst"} indicates the identity of the
  burst. Each data frame stores the following columns:
\item{x}{the x coordinate for each relocation}
\item{y}{the y coordinate for each relocation}
\item{date}{the date for each relocation}
\item{dx}{the increase of the move in the x direction}
\item{dy}{the increase of the move in the y direction}
\item{dist}{the length of each move}
\item{dt}{the time interval between successive relocations}
\item{R2n}{the squared net displacement between the current relocation
  and the first relocation of the trajectory}
\item{abs.angle}{the angle between each move and the x axis}
\item{rel.angle}{the turning angles between successive moves.}
}
\references{
  Dunn, J.E. and Gipson, P.S. (1977) Analysis of radio telemetry data in
  studies of home range. \emph{Biometrics}. \bold{59}, 794--800.
}
\author{
  Clément Calenge \email{calenge@biomserv.univ-lyon1.fr}\cr
  Stéphane Dray \email{dray@biomserv.univ-lyon1.fr}
}
\note{
  The class \code{ltraj} is a better alternative to the class
  \code{traj}. Indeed, the class \code{ltraj} already contains the
  basic information needed for the modelling of movement processes. In
  a close future, many functions will be added to adehabitat, allowing
  such a modelling.\cr

  Furthermore, note that the computation of the
  turning angles is faster with \code{as.ltraj} than with
  \code{angles}.
}
\seealso{ \code{\link{c.ltraj}} to combine several objects of class
  \code{ltraj}, \code{\link{Extract.ltraj}} to extract or replace
  bursts of relocations, \code{\link{plot.ltraj}} for graphical
  displays, \code{\link{gdltraj}} to specify a time period.  For further
  information on the class \code{traj}, see \code{\link{as.traj}}. }
\examples{

data(puechabon)
locs <- puechabon$locs
locs[1:4,]
lixy <- split(locs[,c("X","Y")],locs$Name)

### Conversion of the date to the format POSIX
da <- as.character(locs$Date)
da <- as.POSIXct(strptime(as.character(locs$Date),"\%y\%m\%d"))
lidat <- split(da,locs$Name)


### Creation of an object of class "ltraj", with for 
### example the first animal
(tr1 <- as.ltraj(lixy[[1]], date = lidat[[1]], id="Brock"))

## The components of the object of class "ltraj"
head(tr1[[1]])

## With all animals
litr <- lapply(1:4, function(i) as.ltraj(lixy[[i]], 
               date = lidat[[i]], id = names(lidat)[i]))

(tr <- do.call("c.ltraj", litr))

}
\keyword{spatial}
