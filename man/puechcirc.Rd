\name{puechcirc}
\alias{puechcirc}
\docType{data}
\title{Movements of wild boars tracked at Puechabon}
\description{
This data set is an object of class \code{traj}, giving the results of the
monitoring of 2 wild boars by radio-tracking at Puéchabon (Mediterranean
habitat, South of France).  These data have been
collected by Daniel Maillard (Office national de la chasse et de la
faune sauvage), and correspond to the activity period of the wild boar
(during the night, when the animals forage. The data set
\code{puechabon} describes the resting sites). 
}
\usage{data(puechcirc)}
\format{
  This data frame has 204 observations, and 5 variables:
  \describe{
    \item{id}{a factor with levels \code{CH93} and \code{JE93}, giving the
      name of the animals}
    \item{x}{a vector giving the x coordinates of the relocations}
    \item{y}{a vector giving the y coordinates of the relocations}
    \item{date}{a POSIXct giving the date of collection of the relocations}
    \item{burst}{a factor with levels \code{CH930803}, \code{CH930824},
      \code{CH930827}, \code{JE930819}, and \code{JE930827}, giving the
      ID of the circuits.}
  }
}
\source{
Maillard, D. (1996). \emph{Occupation et l'utilisation de la garrigue et
  du vignoble méditerranéens par le Sanglier}.
Université d'Aix-Marseille III: PhD thesis.
}
\keyword{datasets}
