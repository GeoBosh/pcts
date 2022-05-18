\name{Cyclic-class}
\Rdversion{1.1}
\docType{class}
\alias{Cyclic-class}
%\alias{nSeasons,Cyclic-method}
%\alias{pcCycle,Cyclic-method}

\title{Class \code{"Cyclic"}}
\description{Class \code{"Cyclic"}}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Cyclic", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{cycle}:}{Object of class \code{"BasicCycle"} ~~ }
    \item{\code{pcstart}:}{Object of class \code{"ANY"} ~~ }
}
}
\section{Methods}{
  %%%    \item{nSeasons}{\code{signature(object = "Cyclic")}: ... }
  %%%    \item{pcCycle}{\code{signature(x = "Cyclic")}: ... }
  \describe{
    \item{allSeasons}{\code{signature(x = "Cyclic", abb = "ANY")}: ... }
    \item{allSeasons<-}{\code{signature(x = "Cyclic")}: ... }
    \item{as_date}{\code{signature(x = "Cyclic")}: ... }
    \item{coerce}{\code{signature(from = "PeriodicMTS", to = "Cyclic")}: ... }
    \item{coerce}{\code{signature(from = "PeriodicTS", to = "Cyclic")}: ... }
    \item{coerce<-}{\code{signature(from = "PeriodicMTS", to = "Cyclic")}: ... }
    \item{coerce<-}{\code{signature(from = "PeriodicTS", to = "Cyclic")}: ... }
    \item{date<-}{\code{signature(x = "Cyclic")}: ... }
    \item{nSeasons}{\code{signature(object = "Cyclic")}: ... }
    \item{nTicks}{\code{signature(x = "Cyclic")}: ... }
    \item{pcCycle}{\code{signature(x = "Cyclic", type = "ANY")}: ... }
    \item{seqSeasons}{\code{signature(x = "Cyclic")}: ... }
    \item{show}{\code{signature(object = "Cyclic")}: ... }
    \item{unitCycle}{\code{signature(x = "Cyclic")}: ... }
    \item{unitCycle<-}{\code{signature(x = "Cyclic")}: ... }
    \item{unitSeason}{\code{signature(x = "Cyclic")}: ... }
    \item{unitSeason<-}{\code{signature(x = "Cyclic")}: ... }
  }
}
%\references{
%%%  ~~put references to the literature/web site here~~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{Pctime}} for conversion from/to dates and datetimes.
}
\examples{
showClass("Cyclic")
}
\keyword{classes}
