\name{Cyclic}
\alias{Cyclic}
\alias{date<-}
\alias{as.Date.Cyclic}
\alias{date.Cyclic}
\alias{as.Date.PeriodicTimeSeries}
\title{Create objects from class Cyclic}
\description{Create objects from class Cyclic.}
\usage{
Cyclic(cycle, start = NULL, \dots)

\method{as.Date}{Cyclic}(x, \dots)

\method{date}{Cyclic}(x)

\method{as.Date}{PeriodicTimeSeries}(x, \dots)
}
\arguments{
  \item{cycle}{
    
    a cycle object, a positive integer giving the number of seasons, or
    any other object that can be used to create a cycle with
    \code{pcCycle(x, ...)}.
    
  }
  \item{start}{

    a cycle-season pair, a datetime object, a Date object or any object
    that can be converted to datetime with \code{as_datetime(start)}.

  }
  \item{\dots}{

    for \code{Cyclic}, arguments passed to \code{pcCycle}, used only if
    \code{cycle} is not from a cycle class. 

  }
  \item{x}{a Cyclic object}
  %\item{value}{the new value}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  for \code{Cyclic}, an object from class \code{"Cyclic"}
  
}
%\references{
%%% ~put references to the literature/web site here ~
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
  \code{\link{BuiltinCycle}}, \code{\link{pcCycle}} for creation of cycle objects,

  \code{\link{pcts}} importing and creating periodic time series

}
\examples{
## bare bone Cyclic starting at Cycle 1, season 1
Cyclic(4)    
Cyclic(4, c(1,1)) # same

## with quarter/year cycle
qu <- Cyclic(BuiltinCycle(4), start = c(2020, 1))
start(qu)
as_datetime(qu)

date(qu) <- c(2009, 2)
qu

ap <- pcts(AirPassengers)
as.Date(ap)
}
%\keyword{ }
% use one of  RShowDoc("KEYWORDS")
