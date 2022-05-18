\name{nSeasons-methods}
\docType{methods}
\alias{nSeasons-methods}
%\alias{nSeasons,slMatrix-method}
\alias{nSeasons,Cyclic-method}

\alias{nSeasons,BareCycle-method}
%\alias{nSeasons,BuiltinCycle-method}
\alias{nSeasons,PartialCycle-method}
%\alias{nSeasons,OpenCloseCycle-method}
\alias{nSeasons,DayWeekCycle-method}
\alias{nSeasons,MonthYearCycle-method}
\alias{nSeasons,QuarterYearCycle-method}
\alias{nSeasons,OpenCloseCycle-method}
\alias{nSeasons,Every30MinutesCycle-method}

\alias{nSeasons,VirtualPeriodicArmaFilter-method}
\alias{nSeasons,VirtualArmaFilter-method}
\alias{nSeasons,PeriodicMonicFilterSpec-method}
\alias{nSeasons,SarimaFilter-method}

\alias{nSeasons,PeriodicInterceptSpec-method}
\alias{nSeasons,PeriodicIntegratedArmaSpec-method}
\alias{nSeasons,PeriodicFilterModel-method}
\alias{nSeasons,VirtualPeriodicModel-method}

\title{Number of seasons of a periodic object}
\description{Number of seasons of a periodic object.}
\usage{
\S4method{nSeasons}{Cyclic}(object)

## same signature for all periodic classes in package "pcts"
}
\arguments{
  \item{object}{
    an object for which the notion of number of seasons makes sense.
  }
}
\details{

  \code{nSeasons} is a generic function. This page gives is for the
  methods defined in package \code{"pcts"} - all periodic classes have
  (or inherit) a method.

}
\value{
  an integer number
}
\author{Georgi N. Boshnakov}
\section{Methods}{
\describe{
\item{\code{signature(object = "DayWeekCycle")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "MonthYearCycle")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "PeriodicIntegratedArmaSpec")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "QuarterYearCycle")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "PeriodicMonicFilterSpec")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "PeriodicInterceptSpec")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "Cyclic")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "BareCycle")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "OpenCloseCycle")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "Every30MinutesCycle")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "PartialCycle")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "VirtualPeriodicModel")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "SarimaFilter")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "VirtualArmaFilter")}}{{
%%  ~~describe this method here~~
}
}}}
\seealso{
  \code{\link{allSeasons}} for other functions related to the
  seasonality of an object;

  \code{\link{nCycles}} for related functions
}
\examples{
## scalar time series
ap <- pcts(AirPassengers)
nSeasons(ap) # 12

## multivariate time series
pcfr <- pcts(dataFranses1996)
nSeasons(pcfr) # 4

## five-day-week period
five_day_week <- BuiltinCycle(5)
five_day_week
nSeasons(five_day_week)
}
\keyword{methods}
\keyword{pcts}
