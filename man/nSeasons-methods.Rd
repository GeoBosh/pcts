\name{nSeasons-methods}
\docType{methods}
%\alias{nSeasons}
\alias{nSeasons-methods}
\alias{nSeasons,BareCycle-method}
%\alias{nSeasons,CycleDescription-method}
\alias{nSeasons,Cyclic-method}
%\alias{nSeasons,pc.Model.WeaklyStat-method}
%\alias{nSeasons,PeriodicFilter-method}
%\alias{nSeasons,slMatrix-method}
%\alias{nSeasons,BuiltinCycle-method}
%\alias{nSeasons,DayWeekCycle-method}
%\alias{nSeasons,FiveDayWeekCycle-method}
%\alias{nSeasons,MonthYearCycle-method}
%\alias{nSeasons,OpenCloseCycle-method}
\alias{nSeasons,PeriodicFilterModel-method}
\alias{nSeasons,PeriodicIntegratedArmaSpec-method}
%\alias{nSeasons,QuarterYearCycle-method}
\alias{nSeasons,VirtualPeriodicArmaFilter-method}
\alias{nSeasons,DayWeekCycle-method}
\alias{nSeasons,FiveDayWeekCycle-method}
\alias{nSeasons,MonthYearCycle-method}
%\alias{nSeasons,OpenCloseCycle-method}
\alias{nSeasons,PeriodicMonicFilterSpec-method}
\alias{nSeasons,QuarterYearCycle-method}
\alias{nSeasons,PeriodicInterceptSpec-method}
\alias{nSeasons,OpenCloseCycle-method}
\alias{nSeasons,Every30MinutesCycle-method}
\alias{nSeasons,PartialCycle-method}
\alias{nSeasons,VirtualPeriodicModel-method}
\title{Number of seasons for a periodic object}
\description{Number of seasons for a periodic object.}
%\usage{
%nSeasons(object)
%}
\arguments{
  \item{object}{
    an object for which the notion of number of seasons makes sense.
  }
}
\details{

  \code{nSeasons} is a generic function. This page gives its methods for
  the periodic classes in package \code{"pcts"}.

}
\value{
  an integer number
}
\author{Georgi N. Boshnakov}
\section{Methods}{
%
%\item{\code{signature(x = "CycleDescription")}}{
%%%  ~~describe this method here~~
%}
%
%
%\item{\code{signature(x = "pc.Model.WeaklyStat")}}{
%%%  ~~describe this method here~~
%}
%
%\item{\code{signature(x = "PeriodicFilter")}}{
%%%  ~~describe this method here~~
%}
%
\describe{
\item{\code{signature(object = "DayWeekCycle")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "FiveDayWeekCycle")}}{{
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
}}}
\examples{
ap <- pcts(AirPassengers)
nSeasons(ap) # 12

pcfr <- pcts(dataFranses1996)
nSeasons(pcfr) # 4

}
\keyword{methods}
\keyword{pcts}
