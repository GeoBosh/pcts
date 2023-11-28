\name{BareCycle-class}
\Rdversion{1.1}
\docType{class}
\alias{BareCycle-class}

\title{Class BareCycle}
\description{Class BareCycle.}
\section{Objects from the Class}{
  
  Objects can be created by calls of the form \code{pcCycle(nseasons)}
  or \code{new("BareCycle", nseasons)}.

  Class \code{"BareCycle"} represents the number of seasons and is
  sufficient for many computations.
}
\section{Slots}{
  \describe{
    \item{\code{nseasons}:}{Object of class \code{"integer"}, the number
    of seasons.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{BasicCycle}"}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "BareCycle")}: ... }
    \item{coerce}{\code{signature(from = "BareCycle", to = "SimpleCycle")}: ... }
    \item{coerce}{\code{signature(from = "BuiltinCycle", to = "BareCycle")}: ... }
    \item{nSeasons}{\code{signature(object = "BareCycle")}: ... }
    \item{show}{\code{signature(object = "BareCycle")}: ... }
  
    % \item{.get_origin}{\code{signature(cycle = "BareCycle")}: ... }
    % \item{.nperiods}{\code{signature(cycle = "BareCycle")}: ... }
  }
}
%\references{
%%%  ~~put references to the literature/web site here~~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{pcCycle}} for creation of cycle objects and extraction of
  cycle part of time series,
  
  \code{\link{BuiltinCycle-class}},
  \code{\link{SimpleCycle-class}},

  \code{\link{DayWeekCycle-class}},
  \code{\link{MonthYearCycle-class}},
  \code{\link{OpenCloseCycle-class}},
  \code{\link{QuarterYearCycle-class}}
  \code{\link{PartialCycle-class}},

  \code{\link{BasicCycle-class}} (virtual, for use in signatures)
%%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
pcCycle(5)
cycle <- new("BareCycle", 5)
identical(new("BareCycle", 5), pcCycle(5)) # TRUE

unitSeason(cycle)
unitCycle(cycle)
allSeasons(cycle)
seqSeasons(cycle)

cycle[]
cycle[3]

## if cycle represents 5-days week one may prefer:
BuiltinCycle(5)
}
\keyword{classes}
