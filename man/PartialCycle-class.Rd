\name{PartialCycle-class}
\Rdversion{1.1}
\docType{class}
\alias{PartialCycle-class}
\alias{allSeasons,PartialCycle,logical-method}
\alias{allSeasons,PartialCycle,missing-method}
%\alias{nSeasons,PartialCycle-method}
%\alias{unitCycle,PartialCycle-method}
%\alias{unitSeason,PartialCycle-method}

\title{Class PartialCycle}
\description{Class PartialCycle}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PartialCycle", ...)}.

Partial cycles are often created implicitly when subsetting time series
using \code{window()} with argument \code{seasons}, see the examples.

}
\section{Slots}{
  \describe{
    \item{\code{orig}:}{
      the parent class of the partial cycle, an object inheriting from
      class \code{"BasicCycle"}.
    }
    \item{\code{subindex}:}{
      an integer vector specifying the seasons to include in the partial
      cycle.
    }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BasicCycle}"}, directly.
}
\section{Methods}{
  \describe{
    \item{allSeasons}{\code{signature(x = "PartialCycle", abb = "logical")}: ... }
    \item{allSeasons}{\code{signature(x = "PartialCycle", abb = "missing")}: ... }
    \item{nSeasons}{\code{signature(object = "PartialCycle")}: ... }
    \item{unitCycle}{\code{signature(x = "PartialCycle")}: ... }
    \item{unitSeason}{\code{signature(x = "PartialCycle")}: ... }
    \item{show}{\code{signature(object = "PartialCycle")}: ... }}
  
    % \item{.cycle_and_pair2time}{\code{signature(cycle = "PartialCycle")}: ... }
    % \item{.cycle_and_time2pair}{\code{signature(cycle = "PartialCycle")}: ... }
    % \item{.get_offset}{\code{signature(cycle = "PartialCycle")}: ... }
    % \item{.get_origin}{\code{signature(cycle = "PartialCycle")}: ... }
    % \item{.get_period_length}{\code{signature(cycle = "PartialCycle")}: ... }
    % \item{.get_period_units}{\code{signature(cycle = "PartialCycle")}: ... }
}
% \references{
% %%  ~~put references to the literature/web site here~~
% }
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }
% 
% %% ~Make other sections like Warning with \section{Warning }{....} ~
% 
\seealso{
  \code{\link{BuiltinCycle}}
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
dwc <- new("DayWeekCycle")
dwc
allSeasons(dwc)

## a five day week cycle
dwc5 <- new("PartialCycle", orig = dwc, subindex = 1:5)
dwc5
allSeasons(dwc5)

weekend <- new("PartialCycle", orig = dwc, subindex = 6:7)
weekend
allSeasons(weekend)

ap <- pcts(AirPassengers)

## take data for the summer months (in Northern hemisphere)
ap7to9 <- window(ap, seasons = 7:9)
## the above implicitly creates a partial cycle
ap7to9
allSeasons(ap7to9)
}
\keyword{classes}

