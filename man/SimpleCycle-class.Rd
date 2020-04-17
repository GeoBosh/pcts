\name{SimpleCycle-class}
\Rdversion{1.1}
\docType{class}
\alias{SimpleCycle-class}

\title{Class SimpleCycle}
\description{Class SimpleCycle.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("SimpleCycle",
  nseasons, seasons, first)}.

  %%  ~~ describe objects here ~~

  In addition to number of seasons, class \code{"SimpleCycle"} holds
  also seasons' names and the index of the season to be treated as the
  first in a cycle.

}
\section{Slots}{
  \describe{
    \item{\code{seasons}:}{Object of class \code{"character"},
      the names of the seasons.
    }
    \item{\code{nseasons}:}{Object of class \code{"integer"},
      number of seasons.
    }

    \item{\code{cycle}:}{Object of class \code{"character"} ~~ }
    \item{\code{season}:}{Object of class \code{"character"} ~~ }
    \item{\code{abbreviated}:}{Object of class \code{"character"} ~~ }
}
}
\section{Extends}{
Class \code{"\linkS4class{BareCycle}"}, directly.
Class \code{"\linkS4class{BasicCycle}"}, directly.
}
\section{Methods}{
No methods defined with class "SimpleCycle" in the signature.
}
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
  \code{\link{QuarterYearCycle-class}},
  \code{\link{PartialCycle-class}}

  \code{\link{BasicCycle-class}} (virtual, for use in signatures)
%%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("SimpleCycle")
}
\keyword{classes}
