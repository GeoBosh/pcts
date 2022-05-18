\name{BuiltinCycle-class}
\Rdversion{1.1}
\docType{class}
\alias{BuiltinCycle-class}
\alias{allSeasons,BuiltinCycle-method}
\alias{initialize,BuiltinCycle-method}
%\alias{nSeasons,BuiltinCycle-method}
\alias{unitCycle,BuiltinCycle-method}
\alias{unitSeason,BuiltinCycle-method}

\alias{OpenCloseCycle-class}
\alias{QuarterYearCycle-class}
\alias{DayWeekCycle-class}
\alias{MonthYearCycle-class}
\alias{Every30MinutesCycle-class}

\title{Class \code{"BuiltinCycle"} and its subclasses in package 'pcts'}
\description{Class \code{"BuiltinCycle"} and its subclasses in package 'pcts'.}
\section{Objects from the Class}{

  Class \code{"BuiltinCycle"} is a virtual Class: no objects may be
  created from it. Class \code{"BuiltinCycle"} has several built-in
  cycle subclasses. Objects from the subclasses can be created by calls
  of the form \code{new("className", first, ...)}, where
  \code{"className"} is the name of the subclass. The optional argument
  \code{first} can be used to designate a season to be considered first
  in the cycle, by default the first.

  The function \code{\link{BuiltinCycle}} provides a more convenient way
  to generate objects from subclasses of class
  \code{"BuiltinCycle"}. Its argument is the number of seasons.

  These classes are effectively unmodifiable, but the user can convert
  them to other cycle classes, e.g. class "SimpleCycle", and adapt as
  needed.

  The subclasses of \code{"BuiltinCycle"} have definitions for all
  methods promised by its superclass \code{"BasicCycle"}.

  Currently, the following builtin classes are defined:
  \tabular{llr}{
    Name                         \tab Description                  \tab nSeasons \cr
    \code{"DayWeekCycle"}        \tab weekdays                     \tab        7 \cr
    \code{"QuarterYearCycle"}    \tab quarters in a year           \tab        4 \cr
    \code{"MonthYearCycle"}      \tab months in a year             \tab       12 \cr
    \code{"Every30MinutesCycle"} \tab half-hour intervals in a day \tab       48 \cr
    \code{"OpenCloseCycle"}      \tab start/end of a working day   \tab        2
  }		     

  There is also a class \code{"FiveDayWeekCycle"} but it is deprecated
  and will be removed in the near future. Use \code{\link{BuiltinCycle}(5)}
  to create objects with equivalent functionality.

}
\section{Slots}{
  The class \code{"BuiltinCycle"} and its subclasses have a single
  common slot:
  \describe{

    \item{\code{first}:}{Object of class \code{"integer"},
      the index of the season to be treated as the first in a cycle.
    }

}}
\section{Extends}{
  Class \code{"BuiltinCycle"} extends class
  \code{"\linkS4class{BasicCycle}"}, directly.

  Classes \code{"DayWeekCycle"}, \code{"Every30MinutesCycle"},
  \code{"FiveDayWeekCycle"}, \code{"OpenCloseCycle"} and
  \code{"QuarterYearCycle"} 
  extend:
  
  Class \code{"\linkS4class{BuiltinCycle}"}, directly.
  Class \code{"\linkS4class{BasicCycle}"}, by class "BuiltinCycle", distance 2.

}
\section{Methods}{Functions with methods for this class:
  \describe{
    \item{coerce}{\code{signature(from = "BuiltinCycle", to = "BareCycle")}: ... }
    \item{coerce}{\code{signature(from = "BuiltinCycle", to = "SimpleCycle")}: ... }
    \item{initialize}{\code{signature(.Object = "BuiltinCycle")}: ... }
    \item{show}{\code{signature(object = "BuiltinCycle")}: ... }
  }

  The functions to extract properties from objects from the builtin
  cycle classes have identical signatures (except for the name of the
  class). For example, for \code{"QuarterYearCycle"} the methods are as
  follows:

  \describe{
    \item{allSeasons}{\code{signature(x = "QuarterYearCycle", abb = "logical")}: ... }
    \item{allSeasons}{\code{signature(x = "QuarterYearCycle", abb = "missing")}: ... }
    \item{nSeasons}{\code{signature(object = "QuarterYearCycle")}: ... }
    \item{unitCycle}{\code{signature(x = "QuarterYearCycle")}: ... }
    \item{unitSeason}{\code{signature(x = "QuarterYearCycle")}: ... }
  }

  The methods for the remaining builtin classes are the same with
  \code{"QuarterYearCycle"} replaced suitably.
}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\link{BuiltinCycle}},
  \code{\link{pcCycle}} for creation of cycle objects and extraction of
  cycle part of time series,

  class \code{\linkS4class{PartialCycle}} for creating variants of the
  builtin classes, e.g., 5-day weeks.

  \code{\link{BareCycle-class}},
  \code{\link{SimpleCycle-class}},

  %%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
## class "DayWeekCycle"
dwcycle <- BuiltinCycle(7) # new("DayWeekCycle")

unitSeason(dwcycle)
unitCycle(dwcycle)

allSeasons(dwcycle)
dwcycle[] # same

allSeasons(dwcycle, abb = TRUE)
dwcycle[ , abb = TRUE] # same

dwcycle[2]
dwcycle[2, abb = TRUE]

seqSeasons(dwcycle)

## start the week on Sunday
dws <- BuiltinCycle(7, first = 7) # new("DayWeekCycle", first = 7)
dws[1] # "Sunday"
allSeasons(dws)

## class "Every30MinutesCycle"
cyc48 <- BuiltinCycle(48) # new("Every30MinutesCycle")
nSeasons(cyc48)
allSeasons(cyc48)

## class "FiveDayWeekCycle" is deprecated, use the equivalent:
fdcycle <- BuiltinCycle(5)

unitSeason(fdcycle)
unitCycle(fdcycle)

allSeasons(fdcycle)
fdcycle[] # same

allSeasons(fdcycle, abb = TRUE)
fdcycle[ , abb = TRUE] # same

fdcycle[2]
fdcycle[2, abb = TRUE]

seqSeasons(fdcycle)

## class "MonthYearCycle"
mycycle <- BuiltinCycle(12) # new("MonthYearCycle")

unitSeason(mycycle)
unitCycle(mycycle)

allSeasons(mycycle)
mycycle[ ] # same

allSeasons(mycycle, , abb = TRUE)
mycycle[ , abb = TRUE] # same

mycycle[2]
mycycle[2, abb = TRUE]

seqSeasons(mycycle)

## class "OpenCloseCycle"
opcycle <- new("OpenCloseCycle")

unitSeason(opcycle)
unitCycle(opcycle)

allSeasons(opcycle)
opcycle[ , abb = FALSE] # same

allSeasons(opcycle, abb = FALSE)
opcycle[] # same

opcycle[2]
opcycle[2, abb = TRUE]

seqSeasons(opcycle)

## class "QuarterYearCycle"
qycycle <- new("QuarterYearCycle")

unitSeason(qycycle)
unitCycle(qycycle)

allSeasons(qycycle)
qycycle[] # same

allSeasons(qycycle, abb = TRUE)
qycycle[ , abb = TRUE] # same

qycycle[2]
qycycle[2, abb = TRUE]

seqSeasons(qycycle)
}
\keyword{classes}
