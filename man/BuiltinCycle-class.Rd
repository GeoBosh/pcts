\name{BuiltinCycle-class}
\Rdversion{1.1}
\docType{class}
\alias{BuiltinCycle-class}
\alias{allSeasons,BuiltinCycle-method}
\alias{initialize,BuiltinCycle-method}
%\alias{nSeasons,BuiltinCycle-method}
\alias{unitCycle,BuiltinCycle-method}
\alias{unitSeason,BuiltinCycle-method}

\alias{DayWeekCycle-class}
%\alias{nSeasons,DayWeekCycle-method}

\alias{Every30MinutesCycle-class}
%\alias{allSeasons,Every30MinutesCycle,logical-method}
%\alias{allSeasons,Every30MinutesCycle,missing-method}
%\alias{nSeasons,Every30MinutesCycle-method}
%\alias{unitCycle,Every30MinutesCycle-method}
%\alias{unitSeason,Every30MinutesCycle-method}

\alias{FiveDayWeekCycle-class}
%\alias{nSeasons,FiveDayWeekCycle-method}

\alias{MonthYearCycle-class}

\alias{OpenCloseCycle-class}
%\alias{nSeasons,OpenCloseCycle-method}

\alias{QuarterYearCycle-class}
%\alias{nSeasons,QuarterYearCycle-method}


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

    %  Class \code{"DayWeekCycle"} 
    \item{allSeasons}{\code{signature(x = "DayWeekCycle", abb = "logical")} }
    \item{allSeasons}{\code{signature(x = "DayWeekCycle", abb = "missing")} }
    \item{nSeasons}{\code{signature(object = "DayWeekCycle")} }
    \item{unitCycle}{\code{signature(x = "DayWeekCycle")} }
    \item{unitSeason}{\code{signature(x = "DayWeekCycle")} }

    % Class \code{"Every30MinutesCycle"}
    \item{allSeasons}{\code{signature(x = "Every30MinutesCycle", abb = "logical")}: ... }
    \item{allSeasons}{\code{signature(x = "Every30MinutesCycle", abb = "missing")}: ... }
    \item{nSeasons}{\code{signature(object = "Every30MinutesCycle")}: ... }
    \item{unitCycle}{\code{signature(x = "Every30MinutesCycle")}: ... }
    \item{unitSeason}{\code{signature(x = "Every30MinutesCycle")}: ... }
    
    % \code{"FiveDayWeekCycle"}
    \item{allSeasons}{\code{signature(x = "FiveDayWeekCycle", abb = "logical")}: ... }
    \item{allSeasons}{\code{signature(x = "FiveDayWeekCycle", abb = "missing")}: ... }
    \item{nSeasons}{\code{signature(object = "FiveDayWeekCycle")}: ... }
    \item{unitCycle}{\code{signature(x = "FiveDayWeekCycle")}: ... }
    \item{unitSeason}{\code{signature(x = "FiveDayWeekCycle")}: ... }

    % \code{MonthYearCycle}
    \item{allSeasons}{\code{signature(x = "MonthYearCycle", abb = "logical")}: ... }
    \item{allSeasons}{\code{signature(x = "MonthYearCycle", abb = "missing")}: ... }
    \item{nSeasons}{\code{signature(object = "MonthYearCycle")}: ... }
    \item{unitCycle}{\code{signature(x = "MonthYearCycle")}: ... }
    \item{unitSeason}{\code{signature(x = "MonthYearCycle")}: ... }
    
    % \code{"OpenCloseCycle"}
    \item{allSeasons}{\code{signature(x = "OpenCloseCycle", abb = "logical")}: ... }
    \item{allSeasons}{\code{signature(x = "OpenCloseCycle", abb = "missing")}: ... }
    \item{nSeasons}{\code{signature(object = "OpenCloseCycle")}: ... }
    \item{unitCycle}{\code{signature(x = "OpenCloseCycle")}: ... }
    \item{unitSeason}{\code{signature(x = "OpenCloseCycle")}: ... }

    % \code{"QuarterYearCycle"}
    \item{allSeasons}{\code{signature(x = "QuarterYearCycle", abb = "logical")}: ... }
    \item{allSeasons}{\code{signature(x = "QuarterYearCycle", abb = "missing")}: ... }
    \item{nSeasons}{\code{signature(object = "QuarterYearCycle")}: ... }
    \item{unitCycle}{\code{signature(x = "QuarterYearCycle")}: ... }
    \item{unitSeason}{\code{signature(x = "QuarterYearCycle")}: ... }
  }
}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\link{BuiltinCycle}},
  \code{\link{pcCycle}} for creation of cycle objects and extraction of
  cycle part of time series,
  % 
  % \code{\link{DayWeekCycle-class}},
  % \code{\link{FiveDayWeekCycle-class}},
  % \code{\link{MonthYearCycle-class}},
  % \code{\link{OpenCloseCycle-class}},
  % \code{\link{QuarterYearCycle-class}},
  % 
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
