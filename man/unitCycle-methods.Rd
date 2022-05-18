\name{unitCycle-methods}
\docType{methods}
\alias{unitCycle-methods}
\alias{unitCycle,ANY-method}
\alias{unitCycle,VirtualPeriodicModel-method}
\alias{unitCycle,Cyclic-method}

\alias{unitCycle,SimpleCycle-method}
\alias{unitCycle,PartialCycle-method}

\alias{unitCycle,OpenCloseCycle-method}
\alias{unitCycle,QuarterYearCycle-method}
%\alias{unitCycle,FiveDayWeekCycle-method}
\alias{unitCycle,DayWeekCycle-method}
\alias{unitCycle,MonthYearCycle-method}
\alias{unitCycle,Every30MinutesCycle-method}


\alias{unitSeason-methods}
\alias{unitSeason,ANY-method}
\alias{unitSeason,VirtualPeriodicModel-method}
\alias{unitSeason,Cyclic-method}

\alias{unitSeason,SimpleCycle-method}
\alias{unitSeason,PartialCycle-method}

\alias{unitSeason,OpenCloseCycle-method}
\alias{unitSeason,QuarterYearCycle-method}
%\alias{unitSeason,FiveDayWeekCycle-method}
\alias{unitSeason,DayWeekCycle-method}
\alias{unitSeason,MonthYearCycle-method}
\alias{unitSeason,Every30MinutesCycle-method}


\title{Methods for \code{unitCycle} and \code{unitSeason} in package pcts}
\description{Methods for \code{unitCycle} and \code{unitSeason} in package pcts.}
\section{Methods}{
  \code{unitCycle} and \code{unitSeason} have methods with identical
  signatures:

  \describe{
    \item{\code{signature(x = "ANY")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "VirtualPeriodicModel")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "Cyclic")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "SimpleCycle")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "PartialCycle")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "OpenCloseCycle")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "QuarterYearCycle")}}{
      %%  ~~describe this method here~~
    }
    
    %\item{\code{signature(x = "FiveDayWeekCycle")}}{
    %  %%  ~~describe this method here~~
    %}
    
    \item{\code{signature(x = "DayWeekCycle")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "MonthYearCycle")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "Every30MinutesCycle")}}{
      %%  ~~describe this method here~~
    }

  }
}
\seealso{
  \code{\link{allSeasons}} for examples and related functions
}
\examples{
## presidents is a quarterly time series in base-R
tsp(presidents)

pc_presidents <- pcts(presidents)
unitCycle(pc_presidents)
unitSeason(pc_presidents)
}
\keyword{methods}
%\keyword{internal}
