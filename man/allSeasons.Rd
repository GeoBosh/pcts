\name{allSeasons}
\docType{methods}
\alias{allSeasons}
\alias{allSeasons-methods}
\alias{allSeasons,BasicCycle,ANY-method}
\alias{allSeasons,DayWeekCycle,logical-method}
\alias{allSeasons,DayWeekCycle,missing-method}
\alias{allSeasons,FiveDayWeekCycle,logical-method}
\alias{allSeasons,FiveDayWeekCycle,missing-method}
\alias{allSeasons,MonthYearCycle,logical-method}
\alias{allSeasons,MonthYearCycle,missing-method}
\alias{allSeasons,OpenCloseCycle,logical-method}
\alias{allSeasons,OpenCloseCycle,missing-method}
\alias{allSeasons,QuarterYearCycle,logical-method}
\alias{allSeasons,QuarterYearCycle,missing-method}
\alias{allSeasons,SimpleCycle,ANY-method}
\alias{allSeasons,Cyclic,ANY-method}
\alias{allSeasons,Every30MinutesCycle,logical-method}
\alias{allSeasons,Every30MinutesCycle,missing-method}
\alias{allSeasons,VirtualPeriodicModel,ANY-method}

\alias{allSeasons<-}
\alias{allSeasons<-,Cyclic-method}
\alias{allSeasons<-,SimpleCycle-method}

\alias{unitSeason}
\alias{unitCycle}
\alias{seqSeasons}
\alias{unitSeason<-}
\alias{unitCycle<-}

\title{Get names of seasons}
\description{

  Functions and methods fo get names of seasons and related quantities
  for objects from the cycle, periodic time series classes and other
  objects for which the concepts are defined.

}
\usage{
unitSeason(x)
unitCycle(x)
seqSeasons(x)
allSeasons(x, abb = FALSE, prefix = "S", \dots)

unitSeason ( x, ... ) <- value
unitCycle ( x, ... ) <- value
allSeasons ( x, abb, ... ) <- value

\S4method{allSeasons}{SimpleCycle}(x, abb, prefix, \dots) <- value

\S4method{allSeasons}{Cyclic}(x, abb = FALSE, \dots) <- value
}
\arguments{
  \item{x}{
    a cycle, time series or other object for which the concept of
    seasons is defined.
  }
  \item{abb}{if \code{TRUE} give the abbreviated names of the seasons.}
  \item{prefix}{
    use this prefix for automatically generated names of seasons.
  }
  \item{\dots}{further arguments for methods.}
  \item{value}{a character string}
}
\details{

  The cycle classes, i.e. classes inheriting from class
  \code{\linkS4class{BasicCycle}}, provide common functionality. In
  particular, they guarantee that the functions described in this topic
  are available. These functions work also for the periodic time series
  classes and may be defined for other classes where they make sense.
  
}
%\value{
%%%  ~Describe the value returned
%%%  If it is a LIST, use
%%%  \item{comp1 }{Description of 'comp1'}
%%%  \item{comp2 }{Description of 'comp2'}
%%% ...
%}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\section{Methods}{
  Methods for \code{allSeasons()}:
\describe{

\item{\code{signature(x = "BasicCycle", abb = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "DayWeekCycle", abb = "logical")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "DayWeekCycle", abb = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "FiveDayWeekCycle", abb = "logical")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "FiveDayWeekCycle", abb = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "MonthYearCycle", abb = "logical")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "MonthYearCycle", abb = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "OpenCloseCycle", abb = "logical")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "OpenCloseCycle", abb = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "QuarterYearCycle", abb = "logical")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "QuarterYearCycle", abb = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "SimpleCycle", abb = "ANY")}}{
%%  ~~describe this method here~~
}
\item{\code{signature(x = "Cyclic", abb = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Every30MinutesCycle", abb = "logical")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Every30MinutesCycle", abb = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "VirtualPeriodicModel", abb = "ANY")}}{{
%%  ~~describe this method here~~
}
}}}
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%% use one of  RShowDoc("KEYWORDS")
\examples{
opcycle <- new("OpenCloseCycle")
## convert to SimpleCycle to change some names
siopcycle <- as(opcycle, "SimpleCycle")
## siopcycle inherits names from opcycle
unitSeason(siopcycle)             # "Season"
unitCycle(siopcycle)              # "Cycle"
allSeasons(siopcycle)             # "Open"  "Close"
allSeasons(siopcycle, abb = TRUE) # "O" "C"

allSeasons(siopcycle) <- c("Day", "Night")
allSeasons(siopcycle) # now: "Day"   "Night"
## change also abbreviations
allSeasons(siopcycle, abb = TRUE) <- c("D", "N")
allSeasons(siopcycle, abb = TRUE) # now: "D" "N"

seasons <- new("SimpleCycle", 4)
unitSeason(seasons)             # "Season"
unitCycle(seasons)              # "Cycle"
allSeasons(seasons)
allSeasons(seasons, abb = TRUE) 

unitCycle(seasons) <- "Year"
unitCycle(seasons)
allSeasons(seasons) <- c("Winter", "Spring", "Summer", "Autumn")
allSeasons(seasons)
allSeasons(seasons, abb = TRUE) <- c("Win", "Spr", "Sum", "Aut")
allSeasons(seasons, abb = TRUE)

## change autumn to Fall
allSeasons(seasons)[4] <- "Fall"
allSeasons(seasons, abb = TRUE)[4] <- "Fal"
allSeasons(seasons)
allSeasons(seasons, abb = TRUE)

## indexing of cycle objects is equivalent to allSeasons.
seasons[]
seasons[ , abb = TRUE]

seasons[4] <- "Herbst"
seasons

unitCycle(seasons) <- "Jahre"
unitCycle(seasons)
unitSeason(seasons) <- "Jahreszeit"
seasons[] <- c("Winter", "Frueling", "Sommer", "Herbst")
seasons[ , abb = TRUE] <- c("W", "F", "S", "H")
seasons[]
seasons
}
\keyword{pcts}
\keyword{methods}
