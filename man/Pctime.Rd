\name{Pctime}
\alias{Pctime}
\alias{as_Pctime.Cyclic}
\alias{as_Pctime.PeriodicTimeSeries}
\alias{[.Pctime}
\alias{[[.Pctime}
\alias{[.Date}
\alias{[[.Date}
\alias{[.ts}
\alias{[<-.POSIXlt}
\alias{as_Pctime}
\title{Pctime objects}
\description{

  Class \code{"Pctime"} is an S3 class inheriting from the base R
  datetime class \code{"POSIXct"}. It has methods for conversion between
  datetimes and the pcts cycle-season pairs, as well as convenience
  methods for a few other functions.
  
}
\usage{
Pctime(x, cycle, \dots)

as_Pctime(x, \dots)

\method{[}{Pctime}(x, i, j, drop = TRUE)

\method{[[}{Pctime}(x, \dots, drop = TRUE)

\method{as_Pctime}{Cyclic}(x, \dots)

\method{as_Pctime}{PeriodicTimeSeries}(x, \dots)
}
\arguments{
  \item{x}{

    for \code{Pctime}, numeric vector, matrix with two columns, or any
    object that is or can be converted to datetime. For the other
    functions see Details.

  }
  \item{cycle}{
    
    a positive integer, cycle object, or missing. 

  }
  \item{i}{subscript}
  \item{j}{noi used}
  \item{drop}{not used}
  % \item{value}{a replacement value for the selected subset.}
  \item{\dots}{further argument for methods.}
}
\details{

\code{Pctime} represents periodic times with cycle specification
contained in attribute \code{"cycle"}. It is basically datetime
(inheriting from \code{"POSIXct"}) with additional attribute(s).

For printing Pctime objects are shown as cycle-season pairs.  To print
in other formats, just convert them using \code{as_datetime} or other
suitable function. Note though that some cycles in pcts do not have
natural datetime representation. For them, Pctime sets it arbitrarilly
as the number of seconds from a origin.

The seasons in cycle-season pairs are numbered from one to the number
of seasons. Names and abbreviations are used when available and this is
the case for all builtin cycles and partial cycles obtained from them. 

The cycles in cycle-season pairs are numbered from a starting point.
For years, it is what is expected. For cycles representing weeks, week 1
is the first ISO week of 1970, so \code{c(1,1)} corresponds to
1969-12-29. For some other cycle classes \code{c(1,1)} also corresponds
to the first time in the first ISO week of 1970.

Subsetting with \code{"["} keeps the \code{Pctime} class, while
\code{"[["} returns a datetime object. Other standard functions work
with Pctime objects, as well, including \code{seq}.

A common source of frustration is the accidental use of \code{as.Date}
or \code{as_date}, instead of \code{as.POSIXlt} or \code{as_datetime}.
These four are often equivalent, most notably for monthly, quarterly and
daily observations but, in general, conversion to dates drops the
fractional day part of a datetime.

The default time zone is UTC. Other time zones can be used since the
calculations use standard datetime and date functions from base R and
package lubridate \insertCite{lubridate}{pcts}, but currently this has
not been tested.

}
\value{
  for \code{Pctime}, an object from S3 class Pctime
  
}
\references{
   \insertAllCited{}
}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
## a bare bone date for four seasons
pct4 <- Pctime(c(2020, 2), pcCycle(4))
pct4

## quarterly cycle
Pctime("2020-04-01", BuiltinCycle(4))
pctQ <- Pctime(c(2020, 2), BuiltinCycle(4))  # same
pctQ

## day-in-week cycle
## c(1, 1) is the start of the first ISO week of 1970
weekW1S1 <- Pctime(c(1, 1), BuiltinCycle(7))  # W1 Mon
weekW1S1
as_datetime(weekW1S1)

Pctime("1970-01-01", BuiltinCycle(7)) #  W1 Thu
pctW1Th <- Pctime(c(1, 4), BuiltinCycle(7))  # same
pctW1Th

Pctime("2020-04-01", BuiltinCycle(7))
pctW2623Wed <- Pctime(c(2623, 3), BuiltinCycle(7))  # same
pctW2623Wed
as_datetime(pctW2623Wed)

## Monday-Friday week - a partial cycle derived from DayOfWeekCycle
BuiltinCycle(5)
pctMF <- Pctime("2020-04-03", BuiltinCycle(5)) # Fri
seq(pctMF, length.out = 10) # note: Sat, Sun are skipped

Pctime("2020-04-04", BuiltinCycle(5)) # Sat, not in the cycle

## monthly cycle
Pctime("2020-04-01", BuiltinCycle(12))
pctY2020Apr <- Pctime(c(50, 4), BuiltinCycle(12))  # same
pctY2020Apr
as_datetime(pctW2623Wed)

## Pctime can hold a vector of times
ap <- pcts(AirPassengers)
aptime <- Pctime(ap)              #  as_Pctime(ap)
aptime[1:12] # keep Pctime class
aptime[1]

aptime[[1]] # drop Pctime class

head(aptime)
tail(aptime)

apdates <- as_datetime(ap)
head(apdates)
tail(apdates)
}
\keyword{pcts}
\keyword{chron}
% use one of  RShowDoc("KEYWORDS")
