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

A common source of frustration is the accidental use of \code{as.Date}
or \code{as_date}, instead of \code{as.POSIXlt} or \code{as_datetime}.
These four are often equivalent, most notably for monthly, quarterly and
daily observations but, in general, conversion to dates drops the
fractional day part of a datetime.

}
\value{
  for \code{Pctime}, an object from S3 class Pctime
  
}
%\references{
%%% ~put references to the literature/web site here ~
%}
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
pctQ <- Pctime(c(2020, 2), BuiltinCycle(4))
Pctime("2020-04-01", BuiltinCycle(4)) # same
pctQ

apdates <- as_datetime(pcts(AirPassengers))
pct <- Pctime(apdates, BuiltinCycle(4))
head(pct)
tail(pct)
}
\keyword{pcts}
\keyword{chron}
% use one of  RShowDoc("KEYWORDS")
