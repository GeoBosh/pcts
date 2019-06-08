\name{BuiltinCycle}
\alias{BuiltinCycle}
\title{Create cycle objects from the builtin cycle classes}
\description{Create cycle objects from the builtin cycle classes.}
\usage{
BuiltinCycle(n, coerce = FALSE, first = 1)
}
\arguments{
  \item{n}{number of seasons, an integer.}
  \item{coerce}{
    if \code{TRUE} coerce the objects to a modifiable cycle class,
    currently \code{"SimpleCycle"}.
  }
  \item{first}{which season is first for this object.}
}
\details{

  \code{BuiltinCycle} is a convenience function to create objects from
  builtin cycle classes by specifying the number of seasons.  The
  builtin cycle classes are esseintially fixed, except that which season
  is considered first can be changed using argument \code{first}. If
  other modifications are desired, convert the returned builtin cycle
  object to class \code{"SimpleCycle"}. This can be done also in the
  call to \code{BuiltinCycle()} by specifying \code{coerce = TRUE}.

}
\value{
  one of the builtin classes, coerced if requested.
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
%
\seealso{
  class \code{\linkS4class{BuiltinCycle}} for the available builtin classes,
  \code{\link{allSeasons}} for further examples,
}
\examples{
BuiltinCycle(2)  # "OpenCloseCycle"
BuiltinCycle(4)  # "QuarterYearCycle"
BuiltinCycle(5)  # "FiveDayWeekCycle"
BuiltinCycle(7)  # "DayWeekCycle"
BuiltinCycle(12) # "MonthYearCycle"
BuiltinCycle(48) # "Every30MinutesCycle"
}
%\keyword{ ~kwd1 }
% use one of  RShowDoc("KEYWORDS")
