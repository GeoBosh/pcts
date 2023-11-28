\name{nCycles}
\alias{nCycles}
\alias{nTicks}
\alias{nVariables}
\alias{nSeasons}

\title{
  Basic information about periodic ts objects
}
\description{

  Basic information about periodic periodic time series objects.

}
\usage{
nCycles(x, \dots)

nTicks(x)

nVariables(x, \dots)

nSeasons(object)
}
\arguments{
  \item{x,object}{an object from a periodic time series class.}
  \item{\dots}{further arguments for methods.}
}
\details{

  % These are generic functions. The default methods will work for any
  % objects for which \code{NROW} and \code{NCOL} are defined and have the
  % conventional case by variables interpretation.

  \code{nTicks} gives the number of time points, i.e. number of rows in
  the matrix representation.

  \code{nVariables} gives the number of variables in the time series.

  \code{nSeasons} gives the number of seasons of time series and other
  periodic objects.

  \code{nCycles} gives the number of cycles available in the data,
  e.g. number of years for monthly data. It always gives an integer
  number. Currently, if the result is not an integer an error is
  raised. \strong{TODO:} There is a case to round up or give the number of full
  cycles available but this seems somewhat dangerous if done quietly. A
  good alternative is to provide argument for control of this.

  There are further functions to get or set the names of the units of
  season and the seasons, see \code{\link{allSeasons}}.

}
\value{
  an integer number
}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{allSeasons}}, \code{"\link{nSeasons-methods}"}
}
\examples{
ap <- pcts(AirPassengers)
nVariables(ap)
nTicks(ap)
nCycles(ap)
nSeasons(ap)

monthplot(ap)
boxplot(ap)
}
\keyword{pcts}
