\name{nCycles}
\alias{nCycles}
\alias{nTicks}
\alias{nVariables}
\title{Basic time information about periodic time series}
\description{Basic information about periodic time series.}
\usage{
nCycles(x, \dots)

nTicks(x, \dots)

nVariables(x, \dots)
}
\arguments{
  \item{x}{an object from a periodic time series class.}
  \item{\dots}{further arguments for methods.}
}
\details{

  These are generic functions. The default methods will work for any
  objects for which \code{NROW} and \code{NCOL} are defined and have the
  conventional case by variables interpretation.

  \code{nTicks} gives the number of time points, i.e. number of rows in
  the matrix representation.

  \code{nVariables} gives the nmber of variables  in the time series.

  \code{nCycles} gives the number of cycles available in the data,
  e.g. number of years for monthly data. It always gives an integer
  number. Cuttently, if the result is not an integer an error is
  raised. \strong{TODO:} There is a case to round up or give the number of full
  cycles available but this seems somewhat dangerous if done quietly. A
  good alternative is to provide argument for control of this.

}
\value{
  integer number
}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
ap <- pcts(AirPassengers)
nVariables(ap)
nTicks(ap)
nCycles(ap)

monthplot(ap)
boxplot(ap)
}
\keyword{pcts}
