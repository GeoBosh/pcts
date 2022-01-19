\name{pcPlot}
\alias{pcPlot}
\alias{monthplot.PeriodicTimeSeries}
\alias{boxplot.PeriodicTimeSeries}

\title{Plot periodic time series}
\description{Plot periodic time series.}
\usage{
\method{boxplot}{PeriodicTimeSeries}(x, \dots)

\method{monthplot}{PeriodicTimeSeries}(x, ylab = deparse(substitute(x)), base, \dots)
}
\arguments{
  \item{x}{a periodic time series object.}
  \item{\dots}{
    further arguments to be passed to the plotting function.
  }
  \item{ylab}{label for the y-axis, only used for univariate time series.}
  \item{base}{a function for use for computing reference lines.}
}
\details{

  Functions for periodic/seasonal plots and boxplots.

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
\seealso{
  \code{\link{monthplot}}
}
\examples{
ap <- pcts(AirPassengers)
monthplot(ap)
boxplot(ap)

fr23 <- pcts(dataFranses1996[ , 2:3])
monthplot(fr23)
boxplot(fr23)
}
\keyword{graphics}
\keyword{ts}
