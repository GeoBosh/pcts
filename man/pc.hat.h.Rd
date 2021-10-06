\name{pc.hat.h}
\alias{pc.hat.h}
\title{ function to compute estimates of the h weights }
\description{
  The h coefficients are scaled cross-covariances between the time
  series and the innovations. This function computes estimates for h
  using as input the observed series, a series of estimated innovations,
  and an estimate of the variance of the innovations.
}
\usage{
pc.hat.h(x, eps, maxlag, si2hat)
}
\arguments{
  \item{x}{ the observed time series x(t) }
  \item{eps}{ a series of esimated innovations  }
  \item{maxlag}{ maximum lag  }
  \item{si2hat}{ estimate of the variance of the innovations }
}
\details{
  If missing, the variance of the innovations is estimated from \code{eps}.
}
\value{
  A matrix of the coefficient up to lag maxlag with one row for each season.
}
\references{
  \insertRef{boshnakov1996pcarma}{pcts}
}
\author{Georgi N. Boshnakov}
%\note{ ~~further notes~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
%\examples{
%}
\keyword{pcarma}
