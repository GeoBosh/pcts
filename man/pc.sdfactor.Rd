\name{pc.sdfactor}
\alias{pc.sdfactor}
\title{ Compute normalising factors }
\description{
  This function computes a matrix of factors such that elementwise
  division of the periodic autocovariance matrix by it will give the periodic
  autocorrelations.
}
\usage{
pc.sdfactor(sd, maxlag)
}
\arguments{
  \item{sd}{  standard deviations of the seasons }
  \item{maxlag}{ maximal lag }
}
\details{
  { todo: describe! }
}
\value{
  a matrix of coefficients of size \code{period x (maxlag+1)}. The
  length of \code{sd} is taken to be the period.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Georgi N. Boshnakov}
%\note{ }
\seealso{ \code{\link{autocorrelations}} }
%\examples{ }
\keyword{pcarith}% at least one, from doc/KEYWORDS
