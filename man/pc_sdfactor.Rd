\name{pc_sdfactor}
\alias{pc_sdfactor}
\alias{pc.sdfactor}
\title{ Compute normalising factors }
\description{
  Compute a matrix of factors such that elementwise division of the
  periodic autocovariance matrix by it will give the periodic
  autocorrelations.
}
\usage{
pc_sdfactor(sd, maxlag)
}
\arguments{
  \item{sd}{standard deviations of the seasons numeric.}
  \item{maxlag}{maximal lag, a number.}
}
%\details{
%  
%}
\value{
  a matrix of coefficients of size \code{period x (maxlag+1)}. The
  length of \code{sd} is taken to be the period.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Georgi N. Boshnakov}
%\note{ }
\seealso{ \code{\link{autocorrelations}} }
\examples{
## equivalent to  data(Fraser, package = "pear")
Fraser <- window(Fraser2017, start = c(1912, 3), end = c(1990, 12))

logfraser <- window(pcts(log(Fraser)), start = c(1913, 1))
acvf1 <- autocovariances(logfraser, maxlag = 2)
fac <- pc_sdfactor(sqrt(acvf1[ , 0]), 2)
fac[ , 1:3]

acrf1 <- autocorrelations(logfraser, maxlag = 2)
all.equal(acvf1[], acrf1[] * fac) # TRUE
}
\keyword{pcarith}
