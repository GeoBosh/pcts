\name{pdSafeParOrder}
\alias{pdSafeParOrder}
\title{ Functions for some basic operations with seasons}
\description{Functions for some basic operations with seasons.}
\usage{
pdSafeParOrder(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{autoregression order, a vector of integers.}
}
\details{
  
 \code{pdSafeParOrder(p)} modifies the periodic AR order specified by vector
 \code{p}. The modified order is such that the correspondence between
 autocovariances and partial autocorrelations is one-to-one, see
 the references for details.

}
\value{
  a vector of integers
}
\references{
  % bibentry: Sophie2000par
Lambert-Lacroix S (2000).
``On periodic autoregressive process estimation .''
\emph{ IEEE Transactions on Signal Processing}, \bold{48}( 6 ), pp. 1800-1803.
% end:bibentry:  Sophie2000par

  % bibentry: Sophie2005pacf
Lambert-Lacroix S (2005).
`` Extension of autocovariance coefficients sequence for periodically correlated
processes.''
\emph{Journal of Time Series Analysis}, \bold{26}(6), pp. 423-435.
% end:bibentry:  Sophie2005pacf
}

\author{Georgi N. Boshnakov}
%\note{}
%\seealso{ \code{\link{pcAcvf}} }
\examples{
pdSafeParOrder(c(0,2))
pdSafeParOrder(c(2,3))
}
\keyword{permodel}
