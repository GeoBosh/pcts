\name{ex1f}
\docType{data}
\alias{ex1f}
\title{An example PAR autocorrelation function}
\description{

  \code{ex1} is the autocorrelation function used in the reference as an
  example when the solution of the periodic Yule-Walker system gives an
  invalid PAR model.  This can happen only if Lambert-Lacroix's
  condition on the PAR order is not satisfied, see \code{pdSafeParOrder}.

}
%\usage{ex1f(t,s)}
%\arguments{
%  \item{t,s}{integer numbers.}
%}
\format{A function of two arguments}
\seealso{ \code{\link{pdSafeParOrder}} }
\source{See pp. 429--430 of the reference.}
\references{
  % bibentry: Sophie2005pacf
Lambert-Lacroix S (2005).
`` Extension of autocovariance coefficients sequence for periodically correlated
processes.''
\emph{Journal of Time Series Analysis}, \bold{26}(6), pp. 423-435.
% end:bibentry:  Sophie2005pacf
}
\examples{
data(ex1f)
## compute the first few autocorrelations
pc3 <- slMatrix(period = 2, maxlag = 5, f = ex1f, type = "tt")
## Fir a PAR(0,2) model
res0p2 <- alg1(pc3[],c(0,2))
## model is invalid since a partial autocorrelation is larger than one:
res0p2$be
## Find a modified order:
pdSafeParOrder(c(0,2)) # PAR(1,2)
## now the parcor's are fine:
res1p2 <- alg1(pc3[],c(1,2))
res1p2$be
}
\keyword{par}
