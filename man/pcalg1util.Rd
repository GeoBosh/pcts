\name{pcalg1util}
\alias{alg1util}
\title{
  Give partial periodic autocorrelations or other partial prediction
  quantities for a pcAcvf object.
}
\description{
  Give partial periodic autocorrelations or other partial prediction
  quantities for a pcAcvf object.
}
\usage{
alg1util(x, s, at0 = 1)
}
\arguments{
  \item{x}{an object of a class inheriting from
    \code{pc.Model.WeaklyStat}}
  \item{s}{ the required quantity, the name of one of the elements of
    the list returned by \code{\link{alg1}}.}
  \item{at0}{if not identical to "var", replace the elements of the
    result at lag zero with 1, see `Details'.}
}
\details{

  This function is a wrapper for \code{alg1()}. It calls \code{alg1}, to
  do the computations and returns the requested element as an object
  from class \code{slMatrix}.  The model order is set to the maximal lag
  avialable in \code{x},

  If \code{at0} is the character string "var", then the lag zero values
  in the result are set to the lag zero autocovariances, otherwise they
  are set to 1.
  This is mainly relevant for the periodic partial autocorrelations
  (\code{s="be"}), since the setting \code{at0="var"} ensures that they
  are in one to one correspondence with the autocovariances.

}

\value{
  the requested quantity as an object of type slMatrix
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
\seealso{ \code{\link{pdSafeParOrder}},
  \code{\link{alg1}}
}
\examples{
r1 <- rbind(c(1,0.81,0.729),c(1,0.90,0.900))

# example of Lambert-Lacroix
data(ex1f)
pc3 <- slMatrix(period=2,maxlag=5,f=ex1f,type="tt")
res0p2 <- alg1(pc3[],c(0,2))
res1p2 <- alg1(pc3[],c(1,2))
res3p3 <- alg1(pc3[],c(3,3))
}
\keyword{pcarith}% at least one, from doc/KEYWORDS
