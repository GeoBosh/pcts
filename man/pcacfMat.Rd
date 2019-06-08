\name{pcacfMat}
\alias{pcacfMat}
\alias{pc.acf.parModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute PAR autocovariance matrix}
\description{Compute PAR autocovariance matrix}
\usage{
pc.acf.parModel(parmodel, maxlag = NULL)

pcacfMat(parmodel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parmodel}{PAR model, an object of class \code{parModel}.}
  \item{maxlag}{maximum lag}
}
\details{


  \code{pc.acf.parModel} returns the autocovariances of a PAR model in
  season-lag form with maximum lag equal to \code{maxlag}. If
  \code{maxlag} is larger than the available precomputed
  autocovariances, they missing ones are computed using the Yule-Walker
  relations. Note that \code{pc.acf.parModel}
  assumes that there are enough precomputed autocovariances to use the
  Yule-Walker recursions directly.

  TODO: \code{pc.acf.parModel} is tied to the old classes since it accesses
  their slots. Could be used as a template to streamline the method for
  \code{autocovariances} for class \code{"PeriodicAutocovariance"}.


  The season-lag form can be easily converted to other forms with the
  powerful indexing operator, see  the examples and \code{\link{slMatrix-class}}.

  \code{pcacfMat} is a convenience function for statistical
  inference. It creates a covariance matrix with dimension chosen
  automatically. This covariance matrix is such that the asymptotic
  covariance matrix of the estimated parameters can be obtained by dividing
  sub-blocks by innovation variances and inverting them.  See,
  eq. (3.3) in the reference.

}
\value{
  for \code{pcacfMat}, a matrix

  for \code{pc.acf.parModel}, an \code{slMatrix}
}
\references{
  % bibentry: McLeod
McLeod A (1994).
``Diagnostic checking of periodic autoregression models with application.''
\emph{Journal of time series analysis}, \bold{15}(2), pp. 221-233.
% end:bibentry:  McLeod
}
\author{Georgi N. Boshnakov}
%\note{
%
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%  \code{\link{PeriodicArModel-class}} for PAR models,
  \code{\link{slMatrix-class}}
}
\examples{
x <- arima.sim(list(ar = 0.9), n = 1000)
proba1 <- fitPM(c(3,2,2,2), x)

acfb <- pc.acf.parModel(proba1, maxlag = 8)
acfb[4:(-2), 4:(-2), type = "tt"]

pcacfMat(proba1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{permodel}
