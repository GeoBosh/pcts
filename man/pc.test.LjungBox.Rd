\name{pwn_McLeodLjungBox_test}
\alias{pwn_McLeodLjungBox_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ McLeod-Ljung-Box test for periodic white noise }
\description{
    Compute the McLeod-Ljung-Box test statistic for examining
     the null hypothesis of periodic white noise.
}
\usage{
pwn_McLeodLjungBox_test(acf, nepoch, use = 1:maxlag,
                 maxlag = ncol(as.matrix(acf)) - 1,
                 period = nrow(as.matrix(acf)), fitdf = numeric(period))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acf}{
    the sample periodic autocorrelation function of the time series.
  }
  \item{nepoch}{ number of cycles used in computing the acf. }
  \item{use}{ number of lags to use, may be a vector.}
  \item{maxlag}{maximal lag.}
  \item{period}{number of seasons in a cycle.}
  \item{fitdf}{degrees of freedom corrections for the number of
    estimated parameters, see Details.}
}
\details{

  The McLeod-Ljung-Box test can be used to test the null hypothesis of
  periodic white noise.

  If \code{acf} contains sample autocorrelations of residuals from a
  fitted model, a correction of the degrees of freedom is strongly
  recommended.

  Argument \code{fitdf} is a vector specifying how may degrees of
  freedom to subtract for each season. In the case of PAR models
  \code{fitdf} can be set to the PAR orders.

  The value of the statistic is set to NA where the correction for
  degrees of freedom results in negative numbers.

}
\value{
  A list containing the following components:
  \item{ statistic }{the value of the  test statistic for each lag
    specified by \code{use}.}
  \item{ df }{the corresponding degrees of freedom}
}
\references{
  % bibentry: McLeod
McLeod A (1994).
``Diagnostic checking of periodic autoregression models with application.''
\emph{Journal of time series analysis}, \bold{15}(2), pp. 221-233.
% end:bibentry:  McLeod
}
\author{Georgi N. Boshnakov}
\note{
  TODO: Consolidate this and similar tests!
}
\seealso{
  \code{\link{Box.test}} for the non-periodic case
}
%\examples{}
\keyword{pcstat}% at least one, from doc/KEYWORDS
