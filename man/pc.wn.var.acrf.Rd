\name{pcacf_pwn_var}
\alias{pcacf_pwn_var}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Variances of sample periodic autocorrelations }
\description{
  Computes the variances of sample periodic autocorrelations from
  periodic white noise.
}
\usage{
pcacf_pwn_var(nepoch, period, lag, season)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lag}{ desired lags, a vector of positive integers.}
  \item{season}{ desired seasons. }
  \item{nepoch}{ number of epochs. }
  \item{period}{ number of seasons. }
}
\details{
  These are given by McLeod (1994), see the reference, eq. (4.3).
}
\value{
  A matrix whose (i,j)th entry contains the variance of the
  autocorrelation coefficient for season \code{season[i]} and lag \code{lag[j]}.
}
\references{
  \insertRef{McLeod1994diagnostic}{pcts}
}
\author{Georgi N. Boshnakov}
%\note{}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
pcacf_pwn_var(79, 12, 0:16, 1:12)
}
\keyword{pcstat}% at least one, from doc/KEYWORDS
