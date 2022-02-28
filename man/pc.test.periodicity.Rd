\name{periodic_acf1_test}
\alias{periodic_acf1_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{McLeod's test for periodic autocorrelation}
\description{
  Performs McLeod's test for periodic autocorrelation.
}
\usage{
periodic_acf1_test(acf, nepochs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acf}{ sample periodic autocorrelation function }
  \item{nepochs}{ the number of epochs used to get the acf }
}
\details{

  The test statistic is a scaled sum of squares of lag 1 sample periodic
  autocorrelation coefficients, see McLeod (1993), eq. (5). The
  distribution is approximately chi-square under the null hypothesis of
  no periodic autocorrelation.

}
\value{
  A list containing the following components:
  \item{statistic}{the value of the  test statistic.}
  \item{pvalue}{the p-value associated with the  test statistic.}
}
\references{
  \insertRef{Mcleod1993parsimony}{pcts}
  
  \insertRef{McLeod1994diagnostic}{pcts}

  \insertRef{McLeod1995diagnosticCorrection}{pcts}
  
}
\author{Georgi N. Boshnakov}
%\note{}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
%\examples{}
\keyword{pcstat}% at least one, from doc/KEYWORDS
