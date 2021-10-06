\name{pcAR2acf}
\alias{pcAR2acf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute periodic autocorrelations from PAR coefficients}
\description{Compute periodic autocorrelations from PAR
  coefficients. This effectively solves the inverse problem to that
  solved by the periodic Levinson-Durbin algorithm but does not use a
  recursion.}
\usage{
pcAR2acf(coef, sigma2, p, maxlag = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coef}{PAR coefficients, a matrix, see Details.}
  \item{sigma2}{innovations variances.}
  \item{p}{PAR order.}
  \item{maxlag}{How many lags to compute.}
}
\details{

  \code{coef} is a matrix with the coefficients for season \code{i} in
  the i-th row. The coefficients start from lag 1.

  The first few autocorrelations are computed by solving a linear
  system, see the references. The rest, are generated using the periodic
  Yule-Walker equations.

}
\value{
  a matrix, in which row \code{s} contains the acf's for season \code{s}
  for lags 0, 1, ..., maxlag (in this order).
}
\references{
  \insertRef{boshnakov1996pcarma}{pcts}
  
  \insertRef{b:Varna92}{pcts}
}
\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{pcarma_acvf_lazy}}, which does the main computation, but note
  that the coefficients for it start from lag zero
}
\examples{
m <- rbind( c(0.81, 0), c(0.4972376, 0.4972376) )
si2 <- PeriodicVector(c(0.3439000, 0.1049724))

pcAR2acf(m)
pcAR2acf(m, si2)
pcAR2acf(m, si2, 2)
pcAR2acf(m, si2, 2, maxlag = 10)


# same using pcarma_acvf_lazy directly
m1 <- rbind( c(1, 0.81, 0), c(1, 0.4972376, 0.4972376) )

testphi <- slMatrix(init = m1)
myf <- pcarma_acvf_lazy(testphi, testtheta, si2, 2, 0, 2, maxlag = 10)
myf(1:2, 0:9)    # get a matrix of values

all(myf(1:2, 0:9) == pcAR2acf(m, si2, 2, maxlag = 9)) # TRUE

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{permodel}

