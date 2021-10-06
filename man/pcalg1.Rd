\name{pcalg1}
\alias{alg1}
\title{
  Periodic Levinson-Durbin algorithm
}
\description{
  Calculate partial periodic autocorrelations, forward and backward
  prediction coefficients and error variances using the periodic
  Levinson-Durbin algorithm.
}
\usage{
alg1(r, p)
}
\arguments{
  \item{r}{periodic autocovariances, a matrix, see `Details'.}
  \item{p}{autoregressive orders, numeric vector.}
}
\details{
    \code{alg1(r,p)} calculates the partial periodic correlations from
    autocovariances \code{r} and autoregression orders \code{p}. The
    matrix \code{r} has the same format as that of the \code{r} slot of
    \code{pcAcvf} objects.  The periodicity, \code{d}, is set equal to
    the number of rows in \code{r}. If the length of \code{p} is not
    equal to the periodicity, all autoregressive orders are set to the
    first element of \code{p}. This last feature is really meant to be
    used only with a scalar \code{p}.

    The convention for the signs of the coefficients is the one from
    Boshnakov(1996) and is consistent with other \R time series
    functions.

    \code{pmax} below stands for the maximal element of \code{p},
    i.e. the maximal AR order.

    As in the non-periodic case, the periodic Levinson-Durbin algorithm
    fits recursively models of order 0, 1, ..., \code{pmax}. Namely,  at
    step \code{i} the AR orders for all seasons are set to
    \code{i}. This is done in a way that correctly handles the case when
    not all elements of \code{p} are equal, see the references.

    The essential quantities calculated by the periodic Levinson-Durbin
    algorithm are returned as matrices, whose \eqn{i}{i-}th rows
    contain values for season \eqn{i}. The complete details depend on
    the quantities, as described below.

    The partial autocorrelations, the forward innovation variances and
    the backward innovation variances are returned as matrices with
    \code{d} rows and \code{1+pmax} columns, whose j-th columns contain
    the quantities for order j-1 (partial autocorrelations, forward
    innovation variances and backward innovation variances, respectively).
    Note that the lag-0 partial autocorrelations are the autocovariances
    for lag 0, see the references for details.

    The forward autoregression parameters are returned as a list whose
    \eqn{j}{j-}th element is a matrix containing the coefficients for
    order \eqn{j}. Similarly for the backward autoregression parameters.

    One often is interested in the model of order \code{p} only. Its
    coefficients are given by \code{af[[pmax]]}, while the innovation
    variances are in the last column of \code{fv}.
}

\value{
  A list with the following elements.
  \item{orders }{autoregression orders}
  \item{ be }{partial autocorrelations, a matrix with d rows}
  \item{ fv }{forward innovation variances, a matrix with d rows}
  \item{ bv }{backward innovation variances, a matrix with d rows}
  \item{ af }{forward  autoregression parameters, a list with one
  element for the parameters for each order. }
  \item{ ab }{backward autoregression parameters, a list with one
    element for the parameters for each order.}

}
\references{
  \insertRef{boshnakov1996pcarma}{pcts}

  \insertRef{Sophie2000par}{pcts}
  
  \insertRef{Sophie2005pacf}{pcts}
}
\author{Georgi N. Boshnakov}
\note{
  The autoregression orders of the output are not necessarilly the same
  as those specified in the call. There may be no PAR model with the
  requested orders, see the references.
}
\seealso{ \code{\link{pdSafeParOrder}} }
\examples{
r1 <- rbind(c(1,0.81,0.729),c(1,0.90,0.900))
alg1(r1,2)

## pc2 <- pcAcvf(init=r1)
## pc2a <- pcAcvf(init=r1,seasonnames=c("am","pm"), periodunit="day")

# example of Lambert-Lacroix
data(ex1f)
pc3 <- slMatrix(period=2,maxlag=5,f=ex1f,type="tt")
res0p2 <- alg1(pc3[],c(0,2))
res1p2 <- alg1(pc3[],c(1,2))
res3p3 <- alg1(pc3[],c(3,3))

paramsys1 <- pcarma_param_system(pc3, NULL, NULL, 2, 0, 2)
t1 <- solve(paramsys1$A,paramsys1$b)

# this is from tests.r but I have lost t1
# set it to pc3 below
# note: t1 is not the t1 computed above and in other examples!

t1 <- pc3
t1
t1[]
alg1(t1[],c(1,1))
alg1(t1[],c(1,0))
alg1(t1[],c(0,1))
alg1(t1[],c(5,5))
alg1(t1[],c(2,2))
alg1(t1[],c(2,3))
alg1(t1[],c(3,3))
alg1(t1[],c(4,4))
alg1(t1[],c(5,5))
}
\keyword{pcarith}% at least one, from doc/KEYWORDS
