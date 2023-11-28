\name{num2pcpar}
\alias{num2pcpar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit PAR model using sample autocorrelations}
\description{Fit PAR model using sample autocorrelations.}
\usage{
num2pcpar(x, order, result = NULL, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{time series, a numeric vector.}
  \item{order}{PAR order, a single number or a vector with one entry for
    each season.}
      %\item{period}{the number of seasons.}
      %\item{mean}{If TRUE, the default, subtract the sample periodic mean
      %  when computing the autocovariances. If \code{mean} is numeric, then
      %  subtract the supplied values when computing the autocovariances.
      %}
  \item{result}{what to return, the default is to return the full model,
    see Details.}
  \item{...}{passed on to \code{calc_peracf}.}
}
\details{
  Computes the periodic autocorrelations and fits a PAR model using the
  Periodic Levinson-Durbin algorithm. 

  The order is a vector of non-negative integers, specifying the
  autoregressive orders for each season. If \code{order} is a single
  number, then all seasons have that order.

  \code{mean} controls centering in the computation of the
  autocorrelations.  If \code{mean} is numeric, then subtract the
  supplied mean before computing the autocovariances. If \code{mean} is
  TRUE, the default, compute and subtract the sample periodic mean
  before computing the autocovariances. If \code{mean} is FALSE, do not
  centre the series, i.e. assume that the mean is zero.

  If \code{result} is NULL, the default, returns the full model. If
  \code{result = "coef"}, returns the PAR coefficients only (currently
  any value of \code{result} other than NULL has this effect).

}
\value{
  The coefficients of the fitted model or a list with components:
  \item{mean}{the mean, set as described in Details.}
  \item{coef}{forward prediction coefficients.}
  \item{scale}{standard deviations of the innovations.}
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }
%
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{fitPM}} which uses \code{num2pcpar} for calculations
}
\examples{
\dontrun{
simts1 <- matrix(rnorm(100), nrow = 4)

num2pcpar(simts1, order = c(3,2,2,2), period = 4 )
num2pcpar(simts1, order = c(3,2,1,2), period = 4 )
pdSafeParOrder(c(3,2,1,2))
pdSafeParOrder(c(3,2,2,1))
num2pcpar(simts1, order = c(3,2,2,1), period = 4 )
num2pcpar(simts1, order = pdSafeParOrder(c(3,2,2,1)), period = 4 )

num2pcpar(simts1, order = c(3,2,1,2), period = 4 )
num2pcpar(simts1, order = c(3,2,1,2), period = 4, mean = rep(0,4) )
num2pcpar(simts1, order = c(3,2,1,2), period = 4, mean = FALSE )
num2pcpar(simts1, order = c(3,2,1,2), period = 4, mean = FALSE )$coef@m -
       num2pcpar(simts1, order = c(3,2,1,2), period = 4 )$coef@m
}
}
\keyword{fitmodel}
