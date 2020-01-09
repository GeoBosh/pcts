\name{pclspiar}
\alias{pclspiar}
\title{Fit a periodically integrated autoregressive model}
\description{Fit a periodically integrated autoregressive model.}
\usage{
pclspiar(x, d, p, icoef = NULL, parcoef = NULL, sintercept = FALSE,
         seasonof1st = 1, weights = TRUE, itol = 1e-07, maxniter = 1000)
}
\arguments{
  \item{x}{time series.}
  \item{d}{period.}
  \item{p}{order of the model, a positive integer, see Details.}
  \item{icoef}{
    initial values for the periodic integration coefficients.
  }
  \item{parcoef}{not used currently.}
  \item{sintercept}{if \code{TRUE} include seasonal intercepts.}
  \item{seasonof1st}{season of the first observation.}
  \item{weights}{if \code{TRUE}, use periodic weights in the nonlinear least
    squares, see Details.
  }
  \item{itol}{threshold value for the stopping criterion.
  }
  \item{maxniter}{maximum number of iterations.}
}
\details{

  This function fits a periodically integrated autoregressive model
  using non-linear least squares. The order of integration is one and
  the order of the periodically correlated part is \code{p - 1}. So,
  \code{p} must be greater than or equal to one.

  If \code{weights = TRUE} the non-linear optimisation is done with
  weights inversely proportional to the innovation variances for the
  seasons, otherwise the unweighted sum of squared residuals is
  minimised.

}
\value{
  a list currently containing the following elements:
  \item{icoef}{coefficients of the periodic integration filter.}
  \item{parcoef}{coefficients of the PAR filter.}
  \item{sintercept}{seasonal intercepts.}
  \item{sigma2hat}{innovation variances.}
}
\references{
  % bibentry: FransesB1
Franses PH (1996).
\emph{Periodicity and Stochastic Trends In Economic Time Series}.
Oxford University Press Inc., New York.
% end:bibentry:  FransesB1

  % bibentry: FransesB2
Franses PH and Paap R (2004).
\emph{Periodic Time Series Models}.
Oxford University Press Inc., New York.
% end:bibentry:  FransesB2

  % bibentry: boshnakov2009gen
Boshnakov GN and Iqelan BM (2009).
``Generation of time series models with given spectral
properties.''
\emph{J. Time Series Anal.}, \bold{30}(3), pp. 349--368.
ISSN 0143-9782, \url{http://dx.doi.org/10.1111/j.1467-9892.2009.00617.x}.
% end:bibentry:  boshnakov2009gen

}
\author{
  Georgi N. Boshnakov
}
% \note{
% %%  ~~further notes~~
% }
%
% %% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{pclsdf}},
  \code{\link{test_piar}},
  \code{\link{fitPM}}
}
\examples{
## see also the examples for fitPM()
ts1 <- window(dataFranses1996[ , "CanadaUnemployment"],
              start = c(1960, 1), end = c(1987, 4))
pclspiar(ts1, 4, p = 1, sintercept = TRUE)
pclspiar(ts1, 4, p = 2, sintercept = TRUE)
}
\keyword{ pcts }
