\name{pclsdf}
\alias{pclsdf}
\title{Fit PAR models using least squares}
\description{Fit PAR models using least squares. The model may contain
  intercepts and linear trends, seasonal or non-seasonal.}
\usage{
pclsdf(x, d, lags = integer(0), sintercept = TRUE, sslope = FALSE,
       intercept = FALSE, slope = FALSE, xreg, contrasts = NULL,
       seasonof1st = NULL, coefonly = FALSE)
}
\arguments{
  \item{x}{time series, a numeric vector.}
  \item{d}{period, an integer.}
  \item{lags}{an integer vector, typically \code{1:p}, where \code{p} is
    the order of the autoregression. The same lags are used for all
    seasons.
  }
  \item{sintercept}{if TRUE include seasonal intercepts.}
  \item{sslope}{if TRUE include seasonal linear trend.}
  \item{intercept}{if TRUE include non-seasonal intercept.}
  \item{slope}{if TRUE include non-seasonal linear trend.}
  \item{xreg}{additional regressors, not used currently.}
  %% changed the handling, was: \item{contrasts}{passed on to \code{lm}.}
  \item{contrasts}{contrasts to use for the seasons factor variable.}
  \item{seasonof1st}{season of the first observation in the time series,
    see Details.
  }
  \item{coefonly}{if TRUE, return only the parameters of the fitted
    model, otherwise include also the object returned by \code{lm}.
  }
}
\details{

  This function fits PAR models by the method of least squares.
  Seasonal intercepts are included by default. Non-seasonal intercepts
  are available, as well as seasonal and non-seasonal linear trend.
  Separate arguments are provided, so that any combination of seasonal
  and non-seasonal intercepts and slopes can be specified.

  If \code{coefonly} is TRUE, \code{pclsdf} returns only the estimated
  parameters, otherwise it includes additional statistical information,
  see section Note for the current details.

}
\value{
  A list with the components listed below. Some components are present
  only if included in the model specification.
  \item{par}{the PAR coefficients, a matrix with a row for each season.}
  \item{sintercept}{(if specified) seasonal intercepts, a numeric vector.}
  \item{sigma2hat}{innovation variances.}
  \item{formula.char}{the formula used in the call of \code{lm}, a
    character string.
  }
  \item{fit}{(if \code{coefonly = FALSE}) the fitted object obtained
    from \code{lm}.
  }
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
  Georgi N. Boshnakov
}
\note{

  Currently, \code{pclsdf} prepares a model formula according to the
  specification and calls \code{lm} to do the fitting. Component "fit"
  in the result (available when \code{coefonly = FALSE}) contains the
  raw fitted object returned by \code{lm}. Statistical inference based
  on this object would, in general, not be justified for correlated
  data.

  \strong{todo:} currently some of the parameters are returned only via the
  fitted object from \code{lm}.
}
%
% %% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{pclspiar}},
}
\examples{
## data(dataFranses1996)
cu <- pcts(dataFranses1996[ , "CanadaUnemployment"])
cu <- window(cu, start = availStart(cu), end = availEnd(cu))

pclsdf(cu, 4, 1:2, sintercept = TRUE)

pclsdf(austres, 4, lags = 1:3)
pclsdf(austres, 4, lags = 1:3, sintercept = TRUE)
pclsdf(austres, 4, lags = 1:3, sintercept = TRUE, sslope = TRUE)

x <- rep(1:4,10)
pclsdf(x, 4, lags = 1:3, sintercept = TRUE, sslope = TRUE)

## this is for the version when contrasts arg. was passed on directly to lm.
## tmp1 <- pclsdf(austres, 4, lags = 1, sintercept = FALSE, sslope = TRUE,
##                contrasts = list(Season = "contr.sum" ))
}
\keyword{pcts}
