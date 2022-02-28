\name{SubsetPM-class}
\Rdversion{1.1}
\docType{class}
\alias{SubsetPM-class}
\alias{coef,SubsetPM-method}
\alias{fitted,SubsetPM-method}
\alias{residuals,SubsetPM-method}
\alias{show,SubsetPM-method}
\alias{vcov,SubsetPM-method}

\title{Class SubsetPM}
\description{

  Class \code{"SubsetPM"} - subset PAR models with trigonometric
  parameterisation.

}
\section{Objects from the Class}{

  Objects can be created by calls of the form \code{new("SubsetPM", ...)}
  but they are typically created by model fitting functions, see the examples.

}
\section{Slots}{
  \describe{
    \item{\code{theTS}:}{
      \code{"ANY"}, the time series to which the model is fitted.
    }
    \item{\code{period}:}{
      \code{"integer"}, the period.
    }
    \item{\code{order}:}{\code{"integer"}, the order.
    }
    \item{\code{findex}:}{
      \code{"function"}.
    }
    \item{\code{harmonics}:}{
      \code{"integer"}, Fourier harmonics to include in the model.
    }
    \item{\code{call}:}{
      \code{"call"}, the call used to fit the model.
    }
    \item{\code{other}:}{
      \code{"namedList"}.
    }
  }
}
\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "SubsetPM")}: ... }
    \item{fitted}{\code{signature(object = "SubsetPM")}: ... }
    \item{residuals}{\code{signature(object = "SubsetPM")}: ... }
    \item{show}{\code{signature(object = "SubsetPM")}: ... }
    \item{vcov}{\code{signature(object = "SubsetPM")}: ... }
	 }
}
%\references{
%%%  ~~put references to the literature/web site here~~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{fit_trigPAR_optim}}
}
\examples{
pcfr4 <- pcts(dataFranses1996)[[4]]
x4 <- as.numeric(window(pcfr4, start = availStart(pcfr4), end = availEnd(pcfr4)))

## without 'harmonics' these models are equivalent
tmpfit  <- fit_trigPAR_optim(x4, 2, 4, tol = 1e-14, verbose = FALSE)
tmpfitL <- fit_trigPAR_optim(x4, 2, 4, tol = 1e-14, type = "bylag", verbose = FALSE)

## for comparison
tmpfitP <- pclsdf(x4, 4, 1:2, sintercept = FALSE)

## with intercept
tmpfitc  <- fit_trigPAR_optim(x4, 2, 4, tol = 1e-14, verbose = FALSE,
    sintercept = TRUE)
tmpfitcn  <- fit_trigPAR_optim(x4, 2, 4, tol = 1e-14, verbose = FALSE,
    sintercept = structure(TRUE, merge = TRUE))
tmpfitLc <- fit_trigPAR_optim(x4, 2, 4, tol = 1e-14, type = "bylag",
    verbose = FALSE, sintercept = TRUE)

coef(tmpfitc, matrix = TRUE)
coef(tmpfitcn, matrix = TRUE)
coef(tmpfitLc, matrix = TRUE)

coef(tmpfitc)
coef(tmpfitcn)
coef(tmpfitLc)

coef(tmpfit)
coef(tmpfitL)

## convert to PAR coefficients:
coef(tmpfitc,  type = "PAR", matrix = TRUE)
coef(tmpfitcn, type = "PAR", matrix = TRUE)
coef(tmpfitLc, type = "PAR", matrix = TRUE)

coef(tmpfitL, type = "PAR", matrix = TRUE)



predict(tmpfitc, n.ahead = 4)
predict(tmpfitcn, n.ahead = 4)

sqrt(diag((vcov(tmpfitL))))
e <- residuals(tmpfitL)
fi <-  fitted(tmpfitL)
}
\keyword{classes}
