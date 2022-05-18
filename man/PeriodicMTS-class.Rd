\name{PeriodicMTS-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicMTS-class}
\alias{plot,PeriodicMTS,missing-method}

\title{Class \code{"PeriodicMTS"}}
\description{

  Class \code{"PeriodicMTS"} is the main class for multivariate periodic
  time series in package \code{"pcts"}.

}
\section{Objects from the Class}{

  Objects can be created by calls of the form
  \code{new("PeriodicMTS", ...)} but it is recommended to use 
  the function \code{\link{pcts}} in most cases. 
  
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"matrix"},
      the core data.  Several functions can be used to extract it in
      various formats, see \code{\link{Vec}}.

    }
    \item{\code{cycle}:}{Object of class \code{"BasicCycle"},
      representing the seasonal information, see
      \code{\link{pcCycle}}. 

    }
    \item{\code{pcstart}:}{Object of class \code{"ANY"},
      the time of the first observation.

    }
  }
}
\section{Extends}{
Class \code{"\linkS4class{PeriodicTimeSeries}"}, directly.
Class \code{"\linkS4class{matrix}"}, from data part.
Class \code{"\linkS4class{Cyclic}"}, by class "PeriodicTimeSeries", distance 2.
Class \code{"\linkS4class{array}"}, by class "matrix", distance 2.
% 2016-10-20: do not link since numLike is not exported and R CMD check
% gives warning:
%      * checking Rd cross-references ... WARNING
%      Missing link or links in documentation object 'LagVector-class.Rd':
%        'numLike-class'
%Class \code{"\linkS4class{mMatrix}"}, by class "matrix", distance 2.
Class \code{"mMatrix"}, by class "matrix", distance 2.
Class \code{"\linkS4class{optionalMatrix}"}, by class "matrix", distance 2.
Class \code{"\linkS4class{structure}"}, by class "matrix", distance 3.
Class \code{"\linkS4class{vector}"}, by class "matrix", distance 4, with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{$}{\code{signature(x = "PeriodicMTS")}: ... }

    \item{[}{\code{signature(x = "PeriodicMTS", i = "missing", j = "missing", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicMTS", i = "ANY", j = "missing", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicMTS", i = "ANY", j = "ANY", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicMTS", i = "AnyDateTime", j = "missing", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicMTS", i = "AnyDateTime", j = "ANY", drop = "ANY")}: ... }

    \item{[[}{\code{signature(x = "PeriodicMTS", i = "ANY", j = "ANY")}: ... }

    \item{coerce}{\code{signature(from = "mts", to = "PeriodicMTS")}: ... }
    \item{coerce}{\code{signature(from = "PeriodicMTS", to = "ts")}: ... }
    \item{coerce}{\code{signature(from = "ts", to = "PeriodicMTS")}: ... }
    \item{coerce}{\code{signature(from = "PeriodicMTS", to = "Cyclic")}: ... }

    \item{coerce<-}{\code{signature(from = "PeriodicMTS", to = "Cyclic")}: ... }

    \item{plot}{\code{signature(x = "PeriodicMTS", y = "missing")}: ... }
    \item{show}{\code{signature(object = "PeriodicMTS")}: ... }
    \item{summary}{\code{signature(object = "PeriodicMTS")}: ... }
  
    \item{fitPM}{\code{signature(model = "PeriodicArModel", x = "PeriodicMTS")}: ... }
    \item{pcApply}{\code{signature(object = "PeriodicMTS")}: ... }
    \item{pcMean}{\code{signature(object = "PeriodicMTS")}: ... }
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
  class \code{\linkS4class{PeriodicTS}} (univariate periodic time series),

  \code{\link{pcts}} (create periodic time series),

  \code{\link{dataFranses1996}} and \code{\link{pcts-package}} for examples
}
\examples{
pcfr <- pcts(dataFranses1996)
colnames(pcfr)[4] # "GermanyGNP"

## extracting single time series as univariate 
class(pcfr[[4]]) # "PeriodicTS"
identical(pcfr[[4]], pcfr$GermanyGNP )     # TRUE
identical(pcfr[[4]], pcfr[["GermanyGNP"]]) # TRUE
plot(pcfr[[4]])

## ... and as multivariate
pcfr[4] #  "PeriodicMTS"
plot(pcfr[4])

## extracting more than one time series
plot(pcfr[2:4])
summary(pcfr[2:4])

pcfr2 <- pcfr[[2]]
plot(pcfr2)
}
\keyword{classes}
