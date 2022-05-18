\name{PeriodicTS-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicTS-class}
\alias{coerce,mts,PeriodicTS-method}
\alias{coerce,PeriodicTS,ts-method}
\alias{coerce,ts,PeriodicTS-method}
\alias{plot,PeriodicTS,missing-method}
\alias{show,PeriodicTS-method}
\alias{summary,PeriodicTS-method}

\title{Class \code{"PeriodicTS"}}
\description{

  Class \code{"PeriodicTS"} is the main class for univariate periodic
  time series in package \code{"pcts"}.

}
\section{Objects from the Class}{

  Objects can be created from numeric vectors and objects from other
  time series classes by calling \code{\link{pcts}} (recommended in most
  cases).

  It is possible also to use calls of the form \code{new("PeriodicTS",
  ...)}. This is more useful in programming. 

}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"numeric"},
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
Class \code{"\linkS4class{numeric}"}, from data part.
Class \code{"\linkS4class{Cyclic}"}, by class "PeriodicTimeSeries", distance 2.
Class \code{"\linkS4class{vector}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{index}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{replValue}"}, by class "numeric", distance 2.
% 2016-10-20: do not link since numLike is not exported and R CMD check
% gives warning:
%      * checking Rd cross-references ... WARNING
%      Missing link or links in documentation object 'LagVector-class.Rd':
%        'numLike-class'
%Class \code{"\linkS4class{numLike}"}, by class "numeric", distance 2.
Class \code{"numLike"}, by class "numeric", distance 2.
Class \code{"\linkS4class{number}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{atomicVector}"}, by class "numeric", distance 2.
Class \code{"numericVector"}, by class "numeric", distance 2.
Class \code{"replValueSp"}, by class "numeric", distance 3.
Class \code{"Mnumeric"}, by class "numeric", distance 3.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "PeriodicTS", i = "AnyDateTime", j = "missing", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicTS", i = "missing", j = "missing", drop = "ANY")}: ... }

    \item{coerce}{\code{signature(from = "mts", to = "PeriodicTS")}: ... }
    \item{coerce}{\code{signature(from = "PeriodicTS", to = "ts")}: ... }
    \item{coerce}{\code{signature(from = "ts", to = "PeriodicTS")}: ... }
    \item{coerce}{\code{signature(from = "PeriodicTS", to = "Cyclic")}: ... }

    \item{coerce<-}{\code{signature(from = "PeriodicTS", to = "Cyclic")}: ... }

    \item{plot}{\code{signature(x = "PeriodicTS", y = "missing")}: ... }
    \item{show}{\code{signature(object = "PeriodicTS")}: ... }
    \item{summary}{\code{signature(object = "PeriodicTS")}: ... }
  
    \item{autocovariances}{\code{signature(x = "PeriodicTS", maxlag = "ANY")}: ... }
    \item{fitPM}{\code{signature(model = "PeriodicArModel", x = "PeriodicTS")}: ... }
    \item{pcApply}{\code{signature(object = "PeriodicTS")}: ... }
    \item{pcMean}{\code{signature(object = "PeriodicTS")}: ... }
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
  \code{\link{pcts}} for creating \code{"PeriodicTS" } objects from raw
  vectors and objects from other time series classes.
  
  \code{\linkS4class{PeriodicMTS}} for multivariate periodic time
  series.
}
%\examples{
%showClass("PeriodicTS")
%}
\keyword{classes}
