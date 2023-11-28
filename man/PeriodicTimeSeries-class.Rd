\name{PeriodicTimeSeries-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicTimeSeries-class}

\title{Class PeriodicTimeSeries}
\description{Class PeriodicTimeSeries.}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.

  \code{PeriodicTimeSeries} is the root class for the periodic time
  series classes in package \code{"pcts"}. It can be used in signatures
  for methods that can handle objcets from any of them.
}
\section{Slots}{
  \describe{
    \item{\code{cycle}:}{Object of class \code{"BasicCycle"}.}
    \item{\code{pcstart}:}{Object of class \code{"ANY"}.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{Cyclic}"}, directly.
}
\section{Methods}{
  \describe{
    \item{as_date}{\code{signature(x = "PeriodicTimeSeries")}: ... }
    \item{as_datetime}{\code{signature(x = "PeriodicTimeSeries")}: ... }
    \item{autocorrelations}{\code{signature(x = "PeriodicTimeSeries", maxlag = "ANY", lag_0 = "missing")}: ... }
    \item{pcTest}{\code{signature(x = "PeriodicTimeSeries", nullmodel = "character")}: ... }

    \item{head}{\code{signature(x = "PeriodicTimeSeries")}: ... }
    \item{nTicks}{\code{signature(x = "PeriodicTimeSeries")}: ... }
    \item{pcCycle}{\code{signature(x = "PeriodicTimeSeries", type = "character")}: ... }
    \item{pcCycle}{\code{signature(x = "PeriodicTimeSeries", type =
	"missing")}: ... }
    \item{tail}{\code{signature(x = "PeriodicTimeSeries")}: ... }
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
  classes \code{\linkS4class{PeriodicTS}}, \code{\linkS4class{PeriodicMTS}}
}
%\examples{
%showClass("PeriodicTimeSeries")
%}
\keyword{classes}
