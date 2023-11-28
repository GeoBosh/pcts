\name{PeriodicAutocovariances-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicAutocovariances-class}
%\alias{autocorrelations,PeriodicAutocovariances,ANY,missing-method}
%\alias{partialAutocorrelations,PeriodicAutocovariances,ANY,missing-method}

\title{Class PeriodicAutocovariances}
\description{Class PeriodicAutocovariances.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicAutocovariances", ..., data)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{modelCycle}:}{Object of class \code{"BasicCycle"} ~~ }
    \item{\code{data}:}{Object of class \code{"Lagged"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{ModelCycleSpec}"}, directly.
Class \code{"\linkS4class{FlexibleLagged}"}, directly.
Class \code{"\linkS4class{VirtualPeriodicAutocovariances}"}, directly.
Class \code{"\linkS4class{Lagged}"}, by class "FlexibleLagged", distance 2.
Class \code{"\linkS4class{VirtualPeriodicModel}"}, by class "VirtualPeriodicAutocovariances", distance 2.
}
\section{Methods}{
  \describe{
    \item{autocorrelations}{\code{signature(x = "PeriodicAutocovariances", maxlag = "ANY", lag_0 = "missing")}: ... }
    \item{partialAutocorrelations}{\code{signature(x = "PeriodicAutocovariances", maxlag = "ANY", lag_0 = "missing")}: ... }
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
%\seealso{
%%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
%\examples{
%showClass("PeriodicAutocovariances")
%}
\keyword{classes}
