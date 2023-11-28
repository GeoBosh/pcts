\name{FittedPeriodicArmaModel-class}
\Rdversion{1.1}
\docType{class}
\alias{FittedPeriodicArmaModel-class}
\alias{as_pcarma_list,FittedPeriodicArmaModel-method}
\alias{show,FittedPeriodicArmaModel-method}

\title{Class FittedPeriodicArmaModel}
\description{Class FittedPeriodicArmaModel in package pcts}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FittedPeriodicArmaModel", ..., mean)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{ar}:}{Object of class \code{"PeriodicBJFilter"} ~~ }
    \item{\code{ma}:}{Object of class \code{"PeriodicSPFilter"} ~~ }
    \item{\code{modelCycle}:}{Object of class \code{"BasicCycle"} ~~ }
    \item{\code{center}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intercept}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sigma2}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{theTS}:}{Object of class \code{"PeriodicTS"} ~~ }
    \item{\code{asyCov}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{ns}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{PeriodicArmaModel}"}, directly.
Class \code{"FittedPM"}, directly.
Class \code{"\linkS4class{VirtualPeriodicArmaModel}"}, by class "PeriodicArmaModel", distance 2.
Class \code{"\linkS4class{PeriodicArmaSpec}"}, by class "PeriodicArmaModel", distance 2.
Class \code{"\linkS4class{VirtualPeriodicFilterModel}"}, by class "PeriodicArmaModel", distance 3.
Class \code{"\linkS4class{VirtualPeriodicStationaryModel}"}, by class "PeriodicArmaModel", distance 3.
Class \code{"\linkS4class{PeriodicArmaFilter}"}, by class "PeriodicArmaModel", distance 3.
Class \code{"\linkS4class{PeriodicInterceptSpec}"}, by class "PeriodicArmaModel", distance 3.
Class \code{"\linkS4class{VirtualPeriodicAutocovarianceModel}"}, by class "PeriodicArmaModel", distance 4.
Class \code{"\linkS4class{VirtualPeriodicMeanModel}"}, by class "PeriodicArmaModel", distance 4.
Class \code{"\linkS4class{VirtualArmaFilter}"}, by class "PeriodicArmaModel", distance 4.
Class \code{"\linkS4class{ModelCycleSpec}"}, by class "PeriodicArmaModel", distance 4.
Class \code{"\linkS4class{InterceptSpec}"}, by class "PeriodicArmaModel", distance 4.
Class \code{"\linkS4class{VirtualPeriodicModel}"}, by class "PeriodicArmaModel", distance 5.
Class \code{"\linkS4class{VirtualMonicFilter}"}, by class "PeriodicArmaModel", distance 5.
}
\section{Methods}{
  \describe{
    \item{as_pcarma_list}{\code{signature(object = "FittedPeriodicArmaModel")}: ... }
    \item{show}{\code{signature(object = "FittedPeriodicArmaModel")}: ... }
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
%showClass("FittedPeriodicArmaModel")
%}
\keyword{classes}
