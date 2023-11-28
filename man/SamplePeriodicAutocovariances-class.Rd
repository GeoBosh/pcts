\name{SamplePeriodicAutocovariances-class}
\Rdversion{1.1}
\docType{class}
\alias{SamplePeriodicAutocovariances-class}
%\alias{autocorrelations,SamplePeriodicAutocovariances,ANY,missing-method}

\title{Class SamplePeriodicAutocovariances}
\description{Class SamplePeriodicAutocovariances.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SamplePeriodicAutocovariances", ..., data)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{modelCycle}:}{Object of class \code{"BasicCycle"} ~~ }
    \item{\code{data}:}{Object of class \code{"Lagged"} ~~ }
    \item{\code{n}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{varnames}:}{Object of class \code{"character"} ~~ }
    \item{\code{objectname}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{PeriodicAutocovariances}"}, directly.
Class \code{"\linkS4class{Fitted}"}, directly.
Class \code{"\linkS4class{ModelCycleSpec}"}, by class "PeriodicAutocovariances", distance 2.
Class \code{"\linkS4class{FlexibleLagged}"}, by class "PeriodicAutocovariances", distance 2.
Class \code{"\linkS4class{VirtualPeriodicAutocovariances}"}, by class "PeriodicAutocovariances", distance 2.
Class \code{"\linkS4class{Lagged}"}, by class "PeriodicAutocovariances", distance 3.
Class \code{"\linkS4class{VirtualPeriodicModel}"}, by class "PeriodicAutocovariances", distance 3.
}
\section{Methods}{
  \describe{
    \item{autocorrelations}{\code{signature(x = "SamplePeriodicAutocovariances", maxlag = "ANY", lag_0 = "missing")}: ... }
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
%showClass("SamplePeriodicAutocovariances")
%}
\keyword{classes}
