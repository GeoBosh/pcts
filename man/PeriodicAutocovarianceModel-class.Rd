\name{PeriodicAutocovarianceModel-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicAutocovarianceModel-class}

\title{Class PeriodicAutocovarianceModel}
\description{Class PeriodicAutocovarianceModel.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicAutocovarianceModel", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{pcmatrix}:}{Object of class \code{"ANY"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualPeriodicAutocovarianceModel}"}, directly.
Class \code{"\linkS4class{PeriodicAutocovarianceSpec}"}, directly.
}
\section{Methods}{
  \describe{
%%%     \item{autocorrelations}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
    \item{autocovariances}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
%%%    \item{backwardPredictionCoefficients}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
%%%     \item{backwardPredictionVariances}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
%%%     \item{partialAutocorrelations}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
%%%     \item{partialAutocovariances}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
%%%     \item{pc.phis2}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
%%%     \item{predictionCoefficients}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
%%%     \item{partialVariances}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
	 
    \item{[}{\code{signature(x = "PeriodicAutocovarianceModel", i = "missing", j = "missing", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicAutocovarianceModel", i = "missing", j = "numeric", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicAutocovarianceModel", i = "numeric", j = "missing", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicAutocovarianceModel", i = "numeric", j = "numeric", drop = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "PeriodicAutocovarianceModel", i = "ANY", j = "ANY", value = "ANY")}: ... }
    \item{autocovariances}{\code{signature(x = "PeriodicAutocovarianceModel", maxlag = "ANY")}: ... }
    \item{backwardPartialCoefficients}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
    \item{backwardPartialVariances}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
    \item{maxLag}{\code{signature(object = "PeriodicAutocovarianceModel")}: ... }
    \item{partialAutocorrelations}{\code{signature(x = "PeriodicAutocovarianceModel", maxlag = "ANY", lag_0 = "ANY")}: ... }
    \item{partialAutocovariances}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
    \item{partialCoefficients}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
    \item{partialVariances}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
    \item{pc.bU}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
    \item{pc.fL}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
    \item{pc.phis2}{\code{signature(x = "PeriodicAutocovarianceModel")}: ... }
    \item{[[}{\code{signature(x = "PeriodicAutocovarianceModel", i = "numeric")}: ... }}
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
%showClass("PeriodicAutocovarianceModel")
%}
\keyword{classes}


