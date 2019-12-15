\name{PeriodicArModel-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicArModel-class}
%\alias{autocovariances,PeriodicArModel-method}
%\alias{fitPM,ANY,PeriodicArModel-method}
%\alias{predictionCoefficients,PeriodicArModel-method}

\title{Class PeriodicArModel}
\description{Class PeriodicArModel.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicArModel", ar, ma, sigma2, ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    %\item{\code{const}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sigma2}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ar}:}{Object of class \code{"PeriodicArFilter"} ~~ }
    \item{\code{ma}:}{Object of class \code{"PeriodicMaFilter"} ~~ }

    \item{\code{center}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intercept}:}{Object of class \code{"numeric"} ~~ }
}
}
\section{Extends}{
Class \code{"\linkS4class{PeriodicArmaModel}"}, directly.
Class \code{"\linkS4class{VirtualPeriodicArmaModel}"}, by class "PeriodicArmaModel", distance 2.
Class \code{"\linkS4class{PeriodicArmaSpec}"}, by class "PeriodicArmaModel", distance 2.
Class \code{"\linkS4class{VirtualPeriodicFilterModel}"}, by class "PeriodicArmaModel", distance 3.
Class \code{"\linkS4class{VirtualPeriodicStationaryModel}"}, by class "PeriodicArmaModel", distance 3.
Class \code{"\linkS4class{PeriodicArmaFilter}"}, by class "PeriodicArmaModel", distance 3.
Class \code{"\linkS4class{VirtualPeriodicAutocovarianceModel}"}, by class "PeriodicArmaModel", distance 4.
Class \code{"\linkS4class{VirtualPeriodicMeanModel}"}, by class "PeriodicArmaModel", distance 4.
}
\section{Methods}{
  \describe{
    \item{autocovariances}{\code{signature(x = "PeriodicArModel")}: ... }
    \item{fitPM}{\code{signature(x = "ANY", model = "PeriodicArModel")}: ... }
%%%     \item{predictionCoefficients}{\code{signature(x = "PeriodicArModel")}: ... }
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
%showClass("PeriodicArModel")
%}
\keyword{classes}

