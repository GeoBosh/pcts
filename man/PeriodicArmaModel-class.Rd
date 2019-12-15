\name{PeriodicArmaModel-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicArmaModel-class}
%\alias{autocovariances,PeriodicArmaModel-method}
%\alias{PeriodicArModel,PeriodicArmaModel-method}

\title{Class PeriodicArmaModel}
\description{Class PeriodicArmaModel.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicArmaModel", ar, ma, sigma2, ...)}.
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
Class \code{"\linkS4class{VirtualPeriodicArmaModel}"}, directly.
Class \code{"\linkS4class{PeriodicArmaSpec}"}, directly.
Class \code{"\linkS4class{VirtualPeriodicFilterModel}"}, by class "VirtualPeriodicArmaModel", distance 2.
Class \code{"\linkS4class{VirtualPeriodicStationaryModel}"}, by class "VirtualPeriodicArmaModel", distance 2.
Class \code{"\linkS4class{PeriodicArmaFilter}"}, by class "PeriodicArmaSpec", distance 2.
Class \code{"\linkS4class{VirtualPeriodicAutocovarianceModel}"}, by class "VirtualPeriodicArmaModel", distance 3.
Class \code{"\linkS4class{VirtualPeriodicMeanModel}"}, by class "VirtualPeriodicArmaModel", distance 3.
}
\section{Methods}{
  \describe{
    \item{autocovariances}{\code{signature(x = "PeriodicArmaModel")}: ... }
%%%     \item{PeriodicArModel}{\code{signature(object = "PeriodicArmaModel")}: ... }
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
%showClass("PeriodicArmaModel")
%}
\keyword{classes}
