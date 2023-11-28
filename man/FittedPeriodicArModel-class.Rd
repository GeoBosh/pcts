\name{FittedPeriodicArModel-class}
\Rdversion{1.1}
\docType{class}
\alias{FittedPeriodicArModel-class}

\title{Class FittedPeriodicArModel}
\description{Class FittedPeriodicArModel.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FittedPeriodicArModel", ar, ma, sigma2, ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{asyCov}:}{Object of class \code{"ANY"} ~~ }
    %\item{\code{const}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sigma2}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ar}:}{Object of class \code{"PeriodicArFilter"} ~~ }
    \item{\code{ma}:}{Object of class \code{"PeriodicMaFilter"} ~~ }

    \item{\code{center}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intercept}:}{Object of class \code{"numeric"} ~~ }

    \item{\code{theTS}:}{Object of class \code{"PeriodicTS"} ~~ }
    \item{\code{ns}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{modelCycle}:}{Object of class \code{"BasicCycle"} ~~ }
}
}
\section{Extends}{
Class \code{"\linkS4class{PeriodicArModel}"}, directly.
Class \code{"\linkS4class{PeriodicArmaModel}"}, by class "PeriodicArModel", distance 2.
Class \code{"\linkS4class{VirtualPeriodicArmaModel}"}, by class "PeriodicArModel", distance 3.
Class \code{"\linkS4class{PeriodicArmaSpec}"}, by class "PeriodicArModel", distance 3.
Class \code{"\linkS4class{VirtualPeriodicFilterModel}"}, by class "PeriodicArModel", distance 4.
Class \code{"\linkS4class{VirtualPeriodicStationaryModel}"}, by class "PeriodicArModel", distance 4.
Class \code{"\linkS4class{VirtualPeriodicAutocovarianceModel}"}, by class "PeriodicArModel", distance 5.
Class \code{"\linkS4class{VirtualPeriodicMeanModel}"}, by class "PeriodicArModel", distance 5.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "FittedPeriodicArModel")}: ... }
    \item{summary}{\code{signature(object = "FittedPeriodicArModel")}: ... }
    \item{as_pcarma_list}{\code{signature(object = "FittedPeriodicArModel")}: ... }
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
%showClass("FittedPeriodicArModel")
%}
\keyword{classes}
