\name{PeriodicMaModel-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicMaModel-class}

\title{Class PeriodicMaModel}
\description{Class PeriodicMaModel.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicMaModel", ar, ma, sigma2, ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{sigma2}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ar}:}{Object of class \code{"PeriodicArFilter"} ~~ }
    \item{\code{ma}:}{Object of class \code{"PeriodicMaFilter"} ~~ }

    \item{\code{center}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intercept}:}{Object of class \code{"numeric"} ~~ }

    \item{\code{modelCycle}:}{Object of class \code{"BasicCycle"} ~~ }
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
    \item{show}{\code{signature(object = "PeriodicMaModel")}: ... }
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
%showClass("PeriodicMaModel")
%}
\keyword{classes}
