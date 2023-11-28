\name{PeriodicFilterModel-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicFilterModel-class}
%\alias{nSeasons,PeriodicFilterModel-method}

\title{Class PeriodicFilterModel}
\description{Class PeriodicFilterModel.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicFilterModel", pcmean, pcintercept, ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{center}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intercept}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sigma2}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ar}:}{Object of class \code{"PeriodicArFilter"} ~~ }
    \item{\code{ma}:}{Object of class \code{"PeriodicMaFilter"} ~~ }
  
    \item{\code{modelCycle}:}{Object of class \code{"BasicCycle"} ~~ }
}
}
\section{Extends}{
Class \code{"\linkS4class{VirtualPeriodicFilterModel}"}, directly.
Class \code{"\linkS4class{PeriodicArmaSpec}"}, directly.
Class \code{"\linkS4class{PeriodicArmaFilter}"}, by class "PeriodicArmaSpec", distance 2.
}
\section{Methods}{
  Functions with methods for this class:
  \describe{
    \item{show}{\code{signature(object = "PeriodicFilterModel")}: ... }
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
%showClass("PeriodicFilterModel")
%}
\keyword{classes}
