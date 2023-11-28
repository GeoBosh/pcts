\name{PeriodicArmaSpec-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicArmaSpec-class}
\alias{innovationVariances,PeriodicArmaSpec-method}

\title{Class PeriodicArmaSpec}
\description{Class PeriodicArmaSpec.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicArmaSpec", pcmean, pcintercept, ...)}.
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
  Class \code{"\linkS4class{PeriodicArmaFilter}"}, directly.
}
\section{Methods}{Functions with methods for this class:
  \describe{
    \item{initialize}{\code{signature(.Object = "PeriodicArmaSpec")}: ... }
    \item{show}{\code{signature(object = "PeriodicArmaSpec")}: ... }
  }
}
% \section{Methods}{
% %%%   \describe{
% %%%     \item{innovationVariances}{\code{signature(object = "PeriodicArmaSpec")}: ... }
% %%% 	 }
% }
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
%showClass("PeriodicArmaSpec")
%}
\keyword{classes}
