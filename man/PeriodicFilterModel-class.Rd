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
    %\item{\code{const}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{center}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intercept}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sigma2}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ar}:}{Object of class \code{"PeriodicArFilter"} ~~ }
    \item{\code{ma}:}{Object of class \code{"PeriodicMaFilter"} ~~ }
}
}
\section{Extends}{
Class \code{"\linkS4class{VirtualPeriodicFilterModel}"}, directly.
Class \code{"\linkS4class{PeriodicArmaSpec}"}, directly.
Class \code{"\linkS4class{PeriodicArmaFilter}"}, by class "PeriodicArmaSpec", distance 2.
}
% \section{Methods}{
% %%%   \describe{
% %%%     \item{nSeasons}{\code{signature(object = "PeriodicFilterModel")}: ... }
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
%showClass("PeriodicFilterModel")
%}
\keyword{classes}
