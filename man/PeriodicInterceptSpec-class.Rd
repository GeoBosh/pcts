\name{PeriodicInterceptSpec-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicInterceptSpec-class}

\title{Class PeriodicInterceptSpec}
\description{Class PeriodicInterceptSpec.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicInterceptSpec", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    %\item{\code{.Data}:}{Object of class \code{"numeric"} ~~ }

    \item{\code{center}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intercept}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sigma2}:}{Object of class \code{"numeric"} ~~ }

    \item{\code{modelCycle}:}{Object of class \code{"BasicCycle"} ~~ }
}
}
\section{Extends}{
Class \code{"\linkS4class{numeric}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{index}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{replValue}"}, by class "numeric", distance 2.
% 2016-10-20: do not link since numLike is not exported and R CMD check
% gives warning:
%      * checking Rd cross-references ... WARNING
%      Missing link or links in documentation object 'LagVector-class.Rd':
%        'numLike-class'
%Class \code{"\linkS4class{numLike}"}, by class "numeric", distance 2.
Class \code{"numLike"}, by class "numeric", distance 2.
Class \code{"\linkS4class{number}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{atomicVector}"}, by class "numeric", distance 2.
}
\section{Methods}{
No methods defined with class "PeriodicInterceptSpec" in the signature.
\describe{
    \item{sigmaSq}{\code{signature(object = "PeriodicInterceptSpec")}: ... }
    \item{allSeasons}{\code{signature(x = "PeriodicInterceptSpec", abb = "ANY")}: ... }
    \item{initialize}{\code{signature(.Object = "PeriodicInterceptSpec")}: ... }
    \item{nSeasons}{\code{signature(object = "PeriodicInterceptSpec")}: ... }
    \item{show}{\code{signature(object = "PeriodicInterceptSpec")}: ... }}}
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
%%%  ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
%\examples{
%showClass("PeriodicInterceptSpec")
%}
\keyword{classes}
