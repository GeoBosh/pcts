\name{PeriodicAutocorrelations-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicAutocorrelations-class}
\alias{plot,PeriodicAutocorrelations,missing-method}

\title{Class PeriodicAutocorrelations}
\description{Class PeriodicAutocorrelations.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicAutocorrelations", ..., data)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{modelCycle}:}{Object of class \code{"BasicCycle"} ~~ }
    \item{\code{data}:}{Object of class \code{"Lagged"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{ModelCycleSpec}"}, directly.
Class \code{"\linkS4class{FlexibleLagged}"}, directly.
Class \code{"\linkS4class{VirtualPeriodicAutocorrelations}"}, directly.
Class \code{"\linkS4class{Lagged}"}, by class "FlexibleLagged", distance 2.
Class \code{"\linkS4class{VirtualPeriodicModel}"}, by class "VirtualPeriodicAutocorrelations", distance 2.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "PeriodicAutocorrelations", y = "missing")}: ... }
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
%showClass("PeriodicAutocorrelations")
%}
\keyword{classes}
