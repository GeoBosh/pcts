\name{PiPeriodicArModel-class}
\Rdversion{1.1}
\docType{class}
\alias{PiPeriodicArModel-class}
%\alias{fitPM,ANY,PiPeriodicArModel-method}

\title{Class PiPeriodicArModel}
\description{Class PiPeriodicArModel.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PiPeriodicArModel", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{piorder}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{picoef}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{pcmodel}:}{Object of class \code{"PeriodicArmaModel"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{PiPeriodicArmaModel}"}, directly.
Class \code{"\linkS4class{VirtualPeriodicFilterModel}"}, by class "PiPeriodicArmaModel", distance 2.
Class \code{"\linkS4class{PeriodicIntegratedArmaSpec}"}, by class "PiPeriodicArmaModel", distance 2.
}
\section{Methods}{
  \describe{
    \item{fitPM}{\code{signature(x = "ANY", model = "PiPeriodicArModel")}: ... }
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
%showClass("PiPeriodicArModel")
%}
\keyword{classes}
