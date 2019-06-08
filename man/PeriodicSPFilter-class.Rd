\name{PeriodicSPFilter-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicSPFilter-class}
%\alias{filterCoef,PeriodicSPFilter,character-method}

\title{Class PeriodicSPFilter}
\description{A class for filters following the signal processing sign convention.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicSPFilter", coef, order, ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{coef}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{order}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{PeriodicMonicFilterSpec}"}, directly.
Class \code{"\linkS4class{VirtualSPFilter}"}, directly.
Class \code{"\linkS4class{VirtualMonicFilterSpec}"}, by class "PeriodicMonicFilterSpec", distance 2.
Class \code{"\linkS4class{VirtualMonicFilter}"}, by class "VirtualSPFilter", distance 2.
}
\section{Methods}{
  \describe{
    \item{filterCoef}{\code{signature(object = "PeriodicSPFilter", convention = "character")}: ... }
	 }
}
%\references{
%%%  ~~put references to the literature/web site here~~
%}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\linkS4class{PeriodicBJFilter}}
}
\examples{
## see "PeriodicBJFilter-class" for examples
}
\keyword{classes}
