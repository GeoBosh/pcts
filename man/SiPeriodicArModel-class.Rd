\name{SiPeriodicArModel-class}
\Rdversion{1.1}
\docType{class}
\alias{SiPeriodicArModel-class}

\title{Class SiPeriodicArModel}
\description{Class SiPeriodicArModel.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SiPeriodicArModel", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{iorder}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{siorder}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{pcmodel}:}{Object of class \code{"PeriodicArmaModel"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SiPeriodicArmaModel}"}, directly.
Class \code{"\linkS4class{VirtualPeriodicFilterModel}"}, by class "SiPeriodicArmaModel", distance 2.
Class \code{"\linkS4class{PeriodicIntegratedArmaSpec}"}, by class "SiPeriodicArmaModel", distance 2.
}
\section{Methods}{
  \describe{
    \item{fitPM}{\code{signature(model = "SiPeriodicArModel", x = "ANY")}: ... }
    \item{show}{\code{signature(object = "SiPeriodicArModel")}: ... }
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
%showClass("SiPeriodicArModel")
%}
\keyword{classes}
