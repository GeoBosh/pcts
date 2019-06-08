\name{PeriodicTimeSeries-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicTimeSeries-class}
%\alias{pcTest,PeriodicTimeSeries,character-method}

\title{Class PeriodicTimeSeries}
\description{Class PeriodicTimeSeries.}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{cycle}:}{Object of class \code{"BasicCycle"}.}
    \item{\code{pcstart}:}{Object of class \code{"ANY"}.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{Cyclic}"}, directly.
}
\section{Methods}{
  \describe{
    \item{pcTest}{\code{signature(x = "PeriodicTimeSeries", nullmodel = "character")}: ... }

    \item{head}{\code{signature(x = "PeriodicTimeSeries")}: ... }
    \item{pcCycle}{\code{signature(x = "PeriodicTimeSeries", type = "ANY")}: ... }
    \item{tail}{\code{signature(x = "PeriodicTimeSeries")}: ... }}}
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
%showClass("PeriodicTimeSeries")
%}
\keyword{classes}
