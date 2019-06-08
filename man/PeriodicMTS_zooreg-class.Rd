\name{PeriodicMTS_zooreg-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicMTS_zooreg-class}

\title{Class \code{"PeriodicMTS_zooreg"}}
\description{

  Class \code{"PeriodicMTS_zooreg"} is a periodic class holding
  multivariate \code{"zooreg"} objects.

}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicMTS_zooreg", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{cycle}:}{Object of class \code{"BasicCycle"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
 
    \item{\code{pcstart}:}{Object of class \code{"ANY"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{PeriodicTimeSeries}"}, directly.
Class \code{"\linkS4class{ts}"}, directly.
Class \code{"\linkS4class{Cyclic}"}, by class "PeriodicTimeSeries", distance 2.
Class \code{"\linkS4class{structure}"}, by class "ts", distance 2.
Class \code{"\linkS4class{oldClass}"}, by class "ts", distance 2.
Class \code{"\linkS4class{AnyTimeSeries}"}, by class "ts", distance 2.
Class \code{"\linkS4class{vector}"}, by class "ts", distance 3, with explicit coerce.
}
\section{Methods}{
No methods defined with class "PeriodicMTS_zooreg" in the signature.
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
%showClass("PeriodicMTS_zooreg")
%}
\keyword{classes}
