\name{PeriodicTS_ts-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicTS_ts-class}

\title{Class \code{"PeriodicTS_ts"}}
\description{

  Class \code{"PeriodicTS_ts"} is a periodic class holding
  \code{"ts"} objects.

}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("PeriodicTS_zooreg", ...)}.
%%  ~~ describe objects here ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicTS_ts", x, ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"vector"} ~~ }
    \item{\code{cycle}:}{Object of class \code{"BasicCycle"} ~~ }
    \item{\code{tsp}:}{Object of class \code{"numeric"} ~~ }
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
Class \code{"\linkS4class{vector}"}, by class "ts", distance 3, with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "ts", to = "PeriodicTS_ts")}: ... }
    \item{initialize}{\code{signature(.Object = "PeriodicTS_ts")}: ...}
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
%showClass("PeriodicTS_ts")
%}
\keyword{classes}
