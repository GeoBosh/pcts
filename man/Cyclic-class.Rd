\name{Cyclic-class}
\Rdversion{1.1}
\docType{class}
\alias{Cyclic-class}
%\alias{nSeasons,Cyclic-method}
%\alias{pcCycle,Cyclic-method}

\title{Class \code{"Cyclic"}}
\description{Class \code{"Cyclic"}}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Cyclic", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{cycle}:}{Object of class \code{"BasicCycle"} ~~ }
    \item{\code{pcstart}:}{Object of class \code{"ANY"} ~~ }
}
}
\section{Methods}{
  \describe{
%%%    \item{nSeasons}{\code{signature(object = "Cyclic")}: ... }
%%%    \item{pcCycle}{\code{signature(x = "Cyclic")}: ... }
	 
    \item{nSeasons}{\code{signature(object = "Cyclic")}: ... }
    \item{pcCycle}{\code{signature(x = "Cyclic", type = "ANY")}: ... }
    \item{allSeasons}{\code{signature(x = "Cyclic", abb = "ANY")}: ... }
    \item{allSeasons<-}{\code{signature(x = "Cyclic")}: ... }
    \item{seqSeasons}{\code{signature(x = "Cyclic")}: ... }
    \item{unitCycle}{\code{signature(x = "Cyclic")}: ... }
    \item{unitCycle<-}{\code{signature(x = "Cyclic")}: ... }
    \item{unitSeason}{\code{signature(x = "Cyclic")}: ... }
    \item{unitSeason<-}{\code{signature(x = "Cyclic")}: ... }}
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
\examples{
showClass("Cyclic")
}
\keyword{classes}
