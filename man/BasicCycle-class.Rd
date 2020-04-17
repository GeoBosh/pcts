\name{BasicCycle-class}
\Rdversion{1.1}
\docType{class}
\alias{BasicCycle-class}

\title{Class BasicCycle}
\description{Class BasicCycle.}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "BasicCycle", i = "ANY", j = "missing", drop = "ANY")} }
    \item{[}{\code{signature(x = "BasicCycle", i = "missing", j = "missing", drop = "ANY")} }
    \item{[<-}{\code{signature(x = "BasicCycle", i = "ANY", j = "missing", value = "ANY")} }
    \item{[<-}{\code{signature(x = "BasicCycle", i = "missing", j = "missing", value = "ANY")} }
    \item{allSeasons}{\code{signature(x = "BasicCycle", abb = "ANY")} }
    \item{seqSeasons}{\code{signature(x = "BasicCycle")} }
  }
}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\link{BareCycle-class}},
  \code{\link{SimpleCycle-class}},
  \code{\link{BuiltinCycle-class}}

  \code{\link{DayWeekCycle-class}},
  \code{\link{MonthYearCycle-class}},
  \code{\link{OpenCloseCycle-class}},
  \code{\link{QuarterYearCycle-class}},
  \code{\link{PartialCycle-class}}
  %%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("BasicCycle")
}
\keyword{classes}
