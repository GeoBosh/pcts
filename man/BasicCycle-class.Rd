\name{BasicCycle-class}
\Rdversion{1.1}
\docType{class}
\alias{BasicCycle-class}

\title{Class BasicCycle}
\description{

  Virtual class \code{"BasicCycle"} is a class union that can be used in
  signatures of methods and classes when any of the cycle classes is
  admissible as argument or slot.

}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "BasicCycle", i = "ANY", j = "missing", drop = "ANY")} }
    \item{[}{\code{signature(x = "BasicCycle", i = "missing", j = "missing", drop = "ANY")} }
    \item{[<-}{\code{signature(x = "BasicCycle", i = "ANY", j = "missing", value = "ANY")} }
    \item{[<-}{\code{signature(x = "BasicCycle", i = "missing", j = "missing", value = "ANY")} }
    \item{date<-}{\code{signature(x = "BasicCycle")}: ... }

    \item{allSeasons}{\code{signature(x = "BasicCycle", abb = "ANY")} }
    \item{seqSeasons}{\code{signature(x = "BasicCycle")} }
  
    \item{pcCycle}{\code{signature(x = "BasicCycle", type = "character")}: ... }
    \item{pcCycle}{\code{signature(x = "BasicCycle", type = "missing")}: ... }
    \item{pcts}{\code{signature(x = "matrix", nseasons = "BasicCycle")}: ... }
    \item{pcts}{\code{signature(x = "numeric", nseasons = "BasicCycle")}: ... }
  }
}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\link{BareCycle-class}} (just number of seasons),
  
  \code{\link{SimpleCycle-class}} (named seasons),
  
  \code{\link{BuiltinCycle-class}} (common cycles, e.g.,
  \code{\link{DayWeekCycle-class}},
  \code{\link{MonthYearCycle-class}},
  \code{\link{OpenCloseCycle-class}},
  \code{\link{QuarterYearCycle-class}}),

  \code{\link{PartialCycle-class}} (cycles obtained from others by
  subsetting or otherwise)
}
\examples{
showClass("BasicCycle")
}
\keyword{classes}
