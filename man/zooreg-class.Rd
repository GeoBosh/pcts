\name{zooreg-class}
\Rdversion{1.1}
\docType{class}
\alias{zooreg-class}
\title{Virtual S4 class zooreg}
\description{Virtual S4 class zooreg.}
\section{
  Objects from the Class}{A virtual Class: No objects may be created
  from it.

  S4 Class \code{"zooreg"} is derived from its namesake in package
  \pkg{zoo} for use as a super class for periodic time series classes
  and in S4 method signatures.

}
\section{Slots}{
  \describe{
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{zoo}"}, directly.
Class \code{"\linkS4class{oldClass}"}, by class "zoo", distance 2.
}
\section{Methods}{
No methods defined with class "zooreg" in the signature.
}
\seealso{
    \code{\linkS4class{PeriodicTS_zooreg}},
    \code{\linkS4class{zoo}}
    and package \pkg{zoo}
}
\examples{
showClass("zooreg")
}
\keyword{classes}
