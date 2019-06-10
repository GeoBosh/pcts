\name{PeriodicBJFilter-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicBJFilter-class}
\alias{PeriodicMonicFilterSpec-class}
%\alias{filterCoef,PeriodicBJFilter,character-method}

\title{Class PeriodicBJFilter}
\description{A class for filters following the Box-Jenkins sign convention}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicBJFilter", coef, order, ...)}.
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
Class \code{"\linkS4class{VirtualBJFilter}"}, directly.
Class \code{"\linkS4class{VirtualMonicFilterSpec}"}, by class "PeriodicMonicFilterSpec", distance 2.
Class \code{"\linkS4class{VirtualMonicFilter}"}, by class "VirtualBJFilter", distance 2.
}
\section{Methods}{
  \describe{
    \item{filterCoef}{\code{signature(object = "PeriodicBJFilter", convention = "character")}: ... }
	 
    \item{coerce}{\code{signature(from = "matrix", to = "PeriodicBJFilter")}: ... }
    \item{coerce}{\code{signature(from = "PeriodicBJFilter", to = "PeriodicSPFilter")}: ... }
    \item{coerce}{\code{signature(from = "PeriodicSPFilter", to = "PeriodicBJFilter")}: ... }
    \item{filterPoly}{\code{signature(object = "PeriodicBJFilter")}: ... }
    \item{filterPolyCoef}{\code{signature(object = "PeriodicBJFilter")}: ... }
    \item{show}{\code{signature(object = "PeriodicBJFilter")}: ... }}
}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\linkS4class{PeriodicSPFilter}}
}
\examples{
## a toy filter of order c(3, 3, 3, 3) and 4 seasons
co <- matrix(c(1, 1, 0,
               2, 2, 2,
               3, 0, 0,
               4, 4, 4), nrow = 4, ncol = 3)

## these are equivalent:
bj1 <- new("PeriodicBJFilter", coef = co)
bj1b <- new("PeriodicBJFilter", coef = co, order = 3)
bj1c <- new("PeriodicBJFilter", coef = co, order = c(3, 3, 3, 3))
identical(bj1b, bj1c) # TRUE
identical(bj1, bj1b) # FALSE but only because classbj1@order is "integer"

\dontshow{
    ## redundant wrap with show() below since pkgdown::build_site()
    ## throws errors without that.
}    
## a more refined spec. for the order:
show( new("PeriodicBJFilter", coef = co, order = c(2, 3, 1, 3)) )

## as()
show( as(co, "PeriodicBJFilter") )
show( as(co, "PeriodicSPFilter") )

## change the sign convention:
sp1 <- as(bj1, "PeriodicSPFilter")

## bj1 and sp1 represent the same filter
filterPolyCoef(bj1)
filterPolyCoef(sp1)

identical(filterPolyCoef(bj1), filterPolyCoef(sp1)) # TRUE

filterOrder(bj1)
nSeasons(bj1)

## filterPoly(bj1)
## filterPoly(sp1)
}
\keyword{classes}

