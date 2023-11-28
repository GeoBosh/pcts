\name{PeriodicVector-class}
\Rdversion{1.1}
\docType{class}
\alias{PeriodicVector-class}
\alias{PeriodicVector}
%\alias{[,PeriodicVector,ANY,ANY,ANY-method}
%\alias{[,PeriodicVector,missing,ANY,ANY-method}
%\alias{[,PeriodicVector,missing,missing,ANY-method}
%\alias{[<-,PeriodicVector,ANY,ANY,ANY-method}
%\alias{[<-,PeriodicVector,missing,ANY,ANY-method}
\title{Class PeriodicVector}
\description{Objects and methods for class PeriodicVector.}
\usage{
PeriodicVector(x, period = length(x))
}
\arguments{
  \item{x}{the values for inidices from 1 to \code{period}, numeric.}
  \item{period}{the period, defaults to \code{length(x)}.}
}
\details{

  A \eqn{p}-periodic vector, \eqn{X}, is such that \eqn{X_{i+pk} = X_i}
  for any integers \eqn{i,k}.

  Class \code{PeriodicVector} stores the values of \eqn{X_1,\ldots,X_p}
  and provides indexing methods for extracting and setting its
  elements.

}
\value{
  an object from class \code{"PeriodicVector"}
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PeriodicVector", ...)}
or more conveniently by using \code{"PeriodicVector()"}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{period}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{numeric}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{atomicVector}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{index}"}, by class "numeric", distance 2.

% 2016-10-20: do not link since numLike is not exported and R CMD check
% gives warning:
%      * checking Rd cross-references ... WARNING
%      Missing link or links in documentation object 'LagVector-class.Rd':
%        'numLike-class'
%Class \code{"numLike"}, by class "numeric", distance 2.
Class \code{"numLike"}, by class "numeric", distance 2.

Class \code{"\linkS4class{number}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{replValue}"}, by class "numeric", distance 2.
}
\section{Methods}{
  \code{"PeriodicVector"} methods are defined for \code{"["} and
  \code{"[<-"}. Arithmetic operations just inherit the recycling rules
  from \code{"numeric"}.
  \describe{
    \item{[}{\code{signature(x = "PeriodicVector", i = "ANY", j = "ANY", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicVector", i = "ANY", j = "missing", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicVector", i = "missing", j = "ANY", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "PeriodicVector", i = "missing", j = "missing", drop = "ANY")}: ... }

    \item{[<-}{\code{signature(x = "PeriodicVector", i = "ANY", j = "ANY", value = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "PeriodicVector", i = "missing", j = "ANY", value = "ANY")}: ... }
  }
}
%\references{
%%%  ~~put references to the literature/web site here~~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{PeriodicVector}}
}
\examples{
PeriodicVector(1:4, period = 4)
PeriodicVector(1:4) ## same
new("PeriodicVector", 1:4, period = 4)

## if period is given but x is missing, the vector is filled with NA's
PeriodicVector(period = 4)

## this throws error, since length(x) != period:
##    PeriodicVector(1:3, period = 4)

## extract
x <- PeriodicVector(1:4)
x[3:12]
x[c(3, 7, 11, 15)]

# any indices in (-Inf, Inf) work
x[0]
x[-3:0]

## "[<-" works on the underling vector
x[1] <- 11; x

## modulo indexing works also in assignments:
x[5] <- 21; x

## empty index returns the underlying vector
x[]

## the recycling rule applies on assignment
x[] <- 9; x
x[] <- 1:2; x

## this gives warning, as for numeric vectors
##     x[] <- 8:1
## compare:
##     x <- 1:4
##     x[] <- 8:1

## arithmetic works as usual:
2 * x
x + 1:4
## x + 1:3 # warning - '... a multiple ...'
}
\keyword{classes}
