\name{pcts}
\docType{methods}
\alias{pcts}
\alias{pcts-methods}
\alias{pcts,ANY-method}
%\alias{pcts,ts-method}
%\alias{pcts,matrix-method}
%\alias{pcts,mts-method}
%\alias{pcts,numeric-method}
\alias{pcts,data.frame,ANY-method}
\alias{pcts,matrix,BasicCycle-method}
\alias{pcts,matrix,missing-method}
\alias{pcts,matrix,numeric-method}
\alias{pcts,mts,missing-method}
\alias{pcts,mts,numeric-method}
\alias{pcts,numeric,BasicCycle-method}
\alias{pcts,numeric,missing-method}
\alias{pcts,numeric,numeric-method}
\alias{pcts,ts,missing-method}
\alias{pcts,ts,numeric-method}
\alias{pcts,xtsORzoo,missing-method}
\title{Create objects from periodic time series classes}
\description{Create objects from periodic time series classes.}
\usage{
pcts(x, nseasons, start, \dots, keep = FALSE)
}
\arguments{
  \item{x}{a time series.}

  \item{nseasons}{

    number of seasons. This argument is ignored by some methods.

  }
  \item{start}{

    the starting time of the time series, can be a \code{(cycle,
      season)} pair or any object that can be converted to datetime.

  }
  \item{keep}{

    if \code{TRUE} and \code{x} is from class \code{"ts"},
    \code{"mts"}, \code{"zoo"}, or \code{"zooreg"}, create a periodic
    object inheriting from that class.

  }
  \item{\dots}{further arguments to be passed on to methods.}
}
\details{

  \code{pcts} creates periodic time series objects inheriting from
  \code{"PeriodicTimeSeries"}. The particular class depends on arguments
  \code{x} and, in some cases, \code{keep}.  The idea is that in normal
  use the user does not care about the particular class.

  There are also methods for \code{as} for conversion to and from the
  time series classes defined in package \pkg{pcts}.

}
\value{
  an object inheriting from \code{"PeriodicTimeSeries"},
  the defaults are \code{"PeriodicTS"} and \code{"PeriodicMTS"}
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\section{Methods}{

\describe{

  \item{\code{signature(x = "numeric", nseasons = "missing")}}{ }
  \item{\code{signature(x = "numeric", nseasons = "numeric")}}{ }
  \item{\code{signature(x = "numeric", nseasons = "BasicCycle")}}{ 

    Creates an object of class \code{"PeriodicTS"}, the native class for
    univariate periodic time series in package \pkg{"pcts"}.

  }
  
  \item{\code{signature(x = "matrix", nseasons = "missing")}}{ }
  \item{\code{signature(x = "matrix", nseasons = "numeric")}}{ }
  \item{\code{signature(x = "matrix", nseasons = "BasicCycle")}}{

    Creates an object of class \code{"PeriodicMTS"}, the native class
    for multivariate periodic time series in package \pkg{"pcts"}.

  }
  \item{\code{signature(x = "data.frame", nseasons = "ANY")}}{
    Currently this just calls \code{pcts(as.data.frame(x), nseasons, ...)}. 
  }
  
  \item{\code{signature(x = "ts", nseasons = "missing")}}{ }
  \item{\code{signature(x = "ts", nseasons = "numeric")}}{

    If \code{keep = TRUE} creates an object of class
    \code{"PeriodicTS_ts"}, otherwise the result is from
    \code{"PeriodicTS"}. The number of seasons is taken from the
    \code{"ts"} object. 

  }

  \item{\code{signature(x = "mts", nseasons = "missing")}}{ }
  \item{\code{signature(x = "mts", nseasons = "numeric")}}{

    If \code{keep = TRUE} creates an object of class
    \code{"PeriodicMTS_ts"}, otherwise the result is from
    \code{"PeriodicMTS"}. The number of seasons is taken from the
    \code{"ts"} object.

  }

  \item{\code{signature(x = "xtsORzoo", nseasons = "missing")}}{
    \code{x} needs to a regular time series, possibly with missing
    values for some times (technically, \code{zoo::is.regular(x)} should
    give TRUE). For daily time series, the cycle is taken to be day of
    week or a subcycle of it, most commonly Monday-Friday.

    The implementation is incomplete but for daily data should work as
    described.
    
  }
}}

\seealso{
  \code{\linkS4class{PeriodicTS}},

  \code{\link{dataFranses1996}} for further examples
}
\examples{
## convert a ts object, no need for further info
pcts(AirPassengers, 12)

## numeric
v24 <- rnorm(24)
pcts(v24, nseasons = 4)          # generic seasons
pcts(v24, nseasons = BuiltinCycle(4)) # Quarter/Year
ts1 <- pcts(v24, nseasons = BuiltinCycle(4), c(2006, 1)) # Quarter/Year with dates

## select subset of the seasons
window(ts1, seasons = 3:4)

## matrix, multivariate pcts
m24 <- matrix(v24, ncol =3)
colnames(m24) <- c("A", "B", "C")
pcts(m24, nseasons = 4)          # generic seasons
pcts(m24, nseasons = BuiltinCycle(4)) # Quarter/Year
mts1 <- pcts(m24, nseasons = BuiltinCycle(4), c(2006, 1)) # Quarter/Year with dates
mts1

## select subset of the seasons for mutivariate
window(mts1, seasons = 3:4)
}
\keyword{methods}
\keyword{pcts}
