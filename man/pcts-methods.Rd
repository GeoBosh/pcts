\name{pcts}
\docType{methods}
\alias{pcts}
\alias{pcts-methods}
\alias{pcts,ANY-method}
\alias{pcts,ts-method}
\alias{pcts,matrix-method}
\alias{pcts,mts-method}
\alias{pcts,numeric-method}
\title{Create objects from periodic time series classes}
\description{Create objects from periodic time series classes.}
\usage{
pcts(x, nseasons, keep, ...)
}
\arguments{
  \item{x}{a time series.}

  \item{nseasons}{

    number of seasons. This argument is ignored by some methods.

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

  TODO: more methods are needed.

}
\value{
  an object inheriting from \code{"PeriodicTimeSeries"}
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

  TODO: need methods for zoo and zooreg

\describe{

  \item{\code{signature(x = "numeric")}}{

    Creates an object of class \code{"PeriodicTS"}, the native class for
    univariate periodic time series in package \pkg{"pcts"}.

  }
  \item{\code{signature(x = "matrix")}}{

    Creates an object of class \code{"PeriodicMTS"}, the native class
    for multivariate periodic time series in package \pkg{"pcts"}.

  }
  \item{\code{signature(x = "ts")}}{

    If \code{keep = TRUE} creates an object of class
    \code{"PeriodicTS_ts"}, otherwise the result is from
    \code{"PeriodicTS"}.

  }
  \item{\code{signature(x = "mts")}}{

    If \code{keep = TRUE} creates an object of class
    \code{"PeriodicMTS_ts"}, otherwise the result is from
    \code{"PeriodicMTS"}.
  }
}

}

\seealso{
  \code{\linkS4class{PeriodicTS}}
}
\examples{
pcts(AirPassengers,12)
}
\keyword{methods}
\keyword{pcts}



