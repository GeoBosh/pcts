\name{pcMean-methods}
\docType{methods}

\alias{pcMean}
\alias{pcMean-methods}
\alias{pcMean,matrix-method}
\alias{pcMean,numeric-method}
\alias{pcMean,PeriodicMTS-method}
\alias{pcMean,PeriodicTS-method}
\alias{pcMean,VirtualPeriodicArmaModel-method}

\title{Compute periodic mean}
\description{
  Compute periodic mean, generic function.
}
\usage{
pcMean(object, \dots)

\S4method{pcMean}{numeric}(object, nseasons, \dots)

\S4method{pcMean}{matrix}(object, nseasons, \dots)

\S4method{pcMean}{PeriodicTS}(object, \dots)

\S4method{pcMean}{PeriodicMTS}(object, \dots)
}
\arguments{
  \item{object}{an object for which periodic mean makes sense.}
  \item{nseasons}{number of seasons.}
  \item{\dots}{further arguments for methods.}
}

\details{

  For univariate periodic time series, \code{pcMean} computes the mean
  for each season and returns a named vector. For multivariate periodic
  time series, the result is a matrix with one column for each variable.

  The methods for \code{"numeric"} and \code{"matrix"} are equivalent to
  those for \code{"PeriodicTS"} and \code{"PeriodicMTS"},
  respectively. The difference is that the latter two don't need
  argument \code{nseasons} and take the names of the seasons from
  \code{object}.

  Argument \code{na.rm = TRUE} can be used to omit NA's.

  In the univariate case, when  \code{length(object)} is an integer
  multiple of the number of seasons the periodic mean is equivalent to
  computing the row means of \code{matrix(object, nrow = nseasons)}.
  
}
\value{
  numeric or matrix for the methods described here, see section
  \sQuote{Details}.
}
\author{Georgi N. Boshnakov}
\section{Methods}{
  \describe{

    \item{\code{signature(object = "matrix")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(object = "numeric")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(object = "PeriodicMTS")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(object = "PeriodicTS")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(object = "VirtualPeriodicArmaModel")}}{
      %%  ~~describe this method here~~
    }
  }
}
\seealso{
  \code{\link{pcApply}} which applies an arbitrary function by season
}
\examples{
pcMean(pcts(presidents))
pcMean(pcts(presidents), na.rm = TRUE)

pcMean(pcts(dataFranses1996)[2:5], na.rm = TRUE)

pcMean(1:20, nseasons = 4)
m <- matrix(1:20, nrow = 4)
all(apply(m, 1, mean) == pcMean(1:20, nseasons = 4)) # TRUE
}
\keyword{methods}
\keyword{ts}
