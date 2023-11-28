\name{maxLag-methods}
\docType{methods}
\alias{maxLag-methods}
\alias{maxLag,PeriodicArmaFilter-method}


\title{Methods for function maxLag() in package 'pcts'}
\description{Methods for function maxLag() in package 'pcts'.}
\section{Methods}{
  \describe{

    \item{\code{signature(object = "PeriodicArmaFilter")}}{{
	%%  ~~describe this method here~~
      }
      
    }
  }
}
\examples{
## non-periodic autocovariances
maxLag(autocovariances(AirPassengers))

## periodic
pcts_exdata() # creates ap, ap7to9, pcfr, pcfr2to4,

maxLag(autocovariances(ap, maxlag = 6))

## pcarma filter
m <- rbind(c(0.81, 0), c(0.4972376, 0.4972376))
ar_filt3 <- new("PeriodicBJFilter",  coef =  m, order = c(1,2))
arma_filt3 <- new("PeriodicArmaFilter", ar = ar_filt3)
maxLag(arma_filt3)
}
\keyword{methods}
