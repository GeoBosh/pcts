\name{autocorrelations-methods}
\docType{methods}
\alias{autocorrelations}
\alias{autocorrelations-methods}
\alias{autocorrelations,numeric,ANY,missing-method}
\alias{autocorrelations,PeriodicTimeSeries,ANY,missing-method}
\alias{autocorrelations,PeriodicAutocovariances,ANY,missing-method}
\alias{autocorrelations,SamplePeriodicAutocovariances,ANY,missing-method}
\alias{autocorrelations,VirtualPeriodicAutocovariances,ANY,missing-method}
\alias{autocorrelations,VirtualPeriodicAutocovarianceModel,ANY,missing-method}

\title{Compute autocorrelations and periodic autocorrelations}
\description{

  Methods for computation of autocorrelations and periodic
  autocorrelations.

}
\section{Methods}{
  \describe{
  \item{\code{signature(x = "numeric", maxlag = "ANY", lag_0 = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "PeriodicTimeSeries", maxlag = "ANY", lag_0 = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "PeriodicAutocovariances", maxlag = "ANY", 
          lag_0 = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "SamplePeriodicAutocovariances", maxlag = "ANY", 
          lag_0 = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "VirtualPeriodicAutocovariances", maxlag = "ANY", 
          lag_0 = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "VirtualPeriodicAutocovarianceModel", maxlag = "ANY", 
          lag_0 = "missing")}}{{
%%  ~~describe this method here~~
}
}}
}
\seealso{
  \code{\link[sarima]{autocorrelations}} in package \pkg{sarima}
  for further details.
  
  \code{\link{autocovariances}} for autocovariances; 
}
\examples{
## periodic ts object => peridic acf
autocorrelations(pcts(AirPassengers), maxlag = 10)

## for "ts" or "numeric" objects the default is non-periodic acf
autocorrelations(AirPassengers, maxlag = 10) 
autocorrelations(as.numeric(AirPassengers))
## argument 'nseasons' forces periodic acf
autocorrelations(AirPassengers, maxlag = 10, nseasons = 12)
autocorrelations(as.numeric(AirPassengers), maxlag = 10, nseasons = 12)
}
\keyword{methods}
\keyword{ts}
\keyword{acf}
