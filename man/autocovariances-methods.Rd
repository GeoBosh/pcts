\name{autocovariances-methods}
\docType{methods}
\alias{autocovariances}
\alias{autocovariances-methods}
%%%% 2019-05-11 commenting out methods defined elsewhere (i,e, in
%%%% package 'sarima'
%%
%% \alias{autocovariances,ANY,ANY-method}
%% \alias{autocovariances,Autocovariances,ANY-method}
%% \alias{autocovariances,Autocovariances,missing-method}

%%%% In R-3.5.2 manually removed the trailing ',ANY' from the following
%%%%       methods.  showMethods() doesn't drop those but there was some
%%%%       discussion on R-devel revealing that such trailing ANY's are
%%%%       dropped in certain circumstances.  Note that:
%%%%
%%%%          (1) the signatures in section 'Methods' do not drop ANY.
%%%%          (2) promptMethods() doesn't drop ANY from the aliases
%%%%          (3) promptMethods() doesn't drop ANY from section 'Methods'
%%%% 
%%%% No longer needed with the release of R-3.6.0, don't know if other R
%%%%       versions in the 3.5.x series were affected by this.
%%%%       BUT now R-devel (devel of 3.7.0) issues the warning!
%%%%
%%%% Leaving both variants below - no complaints from r CMD check; for now ...
\alias{autocovariances,matrix,ANY-method}
\alias{autocovariances,numeric,ANY-method}
\alias{autocovariances,PeriodicArmaModel,ANY-method}
\alias{autocovariances,PeriodicArModel,ANY-method}

\alias{autocovariances,matrix-method}
\alias{autocovariances,numeric-method}
\alias{autocovariances,PeriodicArmaModel-method}
\alias{autocovariances,PeriodicArModel-method}
\alias{autocovariances,PeriodicTS,ANY-method}
\alias{autocovariances,VirtualPeriodicAutocovariances,ANY-method}

%% \alias{autocovariances,VirtualArmaModel,ANY-method}
%% \alias{autocovariances,VirtualAutocovariances,ANY-method}

\title{Compute autocovariances and periodic autocovariances}
\description{

  Methods for the generic function \code{autocovariances()}, which
  computes autocovariances meaningful for the first argument. For
  objects representing time series, it computes sample autocovariances
  (univariate, multivariate, periodic, as appropriate). For objects
  representing models, it computes the relevant theoretical
  autocovariances.

}
\section{Methods}{
\describe{\item{\code{signature(x = "matrix", maxlag = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "numeric", maxlag = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "PeriodicArmaModel", maxlag = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "PeriodicArModel", maxlag = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "PeriodicAutocovarianceModel", maxlag = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "PeriodicTS", maxlag = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "VirtualPeriodicAutocovariances", maxlag = "ANY")}}{{

    If \code{maxlag} is missing or equal to \code{maxLag(x)}, \code{x}
    is returned unchanged. Otherwise the number of available lags is
    adjusted to \code{maxlag}.

}
}}}
\seealso{
  \code{\link[sarima]{autocovariances}} in package \pkg{sarima}
  for further details.
  
  \code{\link{autocorrelations}} for autocorrelations; 

}
\examples{
## periodic ts object => peridic acvf
autocovariances(pcts(AirPassengers), maxlag = 10)

## for "ts" or "numeric" objects the default is non-periodic acvf
autocovariances(AirPassengers, maxlag = 10) 
autocovariances(as.numeric(AirPassengers))
## argument 'nseasons' forces periodic acvf
autocovariances(AirPassengers, maxlag = 10, nseasons = 12)
autocovariances(as.numeric(AirPassengers), maxlag = 10, nseasons = 12)
}
\keyword{methods}
\keyword{ts}
\keyword{acf}

