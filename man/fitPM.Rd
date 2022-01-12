\name{fitPM}
\docType{methods}
\alias{fitPM}
\alias{fitPM-methods}
\alias{fitPM,ANY,ANY-method}
\alias{fitPM,mcSpec,ANY-method}
\alias{fitPM,numeric,ANY-method}
\alias{fitPM,PeriodicArModel,ANY-method}
\alias{fitPM,PeriodicArModel,PeriodicMTS-method}
\alias{fitPM,PeriodicArModel,PeriodicTS-method}
\alias{fitPM,PiPeriodicArModel,ANY-method}
\alias{fitPM,SiPeriodicArModel,ANY-method}
\title{Fit periodic time series models}
\description{
  Generic function with methods for fitting periodic time series models.
}
\usage{
fitPM( model, x, ...)
}
\arguments{
  \item{x}{the time series.}
  \item{model}{a periodic model, see Details.}
  \item{\dots}{further arguments to be passed on to individual methods.}
}
\details{

  This is a generic function.

  \code{model} provides the specification of the model. In particular,
  the class of \code{model} determines what model is fitted.  Specific
  values of the parameters are generally ignored by non-iterative
  methods but some methods can handle more detailed specifications, see
  the individual methods.

}
\value{
  the fitted model,
  typically an object of class \code{class(model)}
}
\section{Methods}{
\describe{

\item{\code{signature(model = "ANY", x = "ANY")}}{
  This is the default method. It simply exits with an error message
  stating that \code{fitPM} does not have a method for the model
  specified by \code{model}.
}

\item{\code{signature(model = "numeric", x = "ANY")}}{
  Fits a PAR model to \code{x}. \code{model} should be a vector of
  non-negative integers giving the PAR order. The length of this vector
  is taken to be the number of seasons.

  This is a convenience method. It constructs a PAR model and callls the
  method for \code{model = "PeriodicArModel"}.
}

\item{\code{signature(model = "PeriodicArModel", x = "ANY")}}{
  Fits a PAR model.
}

\item{\code{signature(model = "mcSpec", x = "ANY")}}{
    Fits a periodic model according to the specification given by
    \code{model}.

    Currently this method uses \code{\link{mC.ss}} to set up the
    optimisation environment and then calls one of the optimisation
    functions in that environment as specified by argument
    \code{optim.method}, see below.

    Additional arguments may be specified to control the optimisation.

    Argument \code{init} can be used to give initial values. It is
    passed on to \code{mC.ss} (and so has the format required by it).

    \code{optim.method} is the name of an optimisation function in the
    environment returned by \code{mC.ss}. The default is
    \code{optim.method = "minim"}, which is based on the standard \R function
    \code{optim}.  Alternatives are "minimBB" or "minimBBLU". All this
    needs to be documented but see \code{mC.ss} and \code{xx.ss} for
    details.

    Further arguments are passed on to the optimisation method. A
    typical argument supported by most optimisation functions is
    \code{control}.
}
\item{\code{signature(model = "PiPeriodicArModel", x = "ANY")}}{{
        Fits a periodically integrated PAR model using the parameters of
    \code{model} as initial values. Calls \code{\link{pclspiar}} to do the
    actual work.
}
}\item{\code{signature(model = "SiPeriodicArModel", x = "ANY")}}{{

  Fits a seasonally integrated PAR model.

}
}
\item{\code{signature(model = "PeriodicArModel", x = "PeriodicMTS")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(model = "PeriodicArModel", x = "PeriodicTS")}}{{
%%  ~~describe this method here~~
}
}}}
\references{ (todo: to be completed properly later)
  \insertRef{HipelMcLeod}{pcts}
  
  \insertRef{boshnakov2009gen}{pcts}
}
\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{PeriodicArModel}},
%\code{\link{SiParModel}}
%}
\examples{
## newm1 <- list(phi = matrix(1:12, nrow=4), p=rep(3,4), period=4, si2 = rep(1,4))
## new_pfm1 <- PeriodicFilterModel(newm1, intercept=0)

## generate some data;
set.seed(1234)
simts1 <- pcts(rnorm(1024), nseasons = 4)

fitPM(c(3,3,3,3), simts1)
fitPM(3, simts1)
## the fit on the underlying data is equivalent.
fitPM(c(3,3,3,3), as.numeric(simts1))

## equivalently, use a PAR(3,3,3,3) model for argument 'model'
## here the coefficients of pfm1 are ignored, since the estimation is linear.
pfm1 <- PeriodicArModel(matrix(1:12, nrow = 4), order = rep(3,4), sigma2 = 1)
pfm1
## these give same results as above
fitPM(pfm1, simts1)
fitPM(pfm1, as.numeric(simts1))

fitPM(c(1,1,1,1), simts1)
fitPM(c(3,2,2,1), simts1)
fitPM(c(3,2,2,2), simts1)

pdSafeParOrder(c(3,2,2,1))
pdSafeParOrder(rev(c(3,2,2,1)))

x <- arima.sim(list(ar = 0.9), n = 960)
pcx <- pcts(x, nseasons = 4)
mx <- matrix(x, nrow = 4)

##pc.acf(mx)
##pc.acf(mx, maxlag=10)
## TODO: avoid the warning when length ot the time series is not multiple
autocovariances(t(mx), maxlag = 6, nseasons = 4)
autocovariances(t(mx))

##It is an error to have more columns than rows.
## autocovariances(mx, maxlag = 6, nseasons = 4)
## autocovariances(mx)

num2pcpar(mx, c(1,1,1,1), period = 4)
num2pcpar(mx, c(3,3,3,3), period = 4)

sipfm1 <- new("SiPeriodicArModel", iorder = 1, siorder = 1, pcmodel = pfm1)
sipfm1
fitPM(sipfm1, mx)
pfm1


## experiments and testing
fit1    <- fitPM(c(3,3,3,3), simts1)
fit1_mf <- new("MultiFilter", coef = fit1@ar@coef)
vs      <- mcompanion::mf_VSform(fit1_mf, form = "I")
tmp <- mcompanion::VAR2pcfilter(vs$Phi[ , -4],
                                Phi0inv = vs$Phi0inv, D = fit1@sigma2, what = "")
names(tmp) #  "pcfilter" "var"      "Uform"   
tmp$var
zapsmall(tmp$pcfilter)
fit1@ar@coef
all.equal(tmp$pcfilter[ , 1:3], fit1@ar@coef, check.attributes = FALSE) # TRUE
tmp$Uform
fit1@sigma2

## both give the matrix Sigma for the "I" form
identical(
    vs$Phi0inv \%*\% diag(fit1@sigma2) \%*\% t(vs$Phi0inv)
    ,
    tmp$Uform$U0inv \%*\% diag(tmp$Uform$Sigma)  \%*\% t(tmp$Uform$U0inv)
) # TRUE

## no, this is a different matrix
var1_mat <- cbind(vs$Phi0, # identity matrix
                  - vs$Phi) # drop trailing zero columns?
var1_mat <- mcompanion::mCompanion(var1_mat)
var1_Sigma <- vs$Phi0inv \%*\% diag(fit1@sigma2) \%*\% t(vs$Phi0inv)
abs(eigen(diag(nrow(var1_mat)) - var1_mat)$values)
}
\keyword{pcts}
\keyword{methods}
