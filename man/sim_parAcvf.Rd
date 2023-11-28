\name{sim_parAcvf}
\alias{sim_parAcvf}
\title{Create a random periodic autocovariance function}
\description{
  Select randomly a periodic autoregression model and return the
  periodic autocovariances associated with it. 
}
\usage{
sim_parAcvf(period, order, sigma2)
}
\arguments{
  \item{period}{the period, a positive integer.}
  \item{order}{the AR order, a vector of non-negative integers.}
  \item{sigma2}{the variances of the innovations, a numeric vector of
    length \code{period} (todo: or one?).
  }
  %\item{...}{passed on to \code{sim_parAcvf}.}
 
}
\details{
  Uses \code{sim_parCoef()} to generate a random PAR model.
}
\value{
  an object of class "matrix". In addition, the specification of the
  model is in attribute \code{"model"} which is a list with the
  following components:
  \item{ar}{a matrix, the coefficients of the PAR model,}
  \item{sigma2}{numeric, the innovation variances,}
  \item{order}{the PAR order.}
}
\references{
  \insertRef{boshnakov2009gen}{pcts}
}
\author{Georgi N. Boshnakov}
\examples{
sim_parAcvf(2, 5)
sim_parAcvf(3, 5)

res <- sim_parAcvf(2, 6)
res
slMatrix(res)[3, 4, type = "tt"]

res <- sim_parAcvf(2, 4)
attr(res, "model")
acv <- res[ , ] # drop attributes

acv[2, 1 + 0]
acv[2, 1 + 1]
slMatrix(acv)[2, 0]
slMatrix(acv)[2, 1]
slMatrix(acv)[3, 4, type = "tt"]
slMatrix(acv)[1:2, 1:2, type = "tt"]
slMatrix(acv)[1:4, 1:4, type = "tt"]

## TODO: need method for autocorrelation()
## pc.acrf(acv)

## TODO: these need changing, after the change of the return values of sim_parAcvf
## pc.fcoeffs(acv, 2)
## pc.fcoeffs(acv, 3)
## pc.fcoeffs(acv, 4)
pcts:::calc_predictionCoefficients(acv, c(2, 2))
pcts:::calc_predictionCoefficients(acv, c(3, 3))
pcts:::calc_predictionCoefficients(acv, c(4, 4))
}
\keyword{permodel}
\keyword{datagen}






