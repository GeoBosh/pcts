\name{sim_parCoef}
\alias{sim_parCoef}
\concept{periodic autoregression}
\title{ Generate a periodic autoregression model }
\description{
  Generate a periodic autoregression model, possibly integrated.
}
\usage{
sim_parCoef(period, n.root, sigma2 = rep(1, period), ...)
}
\arguments{
  \item{period}{ number of seasons in a cycle. }
  \item{n.root}{ number of non-zero roots, see details. }
  \item{sigma2}{ variances of the innovations. }
  \item{...}{ additional arguments to be passed down to \code{sim_pcfilter} }
}
\details{
  \code{sim_parCoef} uses the multi-companion method to generate the
  model. The function is essentially a wrapper for \code{sim_pcfilter}.

  The order of the filter is set to \code{n.root} for each season.  Part
  of the spectral information may be specified with the \code{"..."}
  arguments, see \code{\link{sim_pcfilter}} and \code{\link{sim_mc}} for
  a discussion of this.
}
\value{
  a periodic autoregression model as a list with elements:
  \item{ar}{a matrix whose \eqn{i}{i-}th row contains the coefficients
  for the \eqn{i}{i-}season,}
  \item{sigma2}{the innovation variances, a numeric vector.}
}
\references{
  \insertRef{boshnakov2009gen}{pcts}
}
\author{Georgi N. Boshnakov}
\seealso{ \code{\link{sim_pcfilter}} }
\examples{
sim_parCoef(2, 4)                    # 2 seasons
sim_parCoef(2, 4, sigma2 = c(2, 4))
sim_parCoef(2, 1)
sim_parCoef(4, 2)                    # 4 seasons

sim_parCoef(period = 4, n.root = 6,
    eigabs = c(1, 1, 1, 0.036568887, 0.001968887),
    type.eigval = c("cp", "r", "r", "r",  "r"),
    eigsign     = c(pi/2,   1,  -1,   1,   -1))
}
\keyword{simulation}
\keyword{permodel}

