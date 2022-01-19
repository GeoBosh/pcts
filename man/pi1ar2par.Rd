\name{pi1ar2par}
\alias{pi1ar2par}
\alias{piar2par}
\title{Convert PIAR coefficients to PAR coefficients}
\description{Convert PIAR coefficients to PAR coefficients}
\usage{
pi1ar2par(picoef, parcoef)
piar2par(picoef, parcoef)
}
\arguments{
  \item{picoef}{coefficients of the periodic integration filter, a
    numeric vector or matrix, see Details.
  }
  \item{parcoef}{
    coefficients of the periodically correlated part of the model.
  }
}
\details{

  These functions expand periodic filters represented in multiplicative
  form.  The non-periodic analogue of the operation of these functions
  is representing a multiplicative filter like \eqn{(1-B)(1-aB)}, where
  \eqn{B} is the backward shift operator, by the single filter \eqn{1 -
  (1+a)B + aB^2}, which is just a product of the polynomials, \eqn{1-B}
  and \eqn{1 - aB}.

  In the non-periodic case however this operation is not, in general,
  equivalent to multiplication of the corresponding polynomials. It is
  also not commutative.

  \code{pi1ar2par} converts PIAR(1) model coefficients specified as a
  set of coefficients corresponding to a periodic unit root and PAR
  coefficients to coefficients for a single filter.

  \code{piar2par} does the same but admits higher order periodic
  integration.

  \code{picoef} is a matrix, specifying one or more first order periodic
  unit root filters. Each column contains the coefficients of one
  filter. If there is only one filter, its coefficients can be given as
  a numeric vector.

  The filters are applied from right to left, in the sense that first
  the PAR filter is applied to the time series, then the filter
  specified by the last column and so on.

}
\value{
  a matrix, whose i-th row contains the coefficients for the i-th season.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
  Georgi N. Boshnakov
}
% \note{
% %%  ~~further notes~~
% }
%
% %% ~Make other sections like Warning with \section{Warning }{....} ~
%
% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
## Lina's example
parcoef    <- rbind(c(0.5, -0.06), c(0.6, -0.08),
                    c(0.7, -0.1),  c(0.2, 0.15) )
picoef1    <- c(0.8, 1.25, 2, 0.5)
parcoef2   <- pi1ar2par(picoef1, parcoef)

picoef2    <- c(4, 0.25, 5, 0.2)
coefper2I2 <- pi1ar2par(picoef2, parcoef2)
}
\keyword{permodel}
