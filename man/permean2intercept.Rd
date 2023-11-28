\name{permean2intercept}
\alias{permean2intercept}
\alias{intercept2permean}
\title{Convert between periodic centering and intercepts}
\description{
  Convert a periodic mean to periodic intercept and vice versa.
} 
\usage{
permean2intercept(mean, coef, order, nseasons = nrow(coef))

intercept2permean(intercept, coef, order, nseasons = nrow(coef))
}
\arguments{
  \item{mean}{periodic mean, numeric.}
  \item{coef}{PAR coefficients, matrix.}
  \item{order}{PAR order, vector of positive integers.}
  \item{nseasons}{number of seasons, a.k.a. period.}
  \item{intercept}{periodic intercepts, numeric.}
}
\details{
  A PAR model can be written in mean corrected or intercept form.
  \code{permean2intercept} calculates the intercepts from the means,
  while \code{intercept2permean} does the inverse (means from
  intercepts).

  No check is made for periodic stationarity of the model. Converting
  from mean corrected to intercept form allways succeeds and in fact the
  means do not need to be means. In the opposite direction there may be
  problems due to unit roots and similar features.
}
\value{
  a numeric vector
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
mu <- c(1, 2)
pm1 <- PeriodicArModel(matrix(c(0.5, 0.5), nrow = 2), order = rep(1, 2), sigma2 = 1, mean = mu)

cc <- permean2intercept(mu, pm1@ar@coef, c(1,1))
cc
intercept2permean(cc, pm1@ar@coef, c(1,1))

d <- 4
mu <- 1:d
co <- rep(0.5, d)
pm1 <- PeriodicArModel(matrix(co, nrow = d), order = rep(1, d), sigma2 = 1, mean = mu)

cc <- permean2intercept(mu, pm1@ar@coef, order = rep(1, d))
cc
intercept2permean(cc, pm1@ar@coef, order = rep(1, d) )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ permodel }
