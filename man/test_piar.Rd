\name{test_piar}
\alias{test_piar}
\title{Test for periodic integration}
\description{Test if a time series is periodically integrated.}
\usage{
test_piar(x, d, p, sintercept = FALSE, sslope = FALSE, homoschedastic = FALSE)
}
\arguments{
  \item{x}{time series.}
  \item{d}{period.}
  \item{p}{autoregressive order, a positive integer.}
  \item{sintercept}{if TRUE, include seasonal intercept.}
  \item{sslope}{if TRUE, include seasonal slope.}
  \item{homoschedastic}{if TRUE, assume the innovations variance is the
    same for all seasons.}
}
\details{
  Computes test statistics for Franses (1996) test for periodic
  integration of order 1. The test is based on periodic autoregression
  of order \code{p}, where \code{p} can be any positive integer.
}
\value{
  a list with the following components:

  \item{p}{autoregressive order.}
  \item{spec}{
    values of \code{sintercept}, \code{sslope}, and
    \code{homoschedastic}, a named logical vector.
  }
  \item{statistics}{
    a matrix containing the test statistics (first row) and the
    corresponding p-values (second row).  \code{"LR"} is not normalised,
    so its p-value is \code{NA}.
  }
}
\references{
  % bibentry: boswijk1996unit
Boswijk HP and Franses PH (1996).
``Unit roots in periodic autoregressions.''
\emph{Journal of Time Series Analysis}, \bold{17}(3), pp. 221--245.
% end:bibentry:  boswijk1996unit
}
\author{Georgi N. Boshnakov}
\note{
  Currently only the case \code{p = 1} is handled, for \code{p > 1} the
  statistics are set to NA. \strong{:TODO: handle this.}

  All statistics are computed but some p-values are not computed yet.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{pclspiar}},
  \code{\link{pclsdf}}
}
\examples{
ts1 <- window(dataFranses1996[ , "CanadaUnemployment"],
              start = c(1960, 1), end = c(1987, 4))
test_piar(ts1, 4, 1, sintercept = TRUE)
pcTest(ts1, "piar", 4, 1, sintercept = TRUE) # same

test_piar(ts1, 4, 1, sintercept = TRUE, sslope = TRUE)
test_piar(ts1, 4, 1)              
test_piar(ts1, 4, 1, homoschedastic = TRUE)              
}
\keyword{pcstat}
