\name{parcovmatlist}
\alias{parcovmatlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute asymptotic covariance matrix for PAR model}
\description{Compute asymptotic covariance matrix for PAR model}
\usage{
parcovmatlist(parmodel, n, cor = FALSE, result = "list")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parmodel}{PAR model, object of class \code{parModel}}
  \item{n}{length of the series or a vector with one element for each
    season.
  }
  \item{cor}{If TRUE return correlation matrix.}
  \item{result}{if "list", the default, return a list, if "Matrix"
    return a Matrix object, otherwise return an ordinary matrix, see
    Details.}
}
\details{
  Uses eq. (3.3) in the reference.

  If \code{result = "list"}, \code{parcovmatlist} returns a list whose
  s-th element is the covariance matrix of the PAR parameters for the
  s-th season. Otherwise, if \code{result = "Matrix"} it returns a
  block-diagonal matrix created by \code{.bdiag()} from package
  "Matrix". If \code{result = "matrix"} it returns an ordinary matrix
  (with the current implementation this is returned for any value other
  than "list" or "Matriix").
}
\value{
  a list, matrix or block-diagonal matrix, as described in Details
}
\references{
  \insertRef{McLeod1994diagnostic}{pcts}
}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{pcacfMat}},
  \code{\link{pc.acf.parModel}}
}
\examples{
x <- arima.sim(list(ar=0.9), n=1000)
proba1 <- fitPM(c(3,2,2,2), x)

parcovmatlist(proba1, 100)
parcovmatlist(proba1, 100, cor = TRUE)
sqrt(diag(parcovmatlist(proba1, 100, cor = TRUE)[[1]]))

meanvarcheck(proba1, 100)
}
\keyword{pcstat}
