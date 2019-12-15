\name{meanvarcheck}
\alias{meanvarcheck}
\alias{meancovmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Asymptotic covariance matrix of periodic mean}
\description{Asymptotic covariance matrix of periodic mean.}
\usage{
meanvarcheck(parmodel, n)

meancovmat(parmodel, n, cor = FALSE, result = "var")
}
\arguments{
  \item{parmodel}{a periodic model.}
  \item{n}{number of observations (TODO: need clarification here).}
  \item{cor}{if \code{TRUE}, return correlations}
  \item{result}{
    if \code{"var"}, return the diagonal of the covariance matrix,
    otherwise return the matrix.
  }
}
\details{

  Computes asymptotic covariance or correlation matrix of the periodic
  means.
  
}
\value{
  if \code{result = "var"} a matrix,
  otherwise a vector
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{parcovmatlist}}
}
\examples{
x <- arima.sim(list(ar=0.9), n=1000)
proba1 <- fitPM(c(3,2,2,2), x)

meancovmat(proba1, 100)
meancovmat(proba1, 100, cor = TRUE)
meancovmat(proba1, 100, result = "")
meancovmat(proba1, 100, cor = TRUE, result = "")

meanvarcheck(proba1, 100)
}
%\keyword{ ~kwd1 }
% use one of  RShowDoc("KEYWORDS")
