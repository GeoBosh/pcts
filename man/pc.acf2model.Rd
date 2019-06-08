\name{pcarma_acvf2model}
\alias{pcarma_acvf2model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit a PC-ARMA model to a periodic autocovariance function}
\description{
  Fit a PC-ARMA model to a periodic autocovariance function.
}
\usage{
pcarma_acvf2model(acf, model, maxlag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acf}{a periodic autocovariance function, an object of class
    \code{pcAcvf}.}
  \item{model}{a pc- arma model, an object of class
    \code{pcARMApq}. (todo: check!)}
  \item{maxlag}{not used. (todo: check!)}
}
% \details{
%
% }
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{
  % bibentry: boshnakov1996pcarma
Boshnakov GN (1996).
``Recursive computation of the parameters of periodic
autoregressive moving-average processes.''
\emph{J. Time Ser. Anal.}, \bold{17}(4), pp. 333--349.
ISSN 0143-9782, \url{http://dx.doi.org/10.1111/j.1467-9892.1996.tb00281.x}.
% end:bibentry:  boshnakov1996pcarma

}
%\author{ ~~who you are~~ }
%\note{}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(ex1f)
pc3 <- slMatrix(period=2,maxlag=5,f=ex1f,type="tt")
# pcarma_param_system(pc3, NULL, NULL, 2, 0, 2)
parsys <- pcarma_param_system(pc3, NULL, NULL, c(2,2), 0, 2)
param <- solve(parsys$A,parsys$b)

# res <- pcarma_acvf2model(pc3, list(p=c(1,2),q=0,period=2))
# res <- pcarma_acvf2model(pc3, list(p=c(1,2),q=0))
# res <- pcarma_acvf2model(pc3, list(p=c(1,2),period=2))
res <- pcarma_acvf2model(pc3, list(p=c(1,2)))

print(param)
print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\author{Georgi N. Boshnakov}
\keyword{permodel}
