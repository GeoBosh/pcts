\name{fit_trigPAR_optim}
\alias{fit_trigPAR_optim}
\title{Fit a subset trigonometric PAR model}
\description{

  Fit a subset PAR model with trigonometric parameterisation.

}
\usage{
fit_trigPAR_optim(x, order, nseasons, seasonof1st = 1, maxiter = 200, 
                  harmonics = NULL, sintercept = FALSE, tol = 1e-07, 
                  type = c("vecbyrow", "bylag"), verbose = TRUE)
}
\arguments{
  \item{x}{time series.}
  \item{order}{order, an integer number.}
  \item{nseasons}{number of seasons, an integer number.}
  \item{seasonof1st}{season of the first observation.}
  \item{maxiter}{max number of iterations.}
  \item{harmonics}{
    the harmonics to include in the model, vector of non-negative
    integers.
  }
  \item{sintercept}{if \code{TRUE} include seasonal intercept.}
  \item{tol}{when to stop the iterations.}
  \item{type}{

    type of parameterisation, currently one of \code{"vecbyrow"} or
    \code{"bylag"}.

  }
  \item{verbose}{if \code{TRUE} print more details during estimation.}
}
\details{

  Fits a subset PAR model using trigonometric parameterisation,
  i.e. Fourier series for the periodic coefficients written in terms of
  sines and cosines.

  If argument \code{type} is \code{bylag}, the parameters for each lag
  are parameterised independently from other lags. If \code{sintercept}
  is \code{TRUE}, it has its own trigonometric representation.

  If argument \code{type} is \code{vecbyrow} (\dQuote{Vec operation by
  row}), the PAR  parameters are stacked in a vector with all parameters
  for the first season, followed by all parameters for the second, and
  so on. The trigonometric parameterisation for this vector is used.
  So the fundamental frequency is \code{1/(nseasons * order)}.
  If \code{sintercept} is \code{TRUE} when \code{type = vecbyrow}, then
  then the intercept for eaach season is put before the PAR parameters
  and the fundamental frequency becomes \code{1/(nseasons * (order + 1)}. 
  Putting together the intercepts and the PAR parameters may not be very
  useful for parsimonious trigonometric parameterisation, so to have a
  separate set of coefficients for the intercepts set
  attribute{"merge"} of \code{sintercept} to \code{FALSE}.
  
}
\value{
  an object from class \code{\linkS4class{SubsetPM}}
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
\note{
  This function may change.
}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
## see examples for class "SubsetPM"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ts}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
