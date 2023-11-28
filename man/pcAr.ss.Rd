\name{pcAr.ss}
\alias{pcAr.ss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the sum of squares for a given PAR model}
\description{Compute the sum of squares for a given PAR model.}
\usage{
pcAr.ss(x, model, eps = numeric(length(x)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{time series, a numeric vector.}
  \item{model}{a model.}
  \item{eps}{residuals, defaults to a vector of zeroes. This may be used
  for models with moving average terms, for example.}
}
\details{
  todo:
}
\value{
  a number
}
\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }
%
% %% ~Make other sections like Warning with \section{Warning }{....} ~
%
% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
% }
\keyword{ pcts }
