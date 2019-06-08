\name{permodelmf}
\alias{permodelmf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the multi-companion form of a per model}
\description{Compute the multi-companion form of a per model.}
\usage{
permodelmf(permodel, update = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{permodel}{a model.}
  \item{update}{If TRUE store the multi-companion form in
    \code{permodel} and return the whole model, otherwise simply return
    the multi-companion form.}
}
\details{todo:}
\value{
  the multi-companion form of the model or the updated model, as
  described in Details.
}
\references{
  % bibentry: boshnakov2009gen
Boshnakov GN and Iqelan BM (2009).
``Generation of time series models with given spectral
properties.''
\emph{J. Time Series Anal.}, \bold{30}(3), pp. 349--368.
ISSN 0143-9782, \url{http://dx.doi.org/10.1111/j.1467-9892.2009.00617.x}.
% end:bibentry:  boshnakov2009gen
}
\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }
%
% %% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ permodel }
