\name{filterCoef-methods}
\docType{methods}
%\alias{filterCoef}
\alias{filterCoef-methods}
\alias{filterCoef,PeriodicBJFilter,character-method}
\alias{filterCoef,PeriodicSPFilter,character-method}
\title{Get the coefficients of a periodic filter}
\description{Get the coefficients of a periodic filter.}
%% \usage{
%% filterCoef(object, convention, ...)
%% }
%% %- maybe also 'usage' for other objects documented here.
%% \arguments{
%%   \item{object}{a periodic filter object.}
%%   \item{convention}{
%%     a character string, convention for the signs, see Details.
%%   }
%%   \item{\dots}{further arguments for methods.}
%% }
\details{

  \code{filterCoef} is a generic function to extract the coefficients of
  periodic filters. Argument \code{convention} can be used to force a
  particular convention for the signs. The description here is for the
  methods defined in this package.

  If \code{convention} is missing, the coefficient matrix is returned as
  stored in the object. Otherwise, if \code{convention} is one of the
  strings \code{"BJ"}, \code{"--"} or \code{"-"}, the coefficients
  returned have the opposite sign of those in the auxilliary polynomial
  (Box-Jenkins' convention). if \code{convention} is one of \code{"SP"},
  \code{"++"} or \code{"+"}, the coefficients are as in the auxilliary
  polynomial (convention used in signal processing).

}
\value{
  a matrix
}
\section{Methods}{
\describe{

\item{\code{signature(object = "PeriodicBJFilter", convention = "character")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "PeriodicSPFilter", convention = "character")}}{{
%%  ~~describe this method here~~
}
}
}}
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
\keyword{methods}









