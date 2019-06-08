\name{PeriodicArModel-methods}
\docType{methods}
\alias{PeriodicArModel}
\alias{PeriodicArModel-methods}
\alias{PeriodicArModel,matrix-method}
\alias{PeriodicArModel,numeric-method}
%\alias{PeriodicArModel,PeriodicArmaSpec-method}
%\alias{PeriodicArModel,PeriodicFilter-method}
\alias{PeriodicArModel,PeriodicArmaModel-method}
\alias{PeriodicArModel,PeriodicMonicFilterSpec-method}
\alias{PeriodicArModel,VirtualPeriodicArmaModel-method}
\title{Create objects from class PeriodicArModel}
\description{Create objects from class PeriodicArModel}
\usage{
PeriodicArModel(object, \dots)
}
\arguments{
  \item{object}{an object, can have one of a number of classes.}
  \item{\dots}{further arguments for methods.}
}
\details{

  \code{PeriodicArModel} creates objects from class
  \code{PeriodicArModel}. This is a generic function with despatch methods on
  the first argument.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  an object from class \code{"PeriodicArModel"}
}

\section{Methods}{
\describe{

  \item{\code{signature(object = "matrix")}}{
    \code{"object"} gives the coefficients, one row per season.
}

\item{\code{signature(object = "numeric")}}{
      \code{"object"} gives the model order. Its length is taken to be
      the number of seasons. The coefficients are set to NA.
%%  ~~describe this method here~~
}

%\item{\code{signature(object = "PeriodicArmaSpec")}}{
%    \code{"object"}  is taken to specify the AR part of the model. Its
%    class should have suitable methods. This is the case for classes
%    from this package.
%%%  ~~describe this method here~~
%}
%
%\item{\code{signature(object = "PeriodicFilter")}}{
%  Another way to specify the Ar part of the model. \code{"object"} is
%  converted internally to \code{"PeriodicArFilter"}.
%%%  ~~describe this method here~~
%}
%%% \item{\code{signature(object = "PeriodicArmaModel")}}{{
%%% %%  ~~describe this method here~~
%%% }
\item{\code{signature(object = "PeriodicMonicFilterSpec")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "VirtualPeriodicArmaModel")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "PeriodicMonicFilterSpec")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "VirtualPeriodicArmaModel")}}{{
%%  ~~describe this method here~~
}
}}}
%\seealso{
%}
%\examples{
%}
\keyword{methods}



