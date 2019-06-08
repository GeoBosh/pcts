\name{modelCycle}
\docType{methods}
\alias{modelCycle}
\alias{modelCycle-methods}
\alias{modelCycle,ANY-method}
\alias{modelCycle,ModelCycleSpec-method}

\alias{modelCycle<-}
\alias{modelCycle<--methods}
\alias{modelCycle<-,ANY-method}
\alias{modelCycle<-,ModelCycleSpec-method}

\title{Get the cycle of a periodic object}
\description{
  Get the cycle of a periodic object, ageneric function.
}
\usage{
modelCycle(object)

modelCycle ( object, ... ) <- value
}
\arguments{
  \item{object}{an object.}
  \item{value}{

    the new value for the cycle, an object inheriting from
    \code{"BasicCycle"}.

  }
  \item{...}{not used.}
}
\details{

  \code{modelCycle()} is essentially internal, for programming. The user
  level function to get the cycle of an object is \code{\link{pcCycle}}.

  \code{modelCycle()} returns the Cycle object (in the sense of package
  \pkg{pcts}), associated with \code{object}. This is a generic
  function which makes it possible to associate a cycle with objects
  from a class, without inheriting from the cycle classes.

  The default method returns \code{NULL}, unconditionally. 
  
}
\value{

  for \code{modelCycle()}, an object inheriting from class
  \code{"BasicCycle"} or \code{NULL};

  \code{"modelCycle<-"()} is used for the side effect of changing the
  cycle of \code{object}. 
  
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "ModelCycleSpec")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
  
  \code{"\link{modelCycle<-}"} for the replacement version.

}
%\examples{
%}
\keyword{methods}
