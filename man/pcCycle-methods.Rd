\name{pcCycle-methods}
\docType{methods}
\alias{pcCycle}
\alias{pcCycle-methods}
%\alias{pcCycle,character-method}
%\alias{pcCycle,numeric-method}
%\alias{pcCycle,Cyclic-method}
%\alias{pcCycle,ts-method}
%\alias{pcCycle,PeriodicTimeSeries-method}
\alias{pcCycle,character,ANY-method}
\alias{pcCycle,Cyclic,ANY-method}
\alias{pcCycle,numeric,character-method}
\alias{pcCycle,numeric,missing-method}
\alias{pcCycle,PeriodicTimeSeries,ANY-method}
\alias{pcCycle,ts,ANY-method}
\alias{pcCycle,BasicCycle,character-method}
\alias{pcCycle,BasicCycle,missing-method}
\alias{pcCycle,character,character-method}
\alias{pcCycle,character,missing-method}
\title{Create or extract Cycle objects}
\description{

  Generic function with methods for creating or extracting cycle
  objects. The type of the cycle object to return is inferred from the
  arguments.

}
\usage{
pcCycle(x, type, \dots)
}
\arguments{
  \item{x}{
    an object, methods include numeric, character and cyclic objects,
    see Details.

  }
  \item{type}{
    class of the result. If equal to \code{"auto"}, the default, the
    class is determined by the argument(s), otherwise should be the name
    of a cycle class.

  }
  \item{\dots}{
    further arguments for methods.
  }
}
\details{

  \code{pcCycle} serves as both a constructor and extractor of cycle
  objects. It is meant to just do the right thing, relieving the user
  from the burden of specifying a particular cycle class.

  If \code{x} inherits from \code{"Cyclic"}, \code{pcCycle} returns its
  cycle component. If \code{x} is numeric it constructs a cycle object
  with period \code{x} and additional properties as specified by the
  other arguments.  If \code{x} is a character string, it is taken to be
  the name of one of the builtin cycles.

  Argument \code{type} should be rarely needed, except maybe to
  conveniently force conversion of the builtin type to an ordinary type.

}
\value{
  an object from one of the cycle classes
}
\section{Methods}{
\describe{

%%%% \item{\code{signature(x = "character")}}{
%%%%   \code{x} should be the name of a builtin cycle class.
%%%% }
%%%% 
%%%% \item{\code{signature(x = "numeric")}}{
%%%% 
%%%%   \code{x} is the period in this case. If \code{x} is the only argument
%%%%   (not counting \code{type}), a \code{"BareCycle"} object is created,
%%%%   otherwise the constructor of \code{"SimpleCycle"} is invoked with all
%%%%   arguments except \code{type} passed on to it.
%%%% 
%%%% }
%%%% \item{\code{signature(x = "Cyclic")}}{
%%%%   Returns \code{x@cycle}. Currently ignores the remaining arguments.
%%%% }
%%%% \item{\code{signature(x = "ts")}}{{
%%%% %%  ~~describe this method here~~
%%%% }
%%%% }\item{\code{signature(x = "PeriodicTimeSeries")}}{{
%%%% %%  ~~describe this method here~~
%%%% }
%%%% }

%\item{\code{signature(x = "character", type = "ANY")}}{{
%  \code{x} should be the name of a builtin cycle class.
%}
%}
\item{\code{signature(x = "Cyclic", type = "ANY")}}{{
  Returns \code{x@cycle}. Currently ignores the remaining arguments.
}
}\item{\code{signature(x = "numeric", type = "character")}}{{

  \code{x} is the period in this case. If \code{x} is the only argument
  (not counting \code{type}), a \code{"BareCycle"} object is created,
  otherwise the constructor of \code{"SimpleCycle"} is invoked with all
  arguments except \code{type} passed on to it.

}
}\item{\code{signature(x = "numeric", type = "missing")}}{{

  \code{x} is the period in this case. If \code{x} is the only argument
  (not counting \code{type}), a \code{"BareCycle"} object is created,
  otherwise the constructor of \code{"SimpleCycle"} is invoked with all
  arguments except \code{type} passed on to it.

}
}\item{\code{signature(x = "PeriodicTimeSeries", type = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "ts", type = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "BasicCycle", type = "character")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "BasicCycle", type = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "character", type = "character")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "character", type = "missing")}}{{
%%  ~~describe this method here~~
}
}}}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
pcCycle(4)
pcCycle(4, seasons = c("Spring", "Summer", "Autumn", "Winter"))

pcCycle("QuarterYearCycle")
pcCycle("QuarterYearCycle", type = "BareCycle")
pcCycle("QuarterYearCycle", type = "SimpleCycle")
}
\keyword{methods}

