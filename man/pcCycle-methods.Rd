\name{pcCycle-methods}
\docType{methods}
\alias{pcCycle}
\alias{pcCycle-methods}
\alias{BuiltinCycle}

%\alias{pcCycle,character-method}
%\alias{pcCycle,numeric-method}
%\alias{pcCycle,Cyclic-method}
%\alias{pcCycle,ts-method}
%\alias{pcCycle,PeriodicTimeSeries-method}
\alias{pcCycle,character,ANY-method}
\alias{pcCycle,Cyclic,ANY-method}
\alias{pcCycle,numeric,character-method}
\alias{pcCycle,numeric,missing-method}
%\alias{pcCycle,PeriodicTimeSeries,ANY-method}
%\alias{pcCycle,ts,ANY-method}
\alias{pcCycle,BasicCycle,character-method}
\alias{pcCycle,BasicCycle,missing-method}
\alias{pcCycle,character,character-method}
\alias{pcCycle,character,missing-method}
\alias{pcCycle,PeriodicTimeSeries,character-method}
\alias{pcCycle,PeriodicTimeSeries,missing-method}
\alias{pcCycle,ts,character-method}
\alias{pcCycle,ts,missing-method}
\title{Create or extract Cycle objects}
\description{

  \code{pcCycle()} is a generic function with methods for creating,
  converting, modifying, and extracting cycle objects. \code{BuiltinCycle()} 
  is a function to create cycle objects from the builtin cycle classes.

}
\usage{
pcCycle(x, type, \dots)

BuiltinCycle(n, coerce = FALSE, first = 1, stop = TRUE)
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
  
  \item{n}{number of seasons, an integer.}
  \item{coerce}{
    if \code{TRUE} coerce the objects to a modifiable cycle class,
    currently \code{"SimpleCycle"}.
  }
  \item{first}{which season is first for this object.}
  \item{stop}{
    if \code{TRUE}, the default, throw error if there is no builtin
    class with \code{n} seasons, otherwise create a \code{"BareCycle"}
    object.
  }
}
\details{

  \code{pcCycle} serves as both a constructor and extractor of cycle
  objects. It is meant to just do the right thing, relieving the user
  from the burden of specifying a particular cycle class.

  If \code{x} is numeric it constructs a cycle object with period
  \code{x} and additional properties as specified by the other
  arguments.  If \code{x} is a character string, it is taken to be the
  name of one of the builtin cycles.

  \code{pcCycle} can be used to create a modified version of a cycle
  object and/or convert it to another cycle type. This is done by
  providing a cycle object as argument \code{x}, i.e. one inheriting
  from \code{"BasicCycle"}. 
  
  If \code{x} inherits from \code{"Cyclic"}, \code{pcCycle} returns its
  cycle component.

  Argument \code{type} should be rarely needed, except maybe to
  conveniently force conversion of the builtin type to an ordinary type.

  The descriptions of the individual methods in section Methods give
  some further specific details.

  \code{BuiltinCycle} is a convenience function to create objects from
  builtin cycle classes by specifying the number of seasons.  The
  builtin cycle classes are esseintially fixed, except that which season
  is considered first can be changed using argument \code{first}. If
  other modifications are desired, convert the returned builtin cycle
  object to class \code{"SimpleCycle"}. This can be done also in the
  call to \code{BuiltinCycle()} by specifying \code{coerce = TRUE}.

  By default, \code{BuiltinCycle} throws an error if there is no builtin
  class with the requested number of seasons. Set argument \code{stop}
  to FALSE to create an object from class \code{"BareCycle"} instead
  (and it will be converted to \code{"SimpleCycle"} if \code{coerce =
  TRUE}). Argument \code{stop} is mainly for programming.

}
\value{
  for \code{pcCycle}, an object from one of the cycle classes;

  for \code{BuiltinCycle}, an object from one of the builtin classes,
  coerced if requested.

}
\section{Methods}{
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
\describe{

  \item{\code{signature(x = "numeric", type = "missing")}}{

    creates a cycle object with period \code{x}. If \code{x} is the only
    argument, a \code{"BareCycle"} object is created, otherwise the
    constructor of \code{"SimpleCycle"} is invoked with all arguments
    except \code{type} passed on to it.

  }

  \item{\code{signature(x = "character", type = "missing")}}{
    creates an object from the class specified by \code{x}.
    Currently this is equivalent to \code{new(x, ...)} but somewhat
    more portable. Future amendments may use a more suitable class
    for some combinations of the arguments. Also, if a class is renamed,
    a code will be inserted here to create an equivalent object.
  }
  
  \item{\code{signature(x = "numeric", type = "character")}}{ }
  \item{\code{signature(x = "character", type = "character")}}{
    first call the method with \code{type = "missing"}, then
    convert the result to class \code{type}.
  }

  \item{\code{signature(x = "Cyclic", type = "ANY")}}{
    extracts the cycle component of \code{x} (\code{x@cycle}).
    Currently ignores the remaining arguments.
  }

  \item{\code{signature(x = "BasicCycle", type = "missing")}}{
    convert an object from any cycle class to class
    \code{"SimpleCycle"}. This is like \code{as(x, "SimpleCycle")} but
    can have further arguments. 
  }
  \item{\code{signature(x = "BasicCycle", type = "character")}}{
    convert an object from any cycle class to class
    \code{type}. 
  }

  \item{\code{signature(x = "ts", type = "missing")}}{ }
  \item{\code{signature(x = "ts", type = "character")}}{

    when \code{x} is of class \code{"ts"}, extract the frequency and
    convert it to a cycle class. Just as for \code{"ts"}, certain
    frequencies are taken to correspond to specific classes. While base
    \R treats periodicities 4 and 12 specially, \code{pcCycle} extends
    this to all builtin classes in pcts. Argument \code{type} can be
    used to overwrite this default behaviour by requesting a specific
    class. In particular, \code{type = "BareCycle"} and \code{type = ""}
    cause the result to be \code{"BareCycle"}.
    
  }

  \item{\code{signature(x = "PeriodicTimeSeries", type = "missing")}}{ }
  \item{\code{signature(x = "PeriodicTimeSeries", type = "character")}}{

    extract the cycle part of an object inheriting from
    \code{"PeriodicTimeSeries"}, currently \code{"PeriodicTS"} or
    \code{"PeriodicMTS"}. Argument \code{type} can be used to force the
    result to be from a specific cycle class, as in the methods for
    \code{"ts"}.

  }

}}
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
\seealso{
  % \code{\link{BuiltinCycle}} for the recommended way to create objects
  % from the builtin classes

  \code{\link{allSeasons}} for further examples,
  
  class \code{\linkS4class{BuiltinCycle}} for the available builtin
  classes and more examples
    
}
\examples{
## pcCycle
pcCycle(4)
pcCycle(4, seasons = c("Spring", "Summer", "Autumn", "Winter"))

pcCycle("QuarterYearCycle")
BuiltinCycle(4) # same, recommended

pcCycle("QuarterYearCycle", type = "BareCycle")
pcCycle("QuarterYearCycle", type = "SimpleCycle")

## BuiltinCycle
BuiltinCycle(2)  # "OpenCloseCycle"
BuiltinCycle(4)  # "QuarterYearCycle"
BuiltinCycle(5)  #  five day week cycle
BuiltinCycle(7)  # "DayWeekCycle"
BuiltinCycle(12) # "MonthYearCycle"
BuiltinCycle(48) # "Every30MinutesCycle"

## error, since there is no builtin cycle with 19 seasons:
## BuiltinCycle(19)

## use stop = FALSE to reate a default cycle in this case
BuiltinCycle(19, stop = FALSE)
BuiltinCycle(19, coerce = TRUE, stop = FALSE)
}
\keyword{methods}

