\name{pcts_exdata}
\alias{pcts_exdata}
\title{Periodic time series objects for examples}
\description{

  Periodic time series objects for examples and tests. These objects are
  from classes defined in package \dQuote{pcts} and as a consequence are
  not suitable for access with \code{data()}.
  
}
\usage{
pcts_exdata(x, envir = parent.frame())
}
\arguments{
  \item{x}{
    a character vector giving the names of objects. If missing, all
    available objects will be created. Can also be \code{NA}. In that
    case no objects are created and the names of all available objects
    are returned.
  }
  \item{envir}{
    environment where the objects are put, the default is the
    environment of the caller.
  }
}
\details{

  The requested objects are created and put in \code{envir}.  Its
  default is the environment of the caller, which should be sufficient
  in most use cases.

  The following objects are currently available:
  \Sexpr[stage=build,results=rd]{paste0("\\\\code{", pcts::pcts_exdata(NA), "}", collapse = ", ")}.
  
}
\value{
  if \code{x} is \code{NA}, the names of the available objects.
  Otherwise the function is called for the side effect of creating
  objects in \code{envir} and the return value (the names of the created
  objects) is usually discarded.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
%\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{dataFranses1996}}
}
\examples{
## the objects are created with something like:
ap <- pcts(AirPassengers)
ap7to9 <- window(ap, seasons = 7:9)

pcfr <- pcts(dataFranses1996)
pcfr2to4 <- pcfr[2:4]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{datasets}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
