\name{pcApply-methods}
\docType{methods}

\alias{pcApply}
\alias{pcApply-methods}
\alias{pcApply,matrix-method}
\alias{pcApply,numeric-method}
\alias{pcApply,PeriodicMTS-method}
\alias{pcApply,PeriodicTS-method}

\title{Apply a function to each season}
\description{Apply a function periodically}
\usage{
pcApply(object, \dots)

\S4method{pcApply}{numeric}(object, nseasons, FUN, \dots)

\S4method{pcApply}{matrix}(object, nseasons, FUN, \dots)

\S4method{pcApply}{PeriodicTS}(object, FUN, \dots)

\S4method{pcApply}{PeriodicMTS}(object, FUN, \dots)
}
\arguments{
  \item{object}{an object for which periodic mean makes sense.}
  \item{nseasons}{number of seasons.}
  \item{FUN}{a function, as for \code{\link[base]{apply}}. }
  \item{\dots}{further arguments for \code{FUN}.}
}
\details{
  
  For univariate periodic time series, \code{pcApply} applies \code{FUN}
  to the data for each season. For multivariate periodic time series,
  this is done for each variable.

  The methods for \code{"numeric"} and \code{"matrix"} are equivalent to
  those for \code{"PeriodicTS"} and \code{"PeriodicMTS"},
  respectively. The difference is that the latter two don't need
  argument \code{nseasons} and take the names of the seasons from
  \code{object}.

  Argument \code{"..."} is for further arguments to \code{FUN}. In
  particular, with many standard \R functions argument \code{na.rm =
  TRUE} can be used to omit NA's, see the examples.

  In the univariate case, when  \code{length(object)} is an integer
  multiple of the number of seasons the periodic mean is equivalent to
  \code{apply(matrix(object, nrow = nseasons), 1, FUN, ...)}.
  
}
\value{
  numeric or matrix for the methods described here, see section
  Details. 
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\section{Methods}{
\describe{

\item{\code{signature(object = "matrix")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "PeriodicMTS")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "PeriodicTS")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
  \code{\link{pcMean}},
  \code{\link[base]{apply}}
}
\examples{
pcApply(pcts(presidents), mean, na.rm = TRUE)
pcMean(pcts(presidents), na.rm = TRUE) # same

pcApply(pcts(presidents), median, na.rm = TRUE)
pcApply(pcts(presidents), var, na.rm = TRUE)
pcApply(pcts(presidents), sd, na.rm = TRUE)

pcfr2to4 <- pcts(dataFranses1996)[2:4]
pcApply(pcfr2to4, median, na.rm = TRUE)
pcApply(pcfr2to4, sd, na.rm = TRUE)
}
\keyword{methods}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
