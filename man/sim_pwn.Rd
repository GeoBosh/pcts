\name{sim_pwn}
\alias{sim_pwn}
\title{Simulate periodic white noise}
\description{
  Generates a sample from periodic white noise.
}
\usage{
sim_pwn(n = 100, period = NA, seasonof1st = 1, scale = NULL, 
        shift = NULL, f = rnorm, \dots)
}
\arguments{
  \item{n}{length of the generated sample.}
  \item{period}{number of seasons in an epoch.}
  \item{seasonof1st}{season of the first observation in the result.}
  \item{scale}{
    scale the series by this amount, a vector of length \code{period} or 1.
  }
  \item{shift}{
    shift the series by this amount, a vector of length \code{period} or 1.
  }
  \item{f}{
    a function or list of functions to generate random numbers.
  }
  \item{\dots}{
    arguments for the random number generator(s) specified by \code{f}.
  }
}
\details{

  First a series, say \eqn{x}, of random numbers is generated as
  requested by the argument \code{f}. Then, if \code{shift} and/or
  \code{scale} are supplied, the values are modified as follows:

  \deqn{y_t = shift_{k} + scale_{k} x_t}{y[t] = shift[k] + scale[k] x[t],}

  where  \eqn{k} is the season corresponding to time \eqn{t}.
  The vector \eqn{y} is returned.

  If \code{f} is a single a function (or name of a function), then the
  series is generated (effectively) by the call \code{f(n,...)}.

  The argument \code{f} may also be a list whose \eqn{k}th element
  is itself a list specifying the random number generator for the
  \eqn{k}th season. The first element being the function (such as
  \code{rnorm}) and the remaining elements being parameters for that
  function.  Parameters common to all seasons may be supplied through
  the \dots{} argument.

  The argument \code{period} may be omitted. In that case it is inferred
  from \code{f} and/or the lengths of \code{shift} and
  \code{scale}. Currently there is no check for consistency here.

  The arguments \code{shift} and \code{scale} may be used to specify
  simple linear transformations of the generated values, possibly
  different for the different seasons. Each of them should be a vector
  of length \code{period} or one.

  \code{seasonof1st} can be used to request the simulated time series to
  start from a season other than the first one.  Note that whatever the
  value of \code{seasonof1st}, the first elements of \code{scale},
  \code{shift} and \code{f} (if a list) are taken to refer to season
  one.

}
\value{
  A vector of length \eqn{n} representing a realization of a periodic
  white noise series. The season of the first observation is
  \code{seasonof1st}.
}
%\references{ }
\author{Georgi N. Boshnakov}

%%% \note{ }
%%%
%%%  ~Make other sections like Warning with \section{Warning }{....} ~
%%%
\section{ Level }{ 0 (base) }

%\seealso{
%       \code{\link{pcAcvf}},
%       \code{\link{pcAcvf-class}}
%}

\examples{
## three equivalent ways to specify periodic white noise with
## normal innovatios, 2 seasons, s.d. = 0.5 for season 1, and 2 for season 2
sim_pwn(100, f = rnorm, scale = c(0.5, 2))
sim_pwn(n = 100, scale = c(0.5, 2))  # rnorm is the default generator
sim_pwn(100, f = list(c(rnorm, 0, 0.5), c(rnorm, 0, 2)))
}
\keyword{datagen}
