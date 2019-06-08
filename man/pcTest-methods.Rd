\name{pcTest-methods}
\docType{methods}
\alias{pcTest}
\alias{pcTest-methods}
\alias{pcTest,ANY,ANY-method}
\alias{pcTest,ANY,character-method}
\alias{pcTest,slMatrix,character-method}
\alias{pcTest,numeric,character-method}
\alias{pcTest,PeriodicTimeSeries,character-method}
\title{Test for periodicity}
\description{Test for periodicity}
\usage{
pcTest(x, nullmodel, nseasons, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object to be tested, e.g. a time series or a periodic acf}
  \item{nullmodel}{specification of the test to be performed}
  \item{nseasons}{number of seasons}
  \item{\dots}{additional arguments to be passed on to methods}
}
\details{

  This is a generic function which acts as a despatcher for various
  tests for periodicity and periodic correlation.

  \code{x} is typically a time series but conceptually it is an object
  containing the statistics needed for carrying out the requested test.
  For example, \code{x} may be the periodic autocovariance function for
  tests based on sample autocorrelations and autocovariances.

  The method with signature \code{(x = "ANY", nullmodel = "character"}) may
  be considered as default for \code{pcTest}.  The ``real'' default
  method simply prints an error message.

}
\value{
  a list containing the results of the requested test,
  see the individual methods for details
}

\section{Methods}{
\describe{

%%% \item{\code{signature(x = "ANY", nullmodel = "ANY")}}{
%%% %%  ~~describe this method here~~
%%% }

\item{\code{signature(x = "ANY", nullmodel = "character")}}{
  Argument \code{nullmodel} specifies the test to be performed.
  It should be a single character string. If it is one of
  the strings recognised by this method,  the test specified below is
  carried out. Otherwise \code{nullmodel} is taken to be the name of a
  function which is called with arguments \code{(x,...)}.

  Currently, the following character strings are recognised:
  \describe{

    \item{"wn"}{Box test for (non-periodic) white noise, simply calls \code{Box.test}.}
    \item{"piar"}{Franses (1996) test for periodic integration.}
  }
}

\item{\code{signature(x = "slMatrix", nullmodel = "character")}}{
  \code{x} here is the periodic autocovariance function. This method
  works similarly to the method for signature \code{(x = "ANY",
  nullmodel = "character")}, see its description.

  Currently, the following character strings are recognised:
  \describe{

    \item{"pwn"}{Ljung-Box test for periodic white noise,}
    \item{"periodicity"}{McLeod test for periodic correlation.}
  }
}
\item{\code{signature(x = "numeric", nullmodel = "character")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "PeriodicTimeSeries", nullmodel = "character")}}{{
%%  ~~describe this method here~~
}
}}}

%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
\note{
  TODO: critical values
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{test_piar}},
  \code{\link{pwn_McLeodLjungBox_test}}
  \code{\link{periodic_acf1_test}}
}


\examples{
cu <- pcts(dataFranses1996[ , "CanadaUnemployment"])
cu <- window(cu, start = availStart(cu), end = availEnd(cu))

test_piar(cu, 4, 1, sintercept = TRUE)
pcTest(cu, "piar", 4, 1, sintercept = TRUE)

if(require(partsm)){
# same with LRurpar.test from partsm
LRurpar.test(cu, list(regular = c(0,0,0), seasonal = c(1,0), regvar = 0), p = 1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
\keyword{pcstat}
