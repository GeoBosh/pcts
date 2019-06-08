\name{sim_pc}
\alias{sim_pc}
\title{Simulate Periodically Correlated ARMA Series }
\description{
  Simulate a realization of a periodically correlated arma model
  or a continuation of an existing series.
  Initial values may be given too.
%   If initial values are given these are used to start the
%   simulation. To simulate a continuation of an existing series give it
%   as \code{xbefore} argument, usually together with an \code{innovbefore} argument.
}
\usage{
sim_pc(model, n = NA, randgen = rnorm, seasonof1st = 1, nepochs = NA,
              n.start = NA, x, eps, nmean = NULL, nintercept = NULL, ...)
}
\arguments{
  \item{model}{ a list with elements
    \code{phi}, \code{theta}, \code{p}, \code{q}, \code{period},
    \code{mean}, \code{intercept},
    specifying the model. }
  \item{n}{ length of the series. }
  \item{randgen}{ random number generator as required by
    \code{\link{sim_pwn}}. }
 %\item{innov}{ a vector of innovations (\code{randgen} is ignored if \code{innov} is given) }
 %\item{xinit}{ initial (starting) values of the series }
 %\item{innovinit}{ initial (starting) values of the innovations }
 %\item{xbefore}{ Past values of the series }
 %\item{innovbefore}{ Past values of the innovations }
  \item{seasonof1st}{ season of the first value. }
  \item{nepochs}{

    number of epochs; if \code{nepochs} is given, then \code{n} is
    computed as \eqn{nepochs * period}. }

  \item{n.start}{
    burn-in number; generate \eqn{\code{n.start + n}} observations and
    discard the first \code{n.start} of them, see Details.
  }
  \item{x}{\emph{initial} or \emph{before} values, see Details.}
  \item{eps}{innovations, see Details. }
  \item{nmean}{a vector of length \code{n} of means, see Details.}
  \item{nintercept}{
    a vector of length \code{n} of intercepts, see Details.
  } 
  \item{\dots}{
    any additional arguments to be passed on to \code{sim_pwn}.
  }
}
\details{
  
  Argument \code{x} can be used to specify two types of initialisation
  values - `before' and `init'. They are used similarly in computations
  but `before' values are not included in the result, while `init'
  values are (unless dropped due to \code{n.start}). `Before' values
  provide a convenient way to simulate continuation trajectories for a
  time series, for example for simulation based prediction intervals.
  
  If \code{x} is \code{"numeric"}, it represents `before'
  values. Alternatively, \code{x} can be a list with components
  \code{"before"} and \code{"init"}.

  Innovations are usually generated with the random number generator
  specified by \code{randgen} (with default \code{rnorm}) and the
  \code{...} parameters by a call to the function \code{sim_pwn}, see
  the documentation for \code{sim_pwn} for various ways to control the
  distribution of the generated sequence.

  The innovations can also be generated in advance and supplied using
  argument \code{eps}. If \code{eps} is numeric, it is taken to
  represent the innovations. Alternatively, \code{eps} can be a list
  with the innovations in component \code{"main"}. This list may also
  contain components \code{"before"} and/or \code{"init"} specifying
  `before' or `initial' values, with interpretation as for \code{x}.

  \code{nintercept} can be used to specify trend representing the effect
  of time and/or covariates. As for \code{eps}, if it is numeric it is
  taken to represent the main values. It can also be a list with
  components \code{before}, \code{init}, and \code{main}. 

  To avoid ambiguity, let's reiterate that \emph{before} values are past
  values of the corresponding quantity (before the start of the
  simulated series), while \emph{init} values are "initial" values. In
  particular, if initial values are specified for \code{x}, these will
  form the start of the generated series (unless \code{n.start} leads to
  them being discarded).

  If \emph{before} values are specified for the series and the
  innovations, then they play a role analogous to that of initial
  values, so it does not make much sense to supply also \emph{initial}
  values.

  The function effectively does the following.
  \code{innov} is generated if not supplied,
  a vector of innovations is created
                 \code{eps <- c(innovbefore,innovinit,innov)},
  a vector \code{x} is created of the same length as \code{eps},
		 and initialised with \code{xbefore} and \code{xinit}.
  If there are no initial or before values, these are assumed to be 0.
  The remaining values of \code{x} are filled using the pc-arma equations.
  Finally, the \code{xbefore} values are discarded as well as the first
  \code{n.start} values.

  \code{n.start} should usually be a multiple of the period since
  otherwise the first observation in the returned vector will not
  correspond to \code{seasonof1st}.

  \code{sim_pc} deals mainly with the interpretation of the parameters.
  The actual computations are done by \code{pc.filter}. Moreover,
  \code{sim_pc} does not look at the \code{model}. It knows only about
  \code{model$period} and uses it to compute \code{n} if \code{n} is not
  specified.
  (It probably should not care even about this.)

}
\value{
  numeric, the simulated time series
}
%\references{ }
\author{Georgi N. Boshnakov}
% \note{  }
\section{To do}{option to return the innovation sequence;
  option to include the before values.

  option to return the season of the first value in the returned series
  (it may be different from seasonof1st due to \code{n.start}).
  }

\seealso{ \code{\link{sim_pwn}}, \code{\link{pc.filter}} }
\examples{
m1 <- rbind( c(1, 0.81, 0), c(1, 0.4972376, 0.4972376) )
testphi <- slMatrix( init = m1 )

m2 <- rbind( c(1, 0, 0), c(1, 0, 0) )
testtheta <- slMatrix( init = m2 )

## phi and theta are slMatrix here.
mo1 <- list(phi = testphi, theta = testtheta, p = 2, q = 2, period = 2)
set.seed(1234)
a1 <- sim_pc(mo1, 100)

## phi and theta are ordinary matrices here.
mo2 <- list(phi = m1[ , 2:ncol(m1)], theta = m2[ , 2:ncol(m2)], p = 2, q = 2, period = 2)
set.seed(1234)
a2 <- sim_pc(mo2, 100)
identical(a1, a2)

## Lina's PAR model
parcoef    <- rbind(c(0.5, -0.06), c(0.6, -0.08),
                    c(0.7, -0.1),  c(0.2, 0.15) )
picoef1    <- c(0.8, 1.25, 2, 0.5)
parcoef2   <- pi1ar2par(picoef1, parcoef)

picoef2    <- c(4, 0.25, 5, 0.2)
coefper2I2 <- pi1ar2par(picoef2, parcoef2)

#### specify the model using multi-companion approach
mc2I2       <- mcompanion::mc_from_filter(coefper2I2)
co2I2       <- eigen(mc2I2)$vectors
co2I2
m2I2 <-  mcompanion::sim_pcfilter(period = 4, n.root = 4,
                 eigabs = c(1, 0.036568887, 0.001968887),
                 eigsign = c(1, 1, -1),
                 len.block = c(2, 1, 1),
                 type.eigval  =  c("r", "r", "r"),
                 co = cbind(co2I2[ ,1], rep(NA, 4), co2I2[,3:4]))
m2I2$pcfilter
perunit2mc  <- sim_pc(list(phi = m2I2$pcfilter, p = 4, q = 0, period = 4), 500)
plot(perunit2mc)
plot(perunit2mc, type = "p")

# todo: give example with sigmat^2 !!!
}
\keyword{datagen}
\keyword{pcarma}
