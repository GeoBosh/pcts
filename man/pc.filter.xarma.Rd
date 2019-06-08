\name{pc.filter.xarma}
%\alias{pc.filter.arma}
\alias{pc.filter.xarma}
\title{ Filter time series with periodic arma filters }
\description{
  Filter time series with periodic arma filters with or options for
  periodic and non-periodic intercepts.
}
\usage{
pc.filter.xarma(x, eps, phi, theta, period, p, q, n, from,
              seasonof1st = 1, intercept = NULL, nintercept = NULL)
}
\arguments{
  \item{x}{ the time series to be filtered, a vector. }
  \item{eps}{ the innovations, a vector.  }
  \item{phi}{ the autoregression parameters, a matrix. }
  \item{theta}{ the moving average parameters, a matrix. }
  \item{period}{ the period (number of seasons in a year). }
  \item{p}{
    the autoregression orders, recycled to \code{period}
    if \code{length(p)=1}.
  }
  \item{q}{
    the moving average orders, recycled to \code{period} if
    \code{length(q)=1}.
  }
  \item{n}{
    a positive  integer, the time index of the last observation to be
    filtered.
  }
  \item{from}{
    a positive  integer, the time index of the first observation to be
    filtered.
  }
  \item{seasonof1st}{
    a positive  integer, the season of the time index of \code{x[1]},
    see Details.
  }
  \item{intercept}{
    the intercepts of the seasons, a vector of length \code{period}.
  }
  \item{nintercept}{
    intercepts, a vector of the same length as \code{x}.
  }
}
\details{
  % pc.filter.arma(x, eps, phi, theta, period, p, q, n, from, seasonof1st = 1)
  \code{pc.filter.xarma} is somewhat lower level.
  The user level function is \code{pc.filter} which uses
  \code{pc.filter.xarma} to do the computations.
  % Similarly, the obsolete \code{pc.armafilter} uses \code{pc.filter.arma}.

  % In \code{pc.filter.arma} values of \code{x[t]} are calculated for
  % \code{t=from,...,n} using the formula
  % \deqn{ x_t
  %   =   \sum_{i=1}^{p_t} \phi  _t(i)x          _{t-i}
  %     + \sum_{i=1}^{q_t} \theta_t(i)\varepsilon_{t-i}
  %     + \varepsilon_t
  %   }{   x(t)
  %   =   \sum_\{i=1\}^\{p_t\} phi(t,i) x(t-i)
  %     + \sum_\{i=1\}^\{q_t\} theta(t,i)e(t-i)
  %     + e(t)
  %   }

  %% 2016-08-15 this is not quite what the function currently does.
  %%            (probably part of the functionality was modev to
  %%            pc.filter() at some stage in the past without updating
  %%            the documentation.)
  % In \code{pc.filter.xarma} values  are first calculated for
  % \eqn{y_t=x_t - \mu_t}{y(t)=x(t)-mean(t)}
  % \code{t=from,...,n}:
  %   \deqn{ y_t
  %   =   c_t
  %     + \sum_{i=1}^{p_t} \phi  _t(i)y          _{t-i}
  %     + \sum_{i=1}^{q_t} \theta_t(i)\varepsilon_{t-i}
  %     + \varepsilon_t
  %   }{   y(t)
  %     = c_t
  %     + \sum_\{i=1\}^\{p_t\} phi(t,i) y(t-i)
  %     + \sum_\{i=1\}^\{q_t\} theta(t,i)e(t-i)
  %     + e(t)
  %   }
  % (where \eqn{c_t}{c(t)} are the intercepts), then the mean is added
  % back, \eqn{x_t=y_t+\mu_t} and the new \code{x} is returned.

  \code{pc.filter.xarma} filters the time series \code{x} by the
  following formula (for \code{t=from,...,n}):
    \deqn{ x_t
    =   c_t
      + \sum_{i=1}^{p_t} \phi  _t(i)x          _{t-i}
      + \sum_{i=1}^{q_t} \theta_t(i)\varepsilon_{t-i}
      + \varepsilon_t,
    }{   x(t)
      = c_t
      + \sum_\{i=1\}^\{p_t\} phi(t,i) x(t-i)
      + \sum_\{i=1\}^\{q_t\} theta(t,i)e(t-i)
      + e(t),
    }
  where \eqn{c_t}{c(t)} is the overall intercept at time \eqn{t}, see
  below.  Values of \code{x[t]} for \code{t} outside the range
  \code{from,n}, if any, are left unchanged. Values for \code{t<from}
  are used as initial values when needed.

  Two intercepts are provided for convenience and some flexibility.  The
  periodic intercept, \code{intercept}, is a vector of length
  \code{period}. It is replicated to length \code{n}, taking care to
  ensure that the first element of the resulting vector, say \eqn{a},
  starts with \code{intercept[seasonof1st]}.  \code{nintercept} can be
  an arbitrary vector of length \code{n}. It can be used to represent
  trend or contributions from covariates.  \code{nintercept} is not
  necessarilly periodic and argument \code{seasonof1st} does not affect
  its use.  The overall intercept is obtained as the sum \code{c = a +
  nintercept}.

  Usually \code{x} is a numeric vector but it can also be a matrix in
  which each column represents the data for one ``year''.
  Also, the length of \code{x} is typically, but not necessarilly, equal
  to \code{n}. It is prudent to ensure that \code{length(x) >= n} and
  this must be done if \code{x} is a matrix.

  % Normally at most one of \code{mean}, \code{xmean}, \code{intercept},
  % and \code{nintercept} will be non-zero, but this is not enforced. In
  % principle any one of them is equivalent to the others, the variants
  % are provided for convenience.

  % \code{mean} is a vector of length \code{period} and is replicated to
  % center the series. \code{xmean} may be an arbitrary vector (not
  % necessarilly periodic) to be used for centering.

  Argument \code{phi} is ignored if \code{p==0},
  argument \code{theta} is ignored if \code{q==0}.

  \code{pc.filter.xarma} is meant to be called by other functions whose
  task is to prepare the arguments with proper checks.  It does not make
  much sense to repeat the checks in \code{pc.filter.xarma}.  In
  particular, no check is made to ensure that \code{from} and \code{n}
  are correctly specified.

  \bold{This is a low level function meant to be used with basic vectors
    and matrices.  TODO: Implement in C/C++.}  In the current
    implementation. it accesses the elements of the arguments with
    straightforward indexing, so objects from classes may be used as
    well, provided that \code{x[t]}, \code{eps[t]}, \code{phi[t,i]},
    \code{theta[t,i]}, as well as assignment to \code{x[t]}, are defined
    for scalar indices.

}
\value{
  Returns \code{x} with \code{x[from]} to \code{x[n]} filled with the
  filtered values and values outside the interval \code{from,...,n}
  left unchanged.

  The mode of \code{x} is left unchanged. In particular, \code{x} may be
  a matrix with each row representing the data for a season. This is
  convenient since periodic time series are often more easily processed
  in this form.

}
% \references{ ~put references to the literature/web site here ~ }
\author{Georgi N. Boshnakov}
\section{Level}{0 (base) }

\seealso{ \code{\link{pc.filter}} }
% \examples{ }
\keyword{pcts}















