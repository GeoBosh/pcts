\name{pc.filter}
\alias{pc.filter}
\alias{pc.armafilter}
\title{ Applies a periodic ARMA filter to a time series }
\description{
  Filter time series with a periodic arma filter.
  If \code{whiten} is \code{FALSE} (default) the function applies
  the given ARMA filter to \code{eps} (\code{eps} is often periodic
  white noise).  If \code{whiten} is \code{TRUE} the function applies
  the \dQuote{inverse filter} to \eqn{x}, effectively computing
  residuals.
}
\usage{
pc.filter(model, x, eps, seasonof1st = 1, from = NA, whiten = FALSE,
          nmean = NULL, nintercept = NULL)
}
% pc.armafilter(x, eps, model, seasonof1st = 1, from = NA, whiten = FALSE)
\arguments{
  \item{x}{ the time series to be filtered, a vector. }
  \item{eps}{ residuals, a vector or NULL. }
  \item{model}{
    the model parameters, a list with components \code{"phi"},
    \code{"theta"}, \code{"p"}, \code{"q"}, \code{"period"},
    \code{"mean"} and \code{"intercept"}, see Details.
  }
  \item{seasonof1st}{
    the season of the first observation (i.e., of x[1]).
  }
  \item{from}{ the index from which to start filtering. }
  \item{whiten}{
    if TRUE use \code{x} as input and apply the inverse filter to
    produce \code{eps} ("whiten" \code{x}), if FALSE use \code{eps} as
    input and generate \code{x} ("colour" \code{eps}).
  }
  \item{nmean}{

    a vector of means having the length of the series, see Details.

  }
  \item{nintercept}{

    a vector of intercepts having the length of the series, see details.

  }
}
\details{
  % The function \code{pc.filter} is similar to \code{pc.armafilter} but
  % in addition handles mean.

  The model is specified by argument \code{model}, which is a list with
  the following components:
  \describe{

    \item{\code{phi}}{ the autoregression parameters,}
    \item{\code{theta}}{ the moving average parameters,}
    \item{\code{p}}{the autoregression orders, a single number or a
      vector with one element for each season,}
    \item{\code{q}}{the moving average orders, a single number or a
      vector with one element for each season,}
    \item{\code{period}}{ number of seasons in a cycle,}
    \item{\code{mean}}{ means of the seasons,}
    \item{\code{intercept}}{intercepts of the seasons.}

  }

  The relation between \code{x} and \code{eps} is assumed to be the
  following. Let
  \deqn{y_t = x_t - mu_t}
  be the mean corrected series, where \eqn{mu_t} is the mean, see below.
  The equation relating the
  mean corrected series, \eqn{y_t=x_t - \mu_t}{y(t)=x(t)-mean(t)}, and
  \code{eps} is the following:
    \deqn{ y_t
    =   c_t
      + \sum_{i=1}^{p_t} \phi  _t(i)y          _{t-i}
      + \sum_{i=1}^{q_t} \theta_t(i)\varepsilon_{t-i}
      + \varepsilon_t
    }{   y(t)
      = c_t
      + \sum_\{i=1\}^\{p_t\} phi(t,i) y(t-i)
      + \sum_\{i=1\}^\{q_t\} theta(t,i)e(t-i)
      + e(t)
    }
  where \eqn{c_t}{c(t)} is the intercept, \code{nintercept}. The inverse
  filter is obtained by writing this as an equation expressing
  \eqn{\varepsilon_t} through the remaining quantities.

  If \code{whiten = TRUE}, \code{pc.filter} uses the above formula to
  compute the filtered values of \code{x} for \code{t=from,...,n},
  i.e. whitening the time series if \code{eps} is white noise.  If
  \code{whiten = FALSE}, \code{eps} is computed, i.e. the inverse filter
  is applied \code{x} from \code{eps}, i.e. ``colouring'' \code{x}.
  In both cases the first few values in \code{x} and/or \code{eps} are
  used as initial values.


  Essentially, the mean is subtracted from the series to obtain the
  mean-corrected series, say \code{y}. Then either \code{y} is filtered
  to obtain \code{eps} or the inverse filter is applied to obtain
  \code{y} from \code{eps}
  finally the mean is added back to \code{y} and the result returned.

  The mean is formed by \code{model$mean} and argument \code{nmean}.  If
  \code{model$mean} is supplied it is recycled periodically to the
  length of the series \code{x} and subtracted from \code{x}. If
  argument \code{nmean} is supplied, it is subtracted from \code{x}. If
  both \code{model$mean} and \code{nmean} are supplied their sum is
  subtracted from \code{x}.

  The above gives a vector \code{y}, \eqn{y_t=x_t -
  \mu_t}{y(t)=x(t)-mean(t)}, which is then filtered.  If the mean is
  zero, \eqn{y_t=x_t}{y[t]=x[t]} in the formulas below.


  Finally, the mean is added back, \eqn{x_t=y_t+\mu_t}, and the new
  \code{x} is returned.

  The above gives a vector \code{y} which is used in the filtering.  If
  the mean is zero, \eqn{y_t=x_t}{y[t]=x[t]} in the formulae below.

  \code{pc.filter} can be used to simulate pc-arma series with the
  default value of \code{whiten=FALSE}. In this case \code{eps} is the
  input series and \code{y} the output.
  \deqn{ y_t
    = c_t
      + \sum_{i=1}^{p_t} \phi  _t(i)y          _{t-i}
      + \sum_{i=1}^{q_t} \theta_t(i)\varepsilon_{t-i}
      + \varepsilon_t
    }{   y(t)
    = c(t) + \sum_\{i=1\}^\{p_t\} phi(t,i) y(t-i)
      + \sum_\{i=1\}^\{q_t\} theta(t,i)e(t-i)
      + e(t)
    }
  Then \code{model$mean} or \code{nmean} are added to \code{y} to form the
  output vector \code{x}.

  Residuals corresponding to a series \code{y} can be obtained by
  setting \code{whiten=TRUE}. In this case \code{y} is the input series.
  The elements of the output vector \code{eps} are calculated by the
  formula:
  \deqn{ \varepsilon_t
    = - c_t
      - \sum_{i=1}^{q_t} \theta_t(i)\varepsilon_{t-i}
      - \sum_{i=1}^{p_t} \phi  _t(i)y          _{t-i}
      + y_t
    }{   e(t)
    = - c(t)
      - \sum_\{i=1\}^\{q_t\} theta(t,i)e(t-i)
      - \sum_\{i=1\}^\{p_t\} phi(t,i) y(t-i)
      + y(t)
    }
  There is no need in this case to restore \code{x} since \code{eps} is
  returned.

  In both cases any necessary initial values are assumed to be already
  in the vectors. If \code{from} is not supplied it is chosen as the
  smallest \code{i} such that for all \eqn{t\ge i}{t >= i},
  \code{t-p[t]>0} and \code{t-q[t]>0}, i.e.  the filter will not require
  negative indices for \code{x} or \code{eps}.

  \code{pc.filter} calls the lower level function \code{pc.filter.xarma}
  to do the computation.

}
\value{
  The filtered series: the modified \code{x} if \code{whiten=FALSE}, the
  modified \code{eps} if \code{whiten=TRUE}.
}
%\references{ }
\author{Georgi N. Boshnakov}
%\note{ }
\section{Level}{1}
\seealso{
  the lower level functions
  \code{\link{pc.filter.xarma}} % and \code{\link{pc.filter.arma}}
  which do the computations
}
%\examples{ }
\keyword{pcts}% at least one, from doc/KEYWORDS
