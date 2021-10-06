\name{pcarma_solve}
\alias{pcarma_acvf_lazy}
\alias{pcarma_h_lazy}
\alias{pcarma_acvf_system}
\alias{pcarma_param_system}
\alias{pcarma_h}

\title{
  Functions to compute various characteristics of a PCARMA model

}
\description{

  Given a PCARMA model, create a function for computing autocovariances
  or coefficients of the corresponding infinite moving average
  representation or prepare the linear system whose solution provides
  the first few autocovariances of the model.

}
\usage{
pcarma_acvf_lazy(phi, theta, sigma2, p, q, period, maxlag = 100)
pcarma_h_lazy(phi, theta, p, q, period, maxlag = 200)
pcarma_acvf_system(phi, theta, sigma2, p, q, period)
pcarma_param_system(acf, h, sigma2, p, q, period)
pcarma_h(h, na = NA)
}
\arguments{
  \item{phi}{
    the autoregression parameters, an object of class \code{"slMatrix"}
  }
  \item{theta}{
    the moving average parameters, an object of class \code{"slMatrix"}
  }
  \item{sigma2}{the innovation variances, an object of class
    \code{"PeriodicVector"} or  a vector of size \code{period}, Details.
  }
  \item{p}{
    the (maximal) autoregression order or the autoregression orders.
  }
  \item{q}{
    the (maximal) moving average order or the moving average orders.
  }
  \item{period}{ number of seasons in an epoch }
  \item{maxlag}{
    maximal lag for which the result is stored internally.
  }

  \item{acf}{ the autocovariance function, an object of class pcAcvf,
    slMatrix, or similar }
  \item{h}{  \code{pcarma_param_system},
    \code{h(t,k)}  is expected to return the coefficient
    \eqn{h_{t,k}}{h(t,k)}. \code{h} is usually created by
    \code{pcarma_h_lazy}.
    For \code{pcarma_h},  a matrix of h(t,i) coefficients.
  }
  \item{na}{
    not used currently, controls what to do for large lags.
  }


}
\details{

  \subsection{Compute acvf from parameters}{

    \code{pcarma_acvf_lazy} creates a function that will compute (on
    demand) values of the acf by a recursive formula. Computed values
    are stored internally for lags up to \code{maxlag}.

  }

  \subsection{System for acvf from parameters}{

    \code{pcarma_acvf_system} forms a linear system for the calculation
    of autocovariances from the parameters of a pc-arma model.  The
    argument \code{theta} is not used if \eqn{q=0} and \code{phi} is not
    used if \eqn{p=0}.

  }

  \subsection{System for parameters from acvf}{
    \code{pcarma_param_system} takes
    the periodic autocovariances of a pc-arma model and computes a
    matrix and a vector representing the linear system whose solution
    provides the parameters of the model.

    Scalar \code{p} specifies the same autoregression order for each
    season, similarly for \code{q}.  \code{p} and \code{q} may be
    vectors of length \code{period} specifying the order for each season
    individually. In the latter case the solution of the system may not
    be a proper model or, if it is, its autocovariances may not be the
    ones used here! See the references for details.

    The class of \code{acf} is not required to be one of those
    explicitly listed above, but it should understand their indexing
    conventions, similarly for \code{sigma2}.

    For pure autoregression, \eqn{q=0}, the arguments \code{h} and
    \code{sigma2} are ignored. \strong{TODO: add sigma2 (if supplied) to
    the returned list?}
  }

  \subsection{Compute h from parameters}{

    \code{pcarma_h_lazy}: h(t,i) are the coefficients in infinite the
    moving average representation of the pc.arma model. The calculations
    use formula (4.4) from my paper (or elsewhere) with internal storage
    (in an slMatrix) of calculated results (for i<maxlag) and recursive
    calls to itself. So, it is not necessary to compute h(t,i) in any
    particular order.

  }

  \subsection{Infinite MA coefficients(h)}{

    \code{pcarma_h} Function to create a function for lazy computation
    of h(t,i) in pc.arma models

    Takes a matrix of h(t,i) coefficeints and returns a function that
    calculates h(t,i) from my paper xxx. The returned value can be used
    in the same way as that of \code{pcarma_h_lazy}.

  }

}
\value{

  \subsection{for pcarma_acvf_lazy}{

    a function taking two arguments \code{t} and \code{k} such that for
    scalar \code{t} and \code{k} the call \code{f(t,k)} will return
    EX(t)X(t-k). If either of the arguments is a vector, then
    \code{f(t,k)} returns a matrix of size (\code{length(t),length(k)})
    containing the respective autocovariances.

  }

  \subsection{for pcarma_h_lazy}{

    a function, say \code{h}. In calls to \code{h}, if both arguments
    are scalars \code{h(t,i)} returns $h_{t,i}$. If at least one of the
    arguments is a vector a matrix of values of $h$ is returned.

  }

  \subsection{for pcarma_acvf_system}{

    a list with two components representing the linear system:
    \describe{
      \item{A}{
	The
	\eqn{(p+1)\mbox{period}\times(p+1)\mbox{period}}{(p+1)*period x (p+1)*period}
	matrix of the system, an object of class "matrix".
      }
      \item{b}{
	The right-hand side of the system,  a vector of length
	\eqn{(p+1)\mbox{period}}{(p+1)*period}, an object of class "vector".
      }
    }
    \eqn{A^{-1}b}{A^(-1)b} can be used to get a vector of the
    autocovariances in the following order (d is the period, p is the
    maximal AR order):
    \deqn{K(1,0),...,K(d,0), K(1,1),...,K(d,1),...,K(1,p),...,K(d,p).}

  }

  \subsection{for pcarma_param_system}{

    A list with components representing the linear system and the AR and
    MA orders:
    \describe{
      \item{A}{The matrix of the system}
      \item{b}{The right-hand side of the system}
      \item{p}{The AR order}
      \item{q}{The MA order}
    }

    \eqn{A^{-1}b}{A^(-1)b} will return a vector of the parameters of the
    pc-arma model: all parameters for the first season, followed by all
    parameters for the second seasons and so on. For each season the
    parameters are in the following order (s is the current season, d is
    the period, \eqn{p[s]} and \eqn{q[s]} are the corresponding AR and
    MA orders):
    \deqn{
      \sigma^2(s), \phi(s,1),...,\phi(s,p[s]),\theta(s,1),...,\theta(s,q[s]).
    }
  }

  \subsection{for pcarma_h}{

    a function, say \code{h}. In calls to \code{h}, if both arguments
    are scalars \code{h(t,i)} returns $h_{t,i}$. If at least one of the
    arguments is a vector a matrix of values of $h$ is returned.
    Analogous to \code{pcarma_h_lazy}.

  }

}
\references{
  \insertRef{boshnakov1996pcarma}{pcts}
}
\author{Georgi N. Boshnakov}
\note{

  for \code{pcarma_acvf_lazy}: The recursion may become extremely slow for lags greater than
  \code{maxlag}. If large lags are likely to be needed the argument
  \code{maxlag} should be used to increase the internal storage. The
  default for \code{maxlag} currently is 100.

}
\seealso{
  % \code{\link{pcarma_h_lazy}}
  \code{\link{pcarma_h}},
  \code{\link{pcarma_param_system}}
}
\examples{
## periodic acf of Lambert-Lacroix
data(ex1f)
(pc3 <- slMatrix(period = 2, maxlag = 5, f = ex1f, type = "tt"))
## find the parameters
s3 <- pcarma_param_system(pc3, NULL, NULL, 2, 0, 2)
coef3 <- solve(s3$A, s3$b)
pcarma_unvec(list(p = 2, q = 0, period = 2, param = coef3))

## actually, the model is PAR(1,2):
s3a <- pcarma_param_system(pc3, NULL, NULL, c(1, 2), 0, 2)
coef3a <- solve(s3a$A, s3a$b)
pcarma_unvec(list(p = c(1,2), q = 0, period = 2, param = coef3a))


## prepare test parameters for a PAR(2) model with period=2.
##   (rounded to 6 digits from the above example.
m1 <- rbind(c(1, 0.81, 0), c(1, 0.4972376, 0.4972376) )
m2 <- rbind(c(1, 0, 0), c(1, 0, 0) )
testphi <- slMatrix(init = m1)
testtheta <- slMatrix(init = m2)
si2 <- PeriodicVector(c(0.3439000, 0.1049724)) #     # or si2 <- c(1,1)

## acf from parameters
myf <- pcarma_acvf_lazy(testphi, testtheta, si2, 2, 0, 2, maxlag = 110)
myf(1,4)        # compute a value
a1 <- myf(1:2,0:9)    # get a matrix of values

## h from parameters
h <- pcarma_h_lazy(testphi, testtheta, 2, 2, 2)
h(3, 2)           # a scalar
h1 <- h(1:2, 1:4) # a matrix

## compute acvf from parameters
( acfsys <- pcarma_acvf_system(testphi, testtheta, si2, 2, 0, 2) )
acfvec <- solve(acfsys$A, acfsys$b)
acf1 <- slMatrix(acfvec, period = 2)

## TODO: examples wirh q != 0
}
\keyword{permodel}
\keyword{pcarma}
