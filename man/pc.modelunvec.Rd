\name{pcarma_unvec}
\alias{pcarma_unvec}
\alias{pcarma_tovec}
\alias{pcarma_prepare}
\title{
  Functions for work with a simple list specification of pcarma models
}
\description{
  Handle a simple list specification of pcarma models.
  Functions to convert to and from a representation appropriate for
  handing on to optimisation functions.
}
\usage{
pcarma_prepare(model, type)
pcarma_unvec(model)
pcarma_tovec(model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{specification of a pcarma model, a list, see Details.}
  \item{type}{not used.}
}
\details{

  These functions work with a specification of a pcarma model as a list
  with components \code{period}, \code{p}, \code{q}, \code{param},
  \code{phi}, \code{theta} and \code{si2}, see also section `Values'.
  The functions do not necessarily need or examine all these components.

  Argument \code{model} is a list with components as accepted by
  \code{pcarma_prepare}. Details are below but the guiding rule is that
  there are sensible defaults for absent components.

  \code{pcarma_prepare} gives a standard representation of \code{model}, in
  the sense that it ensures that the model has components \code{period},
  \code{p} and \code{q}, such that \code{p} and \code{q} are vectors of
  length \code{period}. \code{pcarma_prepare}  does not examine any other
  components of the model. (\strong{TODO:} do the same for the
  innovation variance?)

  If \code{model$period} is NULL, \code{pcarma_prepare} sets it to the
  length of the longer of \code{model$p} and \code{model$q}.  If
  \code{model$p} is a scalar it is extended with \code{rep(model$p,
  period)}.  Missing or NULL \code{model$p} is equivalent to
  \code{model$p = 0}.  \code{model$q} is processed analogously.

  The net effect is that \code{period}, \code{p} and \code{q} will be set
  as expected as long as \code{period} is given or at least one of the
  other two is of length equal to the period. A warning is issued if
  \code{period <= 1} (it is all too easy to give scalar values for
  \code{p} and \code{q} and forget to set the period, in which case
  \code{period} will be deduced to be one).

  A number of functions (including \code{pcarma_tovec} and
  \code{pcarma_unvec}) dealing with the list representation of pcarma
  models start by calling \code{pcarma_prepare} to avoid the need for
  handling all possible cases.


  \code{pcarma_tovec} returns a list with components \code{p},
  \code{q} and \code{param}, where \code{param} is a numeric vector
  containing the pcarma parameters and the innovations variances and
  thus is suitable for optimisation functions. Notice that it is
  component param that is a vector. The reason that \code{pcarma_tovec}
  returns a list, is that the caller may need to do further work before
  calling a generic optimisation function. For exampe, it may wish to
  dop the variances from the vector.


  \code{pcarma_unvec(model)} performs the inverse operation. It takes a
  list like that produced by \code{pcarma_tovec} and converts it to a
  detailed list containing the components of the model.

}
\value{

  for \code{pcarma_unvec}, a list with components:
  \item{p}{autoregressive orders, numeric vector}
  \item{q}{moving average orders, numeric vector}
  \item{si2}{innovation variances}
  \item{phi}{autoregressive parameters}
  \item{theta}{moving average parameters}


  for \code{pcarma_tovec},  a list with components:
  \item{p}{autoregressive order}
  \item{q}{moving average order}
  \item{param}{parameters of the model, a numeric vector.
    TODO: give the order of the parameters in the vector!
  }

  for \code{pcarma_prepare}, a list as \code{pcarma_unvec}, see also
  Details.


}
% \references{
% }
\section{Note}{

  The specification and the functions were created ad hoc to get the
  computations going and are not always consistent with other parts of
  the package.

}

%%% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
%%% \examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\author{Georgi N. Boshnakov}
\keyword{pcarma}
