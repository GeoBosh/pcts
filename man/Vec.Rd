\name{Vec}
\alias{Vec}
\alias{tsMatrix}
\alias{tsVector}
\alias{tsVec}
\alias{pcMatrix}
\alias{pcArray}
\alias{pctsArray}
\title{Core data of periodic time series}
\description{

  Extract the core data from a periodic time series as a vector, matrix
  or array.

}
\usage{
Vec(x, \dots)

tsMatrix(x, \dots)

tsVector(x, \dots)

tsVec(x, \dots)

pcMatrix(x, \dots)

pcArray(x, ndim = 3, \dots)

pctsArray(x, ndim = 3, \dots)
}
\arguments{
  \item{x}{an object.}
  \item{\dots}{further arguments for methods.}
  \item{ndim}{currently not used.}
}
\details{

  These functions give the core data in various common forms.

  The data values can be extracted as a vector from a periodic time
  series object, say \code{x}, with \code{as.vector(x)} or \code{as(x,
  "vector")}.  For multivariate time series the vector returned by
  \code{as.vector(x)} (or \code{as(x, "vector")}) is equivalent to
  \code{as.vector(as.matrix(x))}.

  Similarly, \code{as.matrix()} and \code{as(x, "matrix")}
  extract the data as a matrix containing one column per variable.  
  
  \code{Vec()} is like \code{as.vector()} but the result is a matrix
  with one column (column vector). The default does literally this.
  Thus both, \code{Vec()} and \code{as.vector()}, implement the
  \strong{Vec} operation from matrix calculus but the latter returns the
  result as a vector, not matrix.
  
  The most common representation of data in statistics is matrix-like
  with one column per variable.  The descriptions of algorithms for
  multivariate time series however usually define the vector of
  observations at a given time to be a column vector. In particular,
  implementations of the Kalman filter often require precisely this
  arrangement.  In that case the data matrix is the transposed of the
  more common one and the vectorising operation stacks the observations,
  not the variables.

  The functions \code{tsMatrix()}, \code{tsVector()} and \code{tsVec()}
  provide the analogues of \code{as.vector()}, \code{as.matrix()} and
  \code{Vec()} for the \dQuote{transposed} arrangement.

  These functions may look redundant since they are simple combinations
  of the above and traspose operations. Having functions makes for more
  readable programming. They may be more efficient, as well, for example
  if the underlying time series class stores the data in the transposed
  format.

  \code{pcMatrix()} and \code{pcArray()} also give the core
  data. Effectively, they give an additional dimension to the
  seasons. The season becomes the first dimension since for column
  oriented data the season changes fastest. \code{pcMatrix} is most
  suitable for univariate time series, \code{pcArray()} for
  multivariate. Note that \code{pcArray()} easily extends to multiple
  periodicities although currently (2019-04-19) there are no methods
  that exploit this.

  For univariate time series, in the matrix returned by
  \code{pcMatrix()} each row represents the data for one season and each
  column for one cycle. For multivariate time series, the matrices for
  each variable are put next to each other.

  \code{pcArray()} returns the data as an array, whose last dimension
  corresponds to variables.  In the default case the array is
  3-dimensonal with dimensions (season, year, variable).

  \code{pctsArray()} is a variant of \code{pcArray()} corresponding to
  the arrangement of \code{tsMatrix()}. The ordering of the dimensions
  here is (variable, season, cycle).

  % There was also \code{pctsMatrix()} as a variant of \code{pcMatrix()}
  % but it is not clear what htis should be.

}
\value{
  vector, matrix or array, as indicated by the name of the function and
  described in section \sQuote{Details}. 
}
\author{Georgi N. Boshnakov}
%\note{
%%%  \code{~further notes~~
%}
%
%%% \code{Make other sections like Warning with \section{Warning }{....} _
%
%\seealso{
%%% \code{~objects to See Also as \code{\link{help}}, ~~
%}
\examples{
## window to make number of years different from number of months
ap <- pcts(window(AirPassengers, start = c(1956, 1)))
class( as.vector(ap)    )
class( as(ap, "vector") )

dim( as.matrix(ap)    )
dim( as(ap, "matrix") )

dim( tsMatrix(ap) )

class( tsVector(ap) ) 
dim(   tsVec(ap)    ) 

dim( pcMatrix(ap)   )
dim( pcArray(ap)    )
dim( pctsArray(ap)  )

dfr <- pcts(dataFranses1996)
dim(dfr)                     # c(148, 19)
nSeasons(dfr)                # 4

length(as.vector(dfr))

all.equal(as.vector(dfr)[1:148],       as.matrix(dfr)[ , 1]) # TRUE
all.equal(tsVector(dfr)[1:19],  unname(as.matrix(dfr)[1, ])) # TRUE

dim( as.matrix(dfr) ) # c(148, 19)
dim( tsMatrix(dfr)  ) # c(19, 148)
all.equal(tsMatrix(dfr)[ , 1],  as.matrix(dfr)[1, ]) # TRUE

dim( Vec(dfr)   ) 
dim( tsVec(dfr) ) 
all.equal(tsVec(dfr)[1:19],  unname(as.matrix(dfr)[1, ])) # TRUE

dim( pcMatrix(dfr)   ) # c(4, 703), one row for each season
dim( pcArray(dfr)    ) # c(4, 37, 19), note: 703 == 37*19   

dim( pctsArray(dfr)  ) # c(19, 4, 37), note: 703 == 37*19   
}
\keyword{pcts}
