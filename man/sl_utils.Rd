\name{sl_utils}
%\alias{pc.omitneg}
\alias{toSeason}
\alias{toSeasonPair}
\alias{ttTosl}
\alias{ttmatToslPairs}
\title{ Functions for some basic operations with seasons}
\description{Functions for some basic operations with seasons.}
\usage{
%pc.omitneg(lags, maxlag)
toSeason(t, period, t1 = 1, from = 1)
toSeasonPair(t, s, period, ...)
ttTosl(r, period)
ttmatToslPairs(i, j, period)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
%  \item{lags}{ todo: describe! }
%  \item{maxlag}{ todo: describe! }

  \item{r}{covariance matrix, see `Details'.}

  \item{t}{a vector of integers, representing times.}
  \item{s}{a vector of integers, representing times.}

  \item{period}{the number of seasons.}
  \item{t1}{time corresponding to the first season, an integer number.}
  \item{from}{1 or 0, depending on whether the season numbers start from
    1 or 0.}

  \item{i}{a vector of integers.}
  \item{j}{a vector of integers.}

  \item{\dots}{ todo: describe! }

}
\details{

 \code{ttmatToslPairs(i,j,period)} transforms time-time pairs to
 season-lag pairs. The time pairs are obtained by pairing each element
 of \code{i} with each element of \code{j}. A four column matrix is
 created with one row for each pair \code{(t,s)}, such that
 \code{t=i[m]} and \code{s=j[n]} for some \code{m} and \code{n}.  The
 row is \code{m, n, s, l}, where \code{(s,l)} is the season-lag pair
 corresponding to \code{(t,s)}.

 \code{ttTosl(r,period)} converts autocovariances given in a covariance
 matrix (i.e. in ``tt'' form) to the ``sl'' form. The result is a
 \code{period x (maxlag+1)} matrix, where \code{maxlag} is the maximal
 lag available in \code{r}. Entries for which no values are available
 are filled with \code{NA}'s.  Warning is given if contradictory entries
 are found (i.e. if \code{r} is not from a periodically correlated
 process with the given period).

 \code{toSeason(t,period,t1=1,from=1)} returns the season corresponding
 to \code{t}.  \code{t1} is a time (integer) whose season is the first
 season, \code{from} is 1 if the numbering of seasons is 1,2,...,period,
 or 0 if the numbering of seasons is 0,1,...,period-1.  Other values for
 \code{from} are not admissible (but not checked).  \strong{Note:} some
 of the functions in this package implicitly assume that \code{t1=1} and
 \code{from=1}.

 \code{toSeasonPair(t,s,period)} converts the ``tt'' pair \code{t,s} to
 ``sl'' pair and returns the result in the form of a list with elements
 \code{season} and \code{lag}. Currently \code{t} and \code{s} must be
 scalars.

 \code{pc.omitneg} helps to implement dropping of negative indices in
 season-lag objects. It returns its first argument, \code{lags}, if all
 of its elements are non-negative. Otherwise, all elements of
 \code{lags} must be non-positive. In this case the function creates the
 vector \code{0:maxlag} and drops the elements specified by lags.  Note
 that the default indexing will not work properly since zero elements in
 an index are omitted (and there are such indices in season-lag
 objects).

}
\value{

  for \code{ttmatToslPairs}, a matrix with four columns;

  for \code{ttTosl}, a matrix with \code{period} rows;

  for \code{toSeason(t,period,t1=1,from=1)}, a vector of integers;

  for \code{toSeasonPair(t,s,period)}, a list with elements
  \code{season} and \code{lag};

  for \code{pc.omitneg}, a vector of lags (non-negative integers).
}
\references{
  % bibentry: boshnakov2009gen
Boshnakov GN and Iqelan BM (2009).
``Generation of time series models with given spectral
properties.''
\emph{J. Time Series Anal.}, \bold{30}(3), pp. 349--368.
ISSN 0143-9782, \url{http://dx.doi.org/10.1111/j.1467-9892.2009.00617.x}.
% end:bibentry:  boshnakov2009gen
}
\author{Georgi N. Boshnakov}
\note{

  2013-10-24 - Corrected the description of the return value of
  \code{ttmatToslPairs}. It incorrectly stated that the first two
  columns are "tt" pair (they are actually indices in \code{i} and
  \code{j}).

}
%\seealso{ \code{\link{pcAcvf}} }
\examples{
# ttmatToslPairs
ttmatToslPairs(3, 3, 4)  # 1, 1, 3, 0
ttmatToslPairs(3, 2, 4)  # 1, 1, 3, 1

ttmatToslPairs(1:4, 1:4, 4)

ttmatToslPairs(3:4, 3:4, 4)

# ttTosl -  :todo:

# toSeason
toSeason(1:10, 4)           # 1 2 3 4  1 2 3 4  1 2
toSeason(1:10, 4, from = 0) # 0 1 2 3  0 1 2 3  0 1

## first data is for 3rd quarter
toSeason(1:10, 4, t1 = 3)  # 3 4 1 2 3 4 1 2 3 4

# toSeasonPair
toSeasonPair(3, 3, period=4) # season=3, lag = 0
toSeasonPair(8, 8, period=4) # season=4, lag = 0

toSeasonPair(3, 2, period=4) # season=3, lag = 1
toSeasonPair(7, 6, period=4) # same

#### # pc.omitneg
#### pc.omitneg(0:5,10) # 0:5, unchaged since all values >= 0
#### 
#### pc.omitneg(-(0:5),10) # 6:10, works like
#### (0:10)[-(0:5 +1)]     # same
#### 
#### # don't mix positive and negative numbers in pc.omitneg
#### \dontrun{pc.omitneg(c(0,2,3,-4,5), 10)}
}
%\keyword{}% at least one, from doc/KEYWORDS
