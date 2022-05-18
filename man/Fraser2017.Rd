\name{Fraser2017}
\alias{Fraser2017}
\docType{data}
\title{Fraser River at Hope, mean monthly flow}
\description{
  
  Mean monthly flow (cms) of Fraser River From March 1912 to December
  2017, recorded by Fraser River at Hope station.

}
\usage{
data("Fraser2017")
}
\format{
  A time series (class \code{"ts"}) with frequency 12, starting from
  January 1912 (the first two data values are \code{NA}) to December 2017.
}
\details{
  
  Dataset \code{Fraser2017} is an extention of dataset \code{"Fraser"}
  in package \code{"pear"}. The latter runs upto December 1990 (not the
  documented December 1991). At the time of writing this package
  \code{"pear"} is archived on CRAN, which is the main reason to include
  the dataset (with the added benefit of almost 30 years of additional
  data).
 

}
\source{
  \url{https://wateroffice.ec.gc.ca/}
}
% \references{
% %%  ~~ possibly secondary sources and usages ~~
% }
\seealso{
  \code{\link{dataFranses1996}},
  \code{\link{four_stocks_since2016_01_01}}
}
\examples{
data(Fraser2017)

fr <- window(Fraser2017, start = c(1912, 3), end = c(1990, 12))
## all.equal(as.numeric(fr), as.numeric(pear::Fraser)) # TRUE
## all.equal(tsp(fr), tsp(pear::Fraser))               # TRUE
}
\keyword{datasets}
