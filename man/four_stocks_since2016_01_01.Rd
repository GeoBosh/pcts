\name{four_stocks_since2016_01_01}
\alias{four_stocks_since2016_01_01}
\docType{data}
\title{Data for four stocks since 2016-01-01}
\description{Data for four stocks since 2016-01-01.}
\usage{data("four_stocks_since2016_01_01")}
\format{

  A list with components "DELL","MSFT", "INTC", "IBM".
  Each component is a time series from class \code{"xts" "zoo"}.

}
\details{

  Stock market data for Dell, Microsoft, Intel and IBM, from 2016-01-01
  to 2020-04-17. The Dell data start from 2016-08-17. All data were
  downloaded from Yahoo Finance on 2020-04-18.

}
\source{
  \code{https://finance.yahoo.com/}
}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\seealso{
  \code{\link{Fraser2017}},
  \code{\link{dataFranses1996}}
}
\examples{
data(four_stocks_since2016_01_01)
DELL <- four_stocks_since2016_01_01$DELL
head(DELL)
tail(DELL)

dell <- pcts(DELL)

head(as_datetime(dell))
head(Pctime(dell))

## Weekends are totally absent from the data,
## so a Monday-Friday sub-cycle is created:
nSeasons(dell)
dell@cycle


## there are some NA's in the data, due to Bank holidays
Pctime(c(2624, 5), pcCycle(dell))               # "W2624 Fri"
as_datetime(Pctime(c(2624, 5), pcCycle(dell)))  # "2020-04-10 UTC"

## dell["2020-04-10 UTC"]

head(cycle(dell))

tail(Pctime(dell))
tail(as.Date(Pctime(dell)))
}
\keyword{datasets}
