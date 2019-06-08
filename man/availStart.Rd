\name{availStart}
\alias{availStart}
\alias{availEnd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time of first or last non-NA value}
\description{Time of first or last non-NA value.}
\usage{
availStart(x)
availEnd(x)
}
\arguments{
  \item{x}{a time series or similar object}
}
\details{

  The time is given as a cycle-season pair.
  The functions can be used to trim \code{NA}'s from the beginning or
  end of the data.

}
\value{
  numeric, length 2
}
%\references{
%%% ~put references to the literature/web site here ~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{window}}
}
\examples{
tipi <- pcts(dataFranses1996[ , "USTotalIPI"])
start(tipi)
end(tipi)
head(tipi)
tail(tipi)

tipi <- window(tipi, start = availStart(tipi), end = availEnd(tipi))
start(tipi)
end(tipi)
plot(tipi)

pcfr <- pcts(dataFranses1996)

pcfr2to4 <- pcfr[2:4]
head(pcfr2to4)
tail(pcfr2to4)
## time of first and last data, can be NA's
start(pcfr2to4) # 1955 Q1
end(pcfr2to4)   # 1991 Q4

## time of first nonNA:
availStart(pcfr[[2]]) # 1960 Q1
availStart(pcfr2to4)  # 1960 Q1

## time of last nonNA:
availEnd(pcfr[[2]])   # 1991 Q4
availEnd(pcfr[[3]])   # 1987 Q4
availEnd(pcfr[[4]])   # 1990 Q4
## but at least one of them is  available for 1991 Q4, so:
availEnd(pcfr2to4)   # 1991 Q4


pcfr2to4a <- window(pcfr2to4, start = availStart(pcfr2to4), end = availEnd(pcfr2to4))
head(pcfr2to4a)
tail(pcfr2to4a)
}
\keyword{ts}
% use one of  RShowDoc("KEYWORDS")
