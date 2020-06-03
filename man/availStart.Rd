\name{availStart}
\alias{availStart}
\alias{availEnd}

\title{Time of first or last non-NA value}
\description{Time of first or last non-NA value.}
\usage{
availStart(x, any = TRUE)

availEnd(x, any = TRUE)
}
\arguments{
  \item{x}{a time series or similar object}
  \item{any}{

    logical flag for multivariate objects. The default \code{TRUE}
    requests the first/last index containing any non-NA value.
    \code{FALSE} requires that all values at
    the first/last index must be non-NA. 

  }
}
\details{

  The time is given as a cycle-season pair.

  Argument \code{any} is meaningful only for multivariate objects.  Its
  name is short for
  "the first/last index for which any of the values (ie at least one) is non-\code{NA}".
  \code{any = FALSE} is taken to mean that the inedx is the first/last
  for which all values are non-\code{NA}.

  
  The functions can be used together with \code{windows} to trim
  \code{NA}'s from the beginning and/or end of the data. As an
  alternative we provide also methods for periodic time series methods
  for \code{zoo:na.trim}, see the examples below.

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
## this requests the time of the last full record:
availEnd(pcfr2to4, any = FALSE)   # 1987 Q4

pcfr2to4a <- window(pcfr2to4, start = availStart(pcfr2to4), end = availEnd(pcfr2to4))
head(pcfr2to4a)
tail(pcfr2to4a, 20)

## trim NA's from both ends, up to the firsxst/last full record:
pcfr2to4b <- window(pcfr2to4, start = availStart(pcfr2to4, FALSE),
                              end = availEnd(pcfr2to4, FALSE))

## TODO: need a better example here since the first non-NA value for all
##     ts in pcfr2to4 is at the same

## alternatively, use na.trim(), the default for is.na is "any"
pcpres <- window(pcts(presidents), end = c(1972, 4))

availStart(pcpres) # 1945 2
availEnd(pcpres)   # 1972 2

both <- na.trim(pcpres) # same as "both"
identical(na.trim(pcpres), both) # TRUE
head(both, 7)
tail(both)
head(na.trim(pcpres, "left"), 7)
tail(na.trim(pcpres, "right"))

cguk <- pcfr[c("CanadaUnemployment", "GermanyGNP", "UKTotalInvestment")]
availStart(cguk)
availStart(cguk, TRUE) # same

availStart(cguk, FALSE)

availEnd(cguk)
availEnd(cguk, TRUE) # same

availEnd(cguk, FALSE)


na.trim(cguk)
head( na.trim(cguk, sides = "left") )
tail( na.trim(cguk, sides = "right") )

head( na.trim(cguk, sides = "left", is.na = "all") )
tail( na.trim(cguk, sides = "right", is.na = "all") )
}
\keyword{ts}
% use one of  RShowDoc("KEYWORDS")
