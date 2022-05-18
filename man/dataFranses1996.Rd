\name{dataFranses1996}
\alias{dataFranses1996}
\docType{data}
\title{Example data from Franses (1996)}
\description{A multivariate time series containing the data used
  in examples by Franses (1996).}
\usage{data("dataFranses1996")}
\format{
  A multivariate quarterly time series.
}
\details{

  Each column is a quarterly time series. The time series start and end
  at different times, so \code{NA}'s are used to align them in a single
  multivariate time series. Detailed account of the sources of the data
  is given by
  \insertCite{@@FransesB1; Data Appendix, p. 214;textual}{pcts}.
  
  \describe{
    \item{\code{year}}{(column 1)

      The time formatted as \verb{yyyy.Q}, where \verb{yyyy} is the year
      and \verb{Q} is the quarter (one of 1, 2, 3 or 4.). This column
      was part of the original data but is not really needed here since
      the time series object contains the time information.

    }
    \item{\code{USTotalIPI}}{ (column 2)

      Total Industrial Production Index for the United States (1985 =
      100), 1960.1--1991.4.
      % Source: OECD Main Economic Indicators 
      % Remark: (1985=100) 
    }
    \item{\code{CanadaUnemployment}}{ (column 3)

      Unemployment in Canada, measured in 1000 persons, 1960.1 - 1987.4.
      % Source: OECD Main Economic Indicators 
    }
    \item{\code{GermanyGNP}}{ (column 4)

      Real GNP in Germany, 1960.1 - 1990.4 .
      % Source: Wolters (1992, p.424, note 4) 
    }
    \item{\code{UKTotalInvestment}}{ (column 5)

      Real Total Investment in the United Kindom, 1955.1 - 1988.4.
      % Source: Osborn (1990) 
    }
    \item{\code{SA_USTotalIPI}}{ (column 6)
      Seasonally adjusted \code{USTotalIPI}.
    }
    \item{\code{SA_CanadaUnemployment}}{ (column 7)

      Seasonally adjusted \code{CanadaUnemployment}.
    }
    \item{\code{SA_GermanyGNP}}{ (column 8)

      Seasonally adjusted \code{GermanyGNP}.
    }
    \item{\code{UKGDP}}{ (column 9)

      United Kingdom gross domestic product (at 1985 prices),
      1955.1--1988.4. 
      % Source: Osborn (1990) 
    }
    \item{\code{UKTotalConsumption}}{ (column 10)

      United Kingdom total consumption (at 1985 prices),
      1955.1--1988.4. 
      % Source: Osborn (1990) 
    }
    \item{\code{UKNondurablesConsumption}}{ (column 11)

      United Kindom nondurables consumption (at 1985 prices), 1955.1--1988.4.
      % Source: Osborn (1990) 
    }
    \item{\code{UKExport}}{ (column 12)

      United Kindom exports of goods and services (at 1985 prices),
      1955.1--1988.4.
      % Source: Osborn (1990) 
    }
    \item{\code{UKImport}}{ (column 13)

      United Kindom imports of goods and services (at 1985 prices),
      1955.1--1988.4.
      % Source: Osborn (1990) 
    }
    \item{\code{UKPublicInvestment}}{ (column 14)

      United Kindom public investment (at 1985 prices), 1962.1--1988.4.
      % Source: Osborn (1990) 
    }
    \item{\code{UKWorkforce}}{ (column 15)

      United Kindom workforce (consisting of workforce in employment and
      unemployment), 1955.1--1988.4.
      % Source: Osborn (1990) 
    }
    \item{\code{SwedenNondurablesConsumption}}{ (column 16)

      Real per capita non-durables consumption in Sweden (measured in
      logs), 1963.1--1988.1.
      % Source: Assarsson (1991) 
    }
    \item{\code{SwedenDisposableIncome}}{ (column 17)

      Real per capita disposable income in Sweden (measured in logs),
      1963.1--1988.1.
      % Source: Assarsson (1991) 
    }
    \item{\code{SA_SwedenNondurablesConsumption}}{ (column 18)

      Seasonally adjusted \code{SwedenNondurablesConsumption} with
      Census X-11 method, 1964.1--1988.1.  (Using the approximate linear
      Census X-11 filter given in Table 4.1, p. 52 in
      \insertCite{FransesB1;textual}{pcts} and generating the forecasts
      and backcasts as described in Ooms (1994)).
  
    }
    \item{\code{SA_SwedenDisposableIncome}}{ (column 19)

      Seasonally adjusted \code{SwedenDisposableIncome} with Census X-11
      method, 1964.1--1988.1. (Using the same method as above.)

    }
  }
  
  More details on the individual time series are given by
  \insertCite{FransesB1;textual}{pcts}. 
  
}
\source{
  The data were downloaded from 
  \verb{http://people.few.eur.nl/franses/research/data/data1.txt},
  but this link is now broken.
}
\note{
  Most of the time series in \code{dataFranses1996} are available as
  separate datasets in package `partsm'. The numbers should be the same
  but note that, at the time of writing this, not all datasets there
  carry complete time information.
}
\references{
  \insertAllCited{}
}
\seealso{
  \code{\link{Fraser2017}},
  \code{\link{four_stocks_since2016_01_01}}
}
\examples{
data(dataFranses1996)
class(dataFranses1996)
colnames(dataFranses1996)
dim(dataFranses1996) # c(148, 19)
plot(dataFranses1996[ , 2:11])

tipi <- dataFranses1996[ , "USTotalIPI"]
plot(tipi)
## convert to PeriodicTS and remove NA's at the start and end
pctipi <- pcts(tipi)
pctipi <- window(pctipi, start = availStart(pctipi), end = availEnd(pctipi))
plot(pctipi)

## convert  the whole dataset to class "PeriodicMTS"
pcfr <- pcts(dataFranses1996)

colnames(pcfr)[2:3] #  "USTotalIPI" "CanadaUnemployment"

## subset as "PeriodicMTS"
pcfr2to3 <- pcfr[2:3]
plot(pcfr2to3)
## "[" "PeriodicMTS" even with length one arg.
pcfr2to2  <- pcfr[2]
pcfr2to2a <- pcfr["USTotalIPI"] # same

## use "[[" or $ to get "PeriodicTS"
pcfr2 <- pcfr[[2]]
pcfr2a <- pcfr[["USTotalIPI"]] # same
pcfr2b <- pcfr$USTotalIPI      # same
identical(pcfr2, pcfr2a) # TRUE
identical(pcfr2, pcfr2b) # TRUE

cycle(pcfr)
frequency(pcfr)
}
\keyword{datasets}
