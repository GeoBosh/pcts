\name{unitCycle<--methods}
\docType{methods}
\alias{unitCycle<--methods}
\alias{unitCycle<-,Cyclic-method}
\alias{unitCycle<-,SimpleCycle-method}

\alias{unitSeason<--methods}
\alias{unitSeason<-,Cyclic-method}
\alias{unitSeason<-,SimpleCycle-method}

% 2020-02-16 use backticks below, since pkgdown doesn't render well
%     double quotes in this context.
\title{Methods for \code{`unitCycle<-`} and \code{`unitSeason<-`} in package pcts}
\description{

  Methods for \code{`unitCycle<-`} and \code{`unitSeason<-`}
  in package pcts.

}
\section{Methods}{
  \code{`unitCycle<-`} and \code{`unitSeason<-`} have methods with identical
   signatures:
  \describe{

    \item{\code{signature(x = "Cyclic")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(x = "SimpleCycle")}}{
      %%  ~~describe this method here~~
    }
  }
}
\seealso{
  \code{\link{allSeasons}} for related functions and examples
}
\examples{
qrt <- BuiltinCycle(4)
unitSeason(qrt)                 # "Quarter"
unitCycle(qrt)                  # "Year"

moreve <- new("SimpleCycle", 2)
unitSeason(moreve)             # "Season"
unitCycle(moreve)              # "Cycle"
allSeasons(moreve)             # c("Season_1", "Season_2")

## change the names
unitCycle(moreve) <- "Day"
unitSeason(moreve) <- "TimeOfDay"
allSeasons(moreve) <- c("Morning", "Evening")

unitSeason(moreve)
unitCycle(moreve) 
allSeasons(moreve)
}
\keyword{methods}

