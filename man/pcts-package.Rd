\name{pcts-package}
\alias{pcts-package}
\docType{package}
\title{
  \Sexpr[stage=build,results=hide]{pd <- packageDescription("pcts")}
  \Sexpr[stage=build,results=hide]{lb <- library(help="pcts", character.only=TRUE)}
  \Sexpr[stage=build,results=hide]{lbinfo <- if(!is.null(lb$info[[2]])) lb$info[[2]] else "No_matches"}
  \Sexpr[stage=build,results=hide]{anypat <- function(string, pat, ...){ any(sapply(pat, function(x) grepl(x, string, ...))) }}
  \Sexpr[stage=build,results=hide]{
    lbsubset <- function(lbinfo, pat){
      if(length(lbinfo) == 0) return("No entries")
      ind.start <- which(sapply(lbinfo, function(x) anypat(x, pat)))
      if(length(ind.start) == 0) return("No entries")
      ind.ws <- which(grepl("^[[:space:]]+", lbinfo))
      res <- ind.start
      for(ind in ind.start){
	while(any((ind+1) - ind.ws == 0)){
	  ind <- ind + 1
	  res <- c(res, ind)
	}
      }
      lbinfo[ sort(res) ]
    }
  }
  \Sexpr[stage=build,results=hide]{lbpaste <- function(pat) paste("\\\\preformatted{",
    {wrk <- lbsubset(lbinfo,pat);
     wrk <- gsub("(^[^\\\\]?\%)|(([^\\\\])\%)", paste0("\\\\3", "\\\\\\\\", "\%"), wrk);
    paste(if(length(wrk) > 0) wrk else "No entries", collapse="\\n")}, "}", sep="")}
  \Sexpr[stage=build]{pd$Title}
}
\description{
  \Sexpr[stage=build]{pd$Description}
}
\details{
%%%% 2020-04-20 - commented out the old index below.
  
%%%%  % \tabular{ll}{
%%%%  %  Package:  \tab \Sexpr[stage=build]{pd$Package}\cr
%%%%  %  Type:     \tab \Sexpr[stage=build]{pd$Type}\cr
%%%%  %  Version:  \tab \Sexpr[stage=build]{pd$Version} \cr
%%%%  %  Date:     \tab \Sexpr[stage=build]{pd$Date}\cr
%%%%  %  License:  \tab \Sexpr[stage=build]{pd$License}\cr
%%%%  %  LazyLoad: \tab \Sexpr[stage=build]{pd$LazyLoad}\cr
%%%%  %  Built:    \tab \Sexpr[stage=build]{pd$Built}\cr
%%%%  %  }
%%%%  % 
%%%%  %  % 2014-06-22 TODO: rachno slozhich subsets (vizh i funktsiite po-gore.
%%%%  %  %                  Napravi tova za Rdpack!
%%%%  %  % 2014-06-22 slagam tematichen index.
%%%%
%%%%  This overview is out of date and needs rewrite.
%%%%  
%%%%   \strong{Index of the main exported objects, classes and methods:}
%%%%%  \Sexpr[stage=build,results=rd]{paste("\\\\preformatted{", paste(if(!is.null(lb$info[[2]])) lb$info[[2]] else "No entries",collapse="\n"), "}", sep="")}
%%%%
%%%%  % 2019-04-17 copy the subtopic setup from mixAR
%%%%  %
%%%%  % Initial setup
%%%%  %    alltopics - regular expression matching all topics that make sense to be included
%%%%  %    pat       - accumulates regular expressions for topics considered up to the
%%%%  %                    corresponding place below. 
%%%% \Sexpr[stage=build,results=hide]{alltopics <- unique(gsub("^(([^ ]+)[^ ]*|(^[ ]))(.*)", "\\\\1", lb$info[[2]]))}
%%%%  \Sexpr[stage=build,results=hide]{alltopics <- alltopics[!grepl("^ ", alltopics)]}
%%%%
%%%%  \Sexpr[stage=build,results=hide]{pat <- list()}
%%%%
%%%%
%%%%  \Sexpr[stage=build,results=hide]{pat <- c(pat, list(fitPM =
%%%%    c("^fitPM", "^mC.ss", "^pcAr.ss",
%%%%      "^num2pcpar", "^pclsdf", "^pclspiar", "^pcarma_acvf2model",
%%%%      "^pc.fit.arma", "^pc[.]est[.]arma0?")))}
%%%%  \subsection{Model fitting functions}{
%%%%    \Sexpr[stage=build,results=rd]{lbpaste(pat[["fitPM"]])}
%%%%  }
%%%%
%%%%
%%%%  % \Sexpr[stage=build,results=hide]{pat <- c(pat, list(acf =
%%%%  %   c("^autocorrelations[^-]", "^autocovariances[^-]"
%%%%  %   )))}
%%%%  % \subsection{Autocorrelations (periodic and non-periodic)}{
%%%%  %   \Sexpr[stage=build,results=rd]{lbpaste(pat[["acf"]])}
%%%%  % }
%%%%  
%%%%
%%%%      
%%%%  \Sexpr[stage=build,results=hide]{pat <- c(pat, list(tests =
%%%%    c("^pcTest", "^pc[.]test", "^pc[.]wn.var.acrf", "^test_piar", "^parcovmatlist",
%%%%      "^pwn_McLeodLjungBox_test", "^periodic_acf1_test"
%%%%    )))}
%%%%  \subsection{Tests and statistics}{
%%%%    \Sexpr[stage=build,results=rd]{lbpaste(pat[["tests"]])}
%%%%  }
%%%%
%%%%  \Sexpr[stage=build,results=hide]{pat <- c(pat, list(geneForClasses =
%%%%    paste(c("^pcSeason", "^slMatrix", "^pcts", "^PeriodicVector"),
%%%%          "[[:space:]]", sep = ""
%%%%    )))}
%%%%  \subsection{Generator functions for classes:}{
%%%%    \Sexpr[stage=build,results=rd]{lbpaste(pat[["geneForClasses"]])}
%%%%    
%%%%  }
%%%%
%%%%  \Sexpr[stage=build,results=hide]{pat <- c(pat, list(simulation =
%%%%    c("sim_pwn", "sim_pc", "sim_parAcvf", "sim_parCoef"
%%%%    )))}
%%%%  \subsection{Simulation}{
%%%%    \Sexpr[stage=build,results=rd]{lbpaste(pat[["simulation"]])}
%%%%  }
%%%%
%%%%  \Sexpr[stage=build,results=hide]{pat <- c(pat, list(par =
%%%%    c("^alg1", "^pc.acf", "pc.fcoeffs", "^pc_sdfactor", "^pcAR2acf",
%%%%      "pcacfMat", "permean2intercept", "^permodelmf", "^pdSafeParOrder"
%%%%    )))}
%%%%  \subsection{Periodic autoregression}{
%%%%    \Sexpr[stage=build,results=rd]{lbpaste(pat[["par"]])}
%%%%  }
%%%%
%%%%
%%%%  \Sexpr[stage=build,results=hide]{pat <- c(pat, list(piar =
%%%%    c("^pi1ar2par"
%%%%    )))}
%%%%  \subsection{Integrated PAR}{
%%%%    \Sexpr[stage=build,results=rd]{lbpaste(pat[["piar"]])}
%%%%  }
%%%%
%%%%  %%% commented out on 2020-04-18
%%%%  %%%% \Sexpr[stage=build,results=hide]{pat <- c(pat, list(other =
%%%%  %%%%   c("^pc.boxplot", "^pc.plot", "^pc.filter", "^pc.nvariables",
%%%%  %%%%   # "^pc.omitneg",
%%%%  %%%%   "^pc.plot",
%%%%  %%%%   "^coreMatrix",
%%%%  %%%%   "^dataFranses1996",
%%%%  %%%%   "^ex1f",
%%%%  %%%%   "^nCycles",
%%%%  %%%%   "^pcPlot",
%%%%  %%%%   "^pcacf_pwn_var",
%%%%  %%%%   "^pcarma_acvf_lazy",
%%%%  %%%%   "^pcarma_unvec",
%%%%  %%%%   "^pcts",
%%%%  %%%%   "^toSeason"
%%%%  %%%%   )))}
%%%%  %%%% \subsection{Other}{
%%%%  %%%%   \Sexpr[stage=build,results=rd]{lbpaste(pat[["other"]])}
%%%%  %%%% }
%%%%
%%%%
%%%%  \Sexpr[stage=build,results=hide]{pat <- c(pat, list(pcArma =
%%%%    c("^pc[.]arma", "^pcArma", "^which.minaic", "^pc[.]modelstd", "^pc[.]modelunvec",
%%%%      "^pc[.]lazyacf", "pc[.]lazyh", "pc[.]nonlazyh", "pc[.]hat[.h]",
%%%%      "pc[.]pqapply", "^pc.printarmapqelem", "^pcmaxp",
%%%%      "^pc.internal.checksigma2"
%%%%    )))}
%%%%  \subsection{pc.arma}{
%%%%    \Sexpr[stage=build,results=rd]{lbpaste(pat[["pcArma"]])}
%%%%  }
%%%%
%%%%
%%%%  % \Sexpr[stage=build,results=hide]{pat <- c(pat, list(obsolete =
%%%%  %   c("^pc.stop",  "^pc.acsum", "^pc.argmin", "^pc.arit.floor", "^sumafr"
%%%%  % )))}
%%%%  % \subsection{Obsolete and internal}{
%%%%  %   %%% 2014-08-22  dropping the functions which were moved to pctsData
%%%%  %   %%%       was:    c("^pc[.]?data", "^pc(Data|Ts1)", "^pcfData", "^pc.stop" )
%%%%  %   \Sexpr[stage=build,results=rd]{lbpaste(pat[["obsolete"]])}
%%%%  % }
%%%%
%%%%
%%%%
%%%%
%%%%
%%%%
%%%%
%%%%  %%% commented out on 2020-04-18
%%%%  %%%% \Sexpr[stage=build,results=hide]{pat <- c(pat, list(methods =
%%%%  %%%%   c("[^[:space:]]+-methods"
%%%%  %%%%   )))}
%%%%  %%%% \subsection{Methods}{
%%%%  %%%%   \Sexpr[stage=build,results=rd]{lbpaste(pat[["methods"]])}
%%%%  %%%% }
%%%%
%%%%
%%%%  % Commenting out, this is useful mainly in development.
%%%%  % 
%%%%  % \Sexpr[stage=build,results=hide]{pat <- c(pat, list(classes =
%%%%  %   c("[^[:space:]]+-class"
%%%%  %   )))}
%%%%  % \subsection{Classes}{
%%%%  %   \Sexpr[stage=build,results=rd]{lbpaste(pat[["classes"]])}
%%%%  % }
%%%%
%%%%
%%%%
%%%%  % \Sexpr[stage=build,results=hide]{pat <- c(pat, list(internal = c("^[.]can[^ ]*")))}
%%%%  % \subsection{Internal}{
%%%%  %   \Sexpr[stage=build,results=rd]{lbpaste(pat[["internal"]])}
%%%%  % }
%%%%
%%%%  %% these are ignored, i.e. not printed anywhere
%%%%  \Sexpr[stage=build,results=hide]{pat <- c(pat, list(ignore =
%%%%  c("^pcts-", "^initialize-methods")))}
%%%%  
%%%%  %\Sexpr[stage=build,results=hide]{pat <- gsub("[[]", "\\\\[", pat)  }
%%%%  
%%%%  \Sexpr[stage=build,results=hide]{rest <- alltopics[!grepl( paste0("(", paste0(unlist(pat),collapse=")|("), ")"),alltopics)]}
%%%%  % commenting out since pkgdown gives error: Error: \Sexpr{result=verbatim} not yet supported
%%%%  %\subsection{rest}{
%%%%  %  % This doesn't work properly, It seems that this is because lbpaste() treats its argument
%%%%  %  % as a pattern, while here it is a list of names.
%%%%  %  % TODO: look into this
%%%%  %  %    \Sexpr[stage=build,results=rd]{lbpaste(rest)}
%%%%  %  %
%%%%  %  % For now print a list of the remaining objects
%%%%  %  \Sexpr[stage=build,results=verbatim]{rest}
%%%%  %}
%%%%








% ~~ An overview of how to use the package, including the most important ~~
% ~~ functions ~~

The underlying assumption is that the observations are made at regular
intervals, such as quarter, month, week, day --- or represent data for
such intervals --- and these intervals are nested into larger periods.
In pcts we call the larger period a cycle and its parts seasons. Typical
examples of season-cycle timing are months in a year, quarters in a
year, days in a week (or business week). The number of seasons in a
cycle is called frequency in class \code{"ts"} in base \R.

Cycles in pcts keep not only the number of seasons (frequency) but other
information, such as the names of the seasons and units of seasons.  In
pcts there are a number of builtin cycle classes for typical cases, as
well as provision for creation of custom cycles on the fly.  See
\code{\link{pcCycle}} and \code{\link{BuiltinCycle}} for ways to create
cycle objects, and \code{\link{allSeasons}} for further examples.

Periodic time series can be created with \code{\link{pcts}}, which
accepts as input vectors, matrices and time series objects from base R
and some other packages, including zoo and xts.  When importing data,
the time information is taken from the data and an attempt is made to
guess the periodicity from the frequency (for time series objects that
have it set) and an analysis of the datetime stamps, if present.
\code{pcts} also has arguments for specifying the number of seasons or
the cycle, as well as the start datetime.

The main periodic time series classes in \pkg{pcts} are
\code{\linkS4class{PeriodicTS}} and \code{\linkS4class{PeriodicMTS}},
for univariate and multivariate time series, respectively.  Standard
base-R time series functions can be used with them directly, see for
example
\code{\link[=window.PeriodicTS]{window}},
\code{\link[=frequency.PeriodicTimeSeries]{frequency}},
\code{\link[=cycle.PeriodicTimeSeries]{cycle}},
\code{\link[=time.PeriodicTimeSeries]{time}},
\code{\link[=deltat.PeriodicTimeSeries]{deltat}},
\code{\link[=start.Cyclic]{start}},
\code{\link[=end.Cyclic]{end}},  
\code{\link[=boxplot.PeriodicTimeSeries]{boxplot}},
\code{\link[=monthplot.PeriodicTimeSeries]{monthplot}},
\code{\link[=na.trim.PeriodicTS]{na.trim}} (\code{na.trim} is from package \pkg{zoo}).

Methods for
\code{plot},
\code{summary},
\code{print},
\code{show},
\code{head},
\code{tail},
and other base-R functions are defined where suitable.  Examples can be
found in section Examples and in help pages for the corresponding
functions, classes and methods.

The naming conventions are as follows. Names of classes generally
consists of one or more words. The first letter of each word, is
capitalised.  Only the first letter of abbreviations for models, such as
ARMA, is capitalised. Similarly for generic functions but for them the
first word is not capitalised. In a few names PM stands for 'periodic
model' and TS for 'time series'.

Significant portion of the code was written in 2005--2007. Many of the
functions and classes have been renamed under the above
conventions and most of those that are not are not exported but a few
still are and they should be considered subject to change.

\code{autocovariances}, \code{autocorrelations}, \code{partialAutocorrelations}
and others are one-stop generic functions for computation of properties
of time series and models. What to compute is deduced from the type of
the object. For models they compute theoretical quantities --- periodic
or non-periodic, scalar or multivariate. For time series they compute
the corresponding sample counterparts.

}
\author{
  \Sexpr[stage=build]{pd$Author}

Maintainer: \Sexpr[stage=build]{pd$Maintainer}
}
\references{% bibentry:all
  \insertNoCite{*}{pcts}
  
  \insertAllCited{}
% end:bibentry:all
}
% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
% ~~ the R documentation directory ~~
\keyword{ package }
\seealso{
  \code{\link{pcts}},
  
  \code{\link{fitPM}},
  \code{\link{pclsdf}},
  \code{\link{pclspiar}}
  
  \code{\link{autocorrelations}}
  
  \code{\link{dataFranses1996}}, \code{\link{Fraser2017}},
  \code{\link{four_stocks_since2016_01_01}},

  \code{\link[mcompanion:mcompanion-package]{mcompanion}}
}
\examples{
data(dataFranses1996) 
class(dataFranses1996) # [1] "mts"    "ts"     "matrix"

pcfr <- pcts(dataFranses1996)

class(pcfr)        # "PeriodicMTS"
nSeasons(pcfr) # 4
allSeasons(pcfr)
allSeasons(pcfr, abb = TRUE)

## subsetting
## one index, x[i], is analogous to lists
pcfr2to4 <- pcfr[2:4]; class(pcfr2to4) # "PeriodicMTS"
pcfr2to2 <- pcfr[2];   class(pcfr2to2) # "PeriodicMTS"
pcfr2    <- pcfr[[2]]; class(pcfr2)    # note '[[', "PeriodicTS"

## data for 1990 quarter 3
pcfr2to4[as_date("1990-07-01")] # note: not "1990-03-01"!
pct1990_Q3 <- Pctime(c(1990, 3), pcCycle(pcfr2to4))
pcfr2to4[pct1990_Q3]

## with empty index, returns the underlying data
dim(pcfr[]) # [1] 148  19
dim(pcfr2to2[]) # 148 1
length(pcfr2[]) # 148 (this is numeric)

summary(pcfr2)
summary(pcfr2to4)
## make the output width shorter
summary(pcfr2to4, row.names = FALSE)
summary(pcfr2to4, row.names = 5) # trim row names to 5 characters

head(pcfr2to4)  # starts with NA's
tail(pcfr2to4)  # some NA's at the end too

## time of first and last data, may be NA's
start(pcfr2to4) # 1955 Q1
end(pcfr2to4)   # 1991 Q4

## time of first nonNA:
availStart(pcfr2)    # 1955 Q1
availStart(pcfr2to4) # 1955 Q1

## time of last nonNA:
availEnd(pcfr[[2]])   # 1991 Q4
availEnd(pcfr[[3]])   # 1987 Q4
availEnd(pcfr[[4]])   # 1990 Q4
## but at least one of them is  available for 1991 Q4, so:
availEnd(pcfr2to4)   # 1991 Q4

## use window() to pick part of the ts by time:
window(pcfr2to4, start = c(1990, 1), end = c(1991, 4))
## drop NA's at the start and end:
window(pcfr2to4, start = availStart(pcfr2to4), end = availEnd(pcfr2to4))

plot(pcfr2) # the points mark the first season in each cycle
boxplot(pcfr2)
monthplot(pcfr2)
}
