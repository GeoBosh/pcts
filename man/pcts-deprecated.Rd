\name{pcts-deprecated}
\alias{pcts-deprecated}
\alias{mCpar}
\alias{sim_arAcf}
\alias{ptildeorders}

\alias{FiveDayWeekCycle-class}
\alias{allSeasons,FiveDayWeekCycle,logical-method}
\alias{allSeasons,FiveDayWeekCycle,missing-method}
\alias{nSeasons,FiveDayWeekCycle-method}
\alias{unitCycle,FiveDayWeekCycle-method}
\alias{unitSeason,FiveDayWeekCycle-method}

\title{Deprecated Functions and classes in Package \pkg{pcts}}
\description{

  These functions and classes are marked for removal and are provided
  temporarily for compatibility with older versions of package
  \pkg{pcts} only. Use the recommended renamed or new functions instead.

  Class \code{"FiveDayWeekCycle"} is deprecated, use
  \code{BuiltinCycle(5)} to create objects with equivalent
  functionality, see \code{\link{BuiltinCycle}}.

}
%\usage{
%}
%\arguments{
%  \item{...}{arguments pass on to the new function.}
%}
\details{
  
  \describe{

    \item{mCpar}{has been renamed to \code{sim_parCoef}}
    \item{sim_arAcf}{has been renamed to \code{sim_parAcvf}}

  }
}
