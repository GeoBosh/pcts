[![CRANStatusBadge](http://www.r-pkg.org/badges/version/pcts)](https://cran.r-project.org/package=pcts)
[![R-CMD-check](https://github.com/GeoBosh/pcts/workflows/R-CMD-check/badge.svg)](https://github.com/GeoBosh/pcts/actions)
[![codecov](https://codecov.io/gh/GeoBosh/pcts/branch/master/graph/badge.svg?token=2SW9HKG71Y)](https://app.codecov.io/gh/GeoBosh/pcts)


'pcts' is an R package for modelling periodically correlated and periodically
integrated time series.


# Installing pcts

Install the [latest stable version](https://cran.r-project.org/package=pcts) of
`pcts` from CRAN:

    install.packages("pcts")


You can install the [development version](https://github.com/GeoBosh/pcts) of
`pcts` from Github:

    library(remotes)
    install_github("GeoBosh/pcts")


# Overview

Periodic time series can be created with `pcts()`. Models are fitted with
`fitPM()` and several other functions. To obtain periodic properties, such as
sample periodic autocorrelations of periodic time series or theoretical periodic
autocorrelations of periodic models, just call the respective functions (here
`autocorrelations()` and `partialAutocorrelations()`) and they will compute the
relevant property depending on the class of the argument, see the examples in
the documentation.

A good place to start is the help topic `?pcts-package`.  Several datasets are
available for examples and experiments.  For example, `?dataFranses1996`
contains the data from Franses (1996). The datasets are from classes `"mts"` or
`"ts"` (the standard R classes for time series), so can be used without loading
pcts, if desired.
