[![CRANStatusBadge](http://www.r-pkg.org/badges/version/pcts)](https://cran.r-project.org/package=pcts)
[![Build Status](https://travis-ci.com/GeoBosh/pcts.svg?branch=master)](https://travis-ci.com/GeoBosh/pcts)
[![Coverage Status](https://coveralls.io/repos/github/GeoBosh/pcts/badge.svg?branch=master)](https://coveralls.io/github/GeoBosh/pcts?branch=master)


'pcts' is an R package for modelling periodically correlated and periodically
integrated time series.


# Installing pcts

Install the [latest stable version](https://cran.r-project.org/package=pcts) of
`pcts` from CRAN:

    install.packages("pcts")


You can install the [development version](https://github.com/GeoBosh/pcts) of
`pcts` from Github:

    library(devtools)
    install_github("GeoBosh/pcts")


# Overview

Periodic time series can be created with `pcts()`. Models are fitted with
`fitPM()` and several other functions. To obtain periodic properties, such as
sample periodic autocorrelations of periodic time series or theoretical periodic
autocorrelations of periodic models, just call the respective functions (here
`autocorrelations()` and `partialAutocorrelations()`) and they will compute the
relevant property depending on the class of the argument, see the examples in
the documentation.

A good place to start is the help topic `pcts-package`.
The data from Franses (1996) are available for examples and
experiments as `dataFranses1996`, see `?dataFranses1996`. `dataFranses1996`
is of class `"mts"` (the standard R class for multivariate time series).

This is a major update of (unreleased) previous versions of 'pcts' which have
been used in the last fifteen years or so by me and collaborators.  I renamed a
number of functions and other objects to improve consistency and to follow the
current convention not to use dot (`.`) in function names.  Also, I have
temporarily removed some functions which will be put back after suitable
revision.
