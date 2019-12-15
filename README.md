[![CRANStatusBadge](http://www.r-pkg.org/badges/version/pcts)](https://cran.r-project.org/package=pcts)
[![Build Status](https://travis-ci.com/GeoBosh/pcts.svg?branch=master)](https://travis-ci.com/GeoBosh/pcts)
[![Coverage Status](https://coveralls.io/repos/github/GeoBosh/pcts/badge.svg?branch=master)](https://coveralls.io/github/GeoBosh/pcts?branch=master)


'pcts' is an R package for modelling periodically correlated and periodically
integrated time series.


# Installing pcts

The [latest stable version](https://cran.r-project.org/package=pcts) is on
CRAN.

    install.packages("pcts")


You can install the [development version](https://github.com/GeoBosh/pcts) of
`pcts` from Github:

    library(devtools)
    install_github("GeoBosh/pcts")

If `R` complains about insufficient version of package 'lagged', install it from
github with `install_github("GeoBosh/lagged")`.


# Overview

Periodic time series can be created with `pcts()`. Models are fitted with
`fitPM()`, see the examples in the documentation.  The data from Franses (1996)
are available for examples and experiments, see `?dataFranses1996`.  A good
place to start is the help topic `pcts-package`.


This is a beta version with the view of release on CRAN in the next few months.
Suggestions and bug reports are welcome.  This is a major update of (unreleased)
previous versions of 'pcts' which have been used by me and collaborators in the
last fifteen years or so.  I have temporarily removed some functions which will
be put back after suitable revision.

The documentation is still in a 'development' state. In particular, pages for S4
classes nd methods will eventually be declared internal and not clutter the
reference guide. 










