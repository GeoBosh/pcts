setOldClass(c("zooreg", "zoo"))

## TODO: This needs to be defined early so that testthat::test()() can unload and reload the
##       package during development. (apparently, it can't unload everything from `methods'.)
##

## initially "numeric" was also here but it is hardly a good idea.
##        TODO: note: if `z' is "mts" then  is(z, "AnyTimeSeries") gives  TRUE
##        TODO: change SimpleCycle to BareCycle where no info about the seasons is available!
##
setClassUnion("AnyTimeSeries", c("ts", "zooreg", "zoo")) ## TODO: add "timeSeries" ?

## virtual class for signatures; all periodic time series classes are its descendants
##
setClass("PeriodicTimeSeries", contains = c("Cyclic", "VIRTUAL") )

## setClassUnion below gives the following error (tested on Lenovo B570):
##        > devtools::test()
##        Loading pcts
##        Testing pcts
##        ...............................1
##        1. Error: the new periodic classes are ok --------------------------------------
##        node stack overflow
##
## `R CMD check ' gives the same when running the tests.  Haven't checked if this is bound to
## devtools but the command seems dodgy, since some of the derived classes from
## "PeriodicTimeSeries" inherit from "ts", so comment it out.
##
## setClassUnion("TSWithRegularTime",
##               c("PeriodicTimeSeries", "ts", "zooreg")) ## TODO: add "timeSeries" ?

## basic "native" classes provided by this package
setClass("PeriodicTS",
         contains = c("PeriodicTimeSeries", "numeric")
         )

setClass("PeriodicMTS",
         contains = c("PeriodicTimeSeries", "matrix")
         )

## classes inheriting from "Cyclic" and common time series classes (regularly spaced)
setClass("PeriodicTS_ts",
         contains = c("PeriodicTimeSeries", "ts"),
         validity = function(object){
             if(is.numeric(object))
                 TRUE
             else
                 "The object is not numeric."
         }
         )

setClass("PeriodicMTS_ts",
         contains = c("PeriodicTimeSeries", "ts"),
         validity = function(object){
             is.matrix(object)
         }
         )

setClass("PeriodicTS_zooreg",
         contains = c("PeriodicTimeSeries", "zooreg"),
         validity = function(object){
             is.numeric(object)
         }
         )

setClass("PeriodicMTS_zooreg",
         contains = c("PeriodicTimeSeries", "zooreg"),
         validity = function(object){
             is.matrix(object)
         }
         )

setMethod("initialize", signature(.Object = "PeriodicTS_ts"),
          function(.Object, x, ...){
              if(!is(x, "ts"))
                  x <- ts(x, ...)

              if(is.matrix(x)){
                  if(ncol(x) == 1)
                      x[] <- as.vector(x)
                  else
                      stop('not a scalar time series; consider "PeriodicMTS_ts"')
              }

                 # was: cycle <- new("SimpleCycle", nseasons = as.integer(frequency(x)))
              cycle <- pcCycle(x)
              .Object <- callNextMethod(.Object, cycle = cycle, x)
              ## TODO: I needed this since callNextMethod above doesn't set this properly
              ##       (missing frequency). Is this still the case?
              ##    2016-04-06 - still needed
              S3Part(.Object) <- x

              .Object
          }
      )

setMethod("initialize", signature(.Object = "PeriodicMTS_ts"),
          function(.Object, x, ...){
              if(!is(x, "ts"))
                  x <- ts(x, ...)
                        # expecting x to be scalar time series here
                        # (can hardly happen but try z[ , 1, drop = FALSE], where z is "mts")
              if(!is.matrix(x))
                  dim(x) <- NULL

              cycle <- pcCycle(x)
              .Object <- callNextMethod(.Object, cycle = cycle, x)
              S3Part(.Object) <- x # see above for why this is needed

              .Object
          }
          )



## The automatically generated method for as() since it calls new() with no arguments, so the
## nseasons slot is not set properly:
##
##      > getMethod("coerce", c("ts", "PeriodicTS_ts"))
##      Method Definition:
##
##      function (from, to = "PeriodicTS_ts", strict = TRUE)
##      {
##          obj <- new("PeriodicTS_ts")
##          as(obj, "ts") <- from
##          obj
##      }
##      <environment: namespace:methods>
##
##      Signatures:
##              from to
##      target  "ts" "PeriodicTS_ts"
##      defined "ts" "PeriodicTS_ts"
##
##
##      > showMethods("coerce", classes = "PeriodicTS_ts", includeDefs = TRUE)
##      Function: coerce (package methods)
##      from="ts", to="PeriodicTS_ts"
##      function (from, to = "PeriodicTS_ts", strict = TRUE)
##      {
##          obj <- new("PeriodicTS_ts")
##          as(obj, "ts") <- from
##          obj
##      }

setAs("ts", "PeriodicTS", function(from){ new("PeriodicTS", from) } )
setAs("ts", "PeriodicMTS", function(from){ new("PeriodicMTS", from) } )
setAs("ts", "PeriodicTS_ts", function(from){ new("PeriodicTS_ts", from) } )
setAs("ts", "PeriodicMTS_ts", function(from){ new("PeriodicMTS_ts", from) } )

setAs("mts", "PeriodicMTS", function(from){ new("PeriodicMTS", from) } )
setAs("mts", "PeriodicTS", function(from){ new("PeriodicMTS", from) } )

## TODO: Conversions between PeriodicXXX classes?

## as.data.frame.pcTimeSeries <- function(x, ...){
##     as.data.frame(coreMatrix(x))
## }
##

setAs("PeriodicTS", "ts", 
      function(from){
          ts(from@.Data, frequency = nSeasons(from), start = start(from))
      })
setAs("PeriodicMTS", "ts", 
      function(from){
          ts(from@.Data, frequency = nSeasons(from), start = start(from))
      })
as.ts.PeriodicTimeSeries <- function(x, ...) as(x, "ts")

setMethod("pcCycle",  "Cyclic", function(x, type, ...) x@cycle)

setMethod("pcCycle",  "PeriodicTimeSeries", function(x, type, ...) x@cycle)

setMethod("pcCycle",  "ts",
          function(x, type, ...){
              nseasons <- frequency(x)
              switch(as.character(nseasons),
                     ## TODO: treat specially other frequencies with builtin classes?
                     "4"  = new("QuarterYearCycle"),
                     "12" = new("MonthYearCycle"),
                     new("BareCycle", nseasons = nseasons)
                     )
          }
          )

frequency.PeriodicTimeSeries <- function(x, ...) nSeasons(x)
deltat.PeriodicTimeSeries    <- function(x, ...) 1 / nSeasons(x)
cycle.PeriodicTimeSeries     <- function(x, ...){
    seas_of_1st <- start(x)[2]
    seas <- seqSeasons(x)
    if(seas_of_1st > 1){
        ind <- 1:(seas_of_1st - 1)
        seas <- c(seas[-ind], seas[ind])
    }

    res <- rep(seas, length = nTicks(x))
    new("PeriodicTS", as(x, "Cyclic"), res)
}

time.PeriodicTimeSeries <- function(x, offset = 0, ...){
    ## ignore offset for now
    nseas <- nSeasons(x)
    beg <- start(x)

    res <- seq(beg[1] + (beg[2] - 1)/nseas, length.out = nTicks(x), by = 1/nseas)
    new("PeriodicTS", as(x, "Cyclic"), res)
}

setGeneric("pcts", def = function(x, nseasons, keep = FALSE, ...){ standardGeneric("pcts") })

## TODO: add further methods for pcts


setMethod("pcts", "numeric",
          function(x, nseasons, keep, ...){
              if(missing(nseasons)) # `x' should have  method for frequency() in this case
                  nseasons <- as.integer(frequency(x))
              period <- new("SimpleCycle", nseasons = nseasons)
              new("PeriodicTS", cycle = period, x, ...)
          }
          )

setMethod("pcts", "matrix",
          function(x, nseasons, keep, ...){
              if(missing(nseasons)) # `x' should have  method for frequency() in this case
                                    # note: frequency() has a default method!
                  nseasons <- as.integer(frequency(x))
              period <- new("SimpleCycle", nseasons = nseasons)
              new("PeriodicMTS", cycle = period, x, ...)
          }
          )

.ts2periodic_ts <- function(x, cls, nseasons){
    if(!missing(nseasons)  &&  frequency(x) != nseasons){
        ## frequency(x) <- nseasons NOTE: no "ts" method for "frequency<-"
        cyc <- pcCycle(nseasons) # creates a bare cycle
    }else{
        cyc <- pcCycle(x)
    }
    new(cls, x, cycle = pcCycle(x), pcstart = start(x))
}

setMethod("pcts", "ts",
          function(x, nseasons, keep, ...){
              if(keep){
                  if(!missing(nseasons)  &&  frequency(x) != nseasons)
                      stop("please change the frequency of the ts object or use keep = FALSE")
                  new("PeriodicTS_ts", x)
              }else{
                  .ts2periodic_ts(x, "PeriodicTS", nseasons)
              }
          }
          )

setMethod("pcts", "mts",
          function(x, nseasons, keep, ...){
              if(keep){
                  if(!missing(nseasons)  &&  frequency(x) != nseasons)
                      stop("please change the frequency of the ts object or use keep = FALSE")
                  new("PeriodicMTS_ts", x)
              }else{
                  # 2019-04-19 was: new("SimpleCycle", nseasons = as.integer(frequency(x)))
                  .ts2periodic_ts(x, "PeriodicMTS", nseasons)
              }
          }
          )

## TODO: add further subclasses of "pcTimeSeries" for work with other time series classes.

## base R
##
## cycle() gives the season (a number)
## time() gives the time, say 2014.75 for the 3rd quarter of 2014
## frequency() gives the number of seasons
## deltat()
## start() end() (but these are for compatibility with S2 only)
## window()

## zoo
##
## index(), time() - times of the observations; start(), end()
## coredata() - gives a plain vector/matrix
## merge() - union and intersection
## plot()
## window()


## setMethod("show","pcTs1",
##           function(object){
##           # y <- ts(data=pc.data.vec(object@.pcdata),frequency=pc.nseasons(object@.pcdata))
##             pc.boxplot(object)
##             callNextMethod()
##           }
##           )

setMethod("show", "PeriodicTS",
          function(object){
              ## y <- ts(data=pc.data.vec(object@.pcdata),frequency=pc.nseasons(object@.pcdata))
              ## pc.boxplot(object)
              ##  callNextMethod()
              start <- start(object)
              end   <- end(object)
              nseas <- nSeasons(object)

              wrk <- seq(start(object)[1], end(object)[1])
              wrk2 <- rep(wrk, each = nSeasons(object))
              cycles <- wrk2[seq(start(object)[2], length = nTicks(object))]
              cyc <- cycle(object)

              data <- object@.Data
              if(start[2] > 1)
                  data <- c(rep(NA_real_, start[2] - 1), data)
              if(end[2] < nseas)
                  data <- c(data, rep(NA_real_, nseas - end[2]))

              data <- object@.Data
              if(length(data) %% nseas == 0){
                  data <- matrix(data, ncol = nSeasons(object), byrow = TRUE)
                  rownames(data) <- as.character(start[1]:end[1])
                  ## TODO: sort out the method for "Cyclic" to work with abb = TRUE
                  ##     colnames(data) <- allSeasons(object, abb = TRUE)
                  colnames(data) <- allSeasons(object@cycle, abb = TRUE)
              }else{
                  ## krapka
                  ## TODO: it is confusing to have different output,
                  ##       maybe do it always this way?
                  names(data) <- paste0(cycles, "_", cyc)
              }
              print(data)
              ## show(as(object, "Cyclic"))
          }
          )

setMethod("show", "PeriodicMTS",
          function(object){
              ## y <- ts(data=pc.data.vec(object@.pcdata),frequency=pc.nseasons(object@.pcdata))
              ## pc.boxplot(object)
              ##  callNextMethod()
              wrk <- seq(start(object)[1], end(object)[1])
              wrk <- rep(wrk, each = nSeasons(object))
              cycles <- wrk[seq(start(object)[2], length = nTicks(object))]
              cyc <- cycle(object)

              data <- object@.Data
              rownames(data) <- paste0(cycles, "_", cyc)
              print(data)
              show(as(object, "Cyclic"))
          }
          )

monthplot.PeriodicTimeSeries <- function(x, ylab = deparse(substitute(x)), base,  ...){
    ## 2019-05-28 putting the body of plPlot here; was: pcPlot(x, ...)
    if(missing(base))
        base = function(x) mean(x, na.rm = TRUE)
    nseas <- nSeasons(x)
    if(nVariables(x) == 1){
        monthplot(ts(data = as(x, "vector"), frequency = nseas), ylab = ylab, base = base, ...)
    }else{
        oldpar <- par(no.readonly = TRUE) # 2019-04-17, adding argument to avoid warnings.
        on.exit(par(oldpar))
        layout(1:nVariables(x))
        ## df <- as.data.frame(x)
        # lapply(names(df), function(x) monthplot(df[[x]], ylab = x, ...))
            # 2019-04-26 was:  m <- coreMatrix(x)
        m <- as(x, "matrix")
        lapply(colnames(x), function(s){
                                  x <- ts(data = m[ , s], frequency = nseas )
                                  monthplot(x, ylab = s, base = base, ...)
                              })
    }
    invisible(NULL)
}

boxplot.PeriodicTimeSeries <- function(x, ...){
    ## 2019-05-28 putting the body of pcBoxplot here; was: pcBoxplot(x, ...)
    ## TODO: set this properly!
    ## TODO: define cycle() for periodic time series objects.

    nseas <- nSeasons(x)

    cyc <- cycle(x)                   # rep(1:nSeasons(x), length = nTicks(x))
    nams <- allSeasons(x, abb = TRUE) # paste0("S", 1:nSeasons(x))

    cyc <- factor(cyc, labels = nams)

    f <- function(x){
        boxplot(x ~ cyc)
    }
    if(nVariables(x) == 1){
             # boxplot(ts(data = coreVector(x), frequency = nSeasons(x)), ...)
             # 2019-04-26 was: res <- f(coreVector(x), ...)
        res <- f(as(x, "vector"), ...)
    }else{
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        layout(1:nVariables(x))
            # 2019-04-26 was:  m <- coreMatrix(x)
        m <- as(x, "matrix")
        res <- lapply(colnames(x), function(s){
                                  x <- m[ , s]
                                  boxplot(x ~ cyc, ylab = s, ...)
                              })
    }
    invisible(res)
}

nTicks <- function(x, ...)
    NROW(x)
setGeneric("nTicks")

nVariables <- function(x, ...)
    NCOL(x)
setGeneric("nVariables")

nCycles <- function(x, ...){
    ## for now default to error if result would be non-integer;
    ## rounding up or down would be dangerous
    if(nTicks(x) %% nSeasons(x) != 0)
        stop("Fractional number of cycles.")
    nTicks(x) %/% nSeasons(x)
}
setGeneric("nCycles")

## the S4 method sould be automatic
## setAs("PeriodicMTS", "matrix", function(from) from@.Data)
as.matrix.PeriodicMTS <- function(x, ...) x@.Data

Vec <- function(x, ...){
    matrix(as.vector(x), ncol = 1)
}
setGeneric("Vec")

## TODO: coreDataFrame ? - zasega ne ya pravya, mozhe da izpolzvam drug klas (ne data.frame)
tsMatrix <- function(x, ...){
    t(as(x, "matrix"))         ## or t(coreMatrix(x))
}
setGeneric("tsMatrix")

tsVector <- function(x, ...){
    as.vector(t(as(x, "matrix"))) ## or as.vector(t(coreMatrix(x))) ?
}
setGeneric("tsVector")

tsVec <- function(x, ...){
    matrix(tsVector(x), ncol = 1)
}
setGeneric("tsVec")

pcMatrix <- function(x, ...){
    nseas <- nSeasons(x)
    res <- as(x, "matrix")
    dim(res) <- c(nseas, length(res)/nseas)
    res
}
setGeneric("pcMatrix")

pcArray <- function(x, ndim = 3, ...){ # ndim not used in the default method
    nseas <- nSeasons(x)
    res <- as(x, "matrix")
    dim(res) <- c(nseas, nrow(res)/nseas, ncol(res))
    res
}
setGeneric("pcArray")

## removing: doesn't make sense to have this simply for the transpose of 
##     tsMatrix() and at the moment I don't see a natural definition analogue 
##     of pcMatrix() for the ts case.
##
## pctsMatrix <- function(x, ...){
##     t(pcMatrix(x, ...))
## }
## setGeneric("pctsMatrix")

pctsArray <- function(x, ndim = 3, ...){ # ndim not used in the default method
    res <- tsMatrix(x)
    dim(res) <- c(nVariables(x), nSeasons(x), nCycles(x))
    res
}
setGeneric("pctsArray")

setMethod("[[", c(x = "PeriodicMTS"), 
          function(x, i){
              if (length(i) != 1) 
                  ## call. = FALSE here since otherwise the error messaage starts
                  ## with "Error in .local(x, i, ...) :" which is non-informative.
                  stop("in '[[' length of argument 'i' must be equal to one", 
                       call. = FALSE)
              new("PeriodicTS", as(x, "Cyclic"), x@.Data[ , i])
          })

setMethod("$", c(x = "PeriodicMTS"), 
          function(x, name){
              new("PeriodicTS", as(x, "Cyclic"), x@.Data[ , name])
          })

setMethod("[", c(x = "PeriodicMTS", i = "ANY", j = "missing"), 
          function(x, i, j, ..., drop = TRUE){
              ## both  x[i] and x[i,] are processed by this method;
              if(nposargs(sys.call()) == 2) # x[i]
                  new("PeriodicMTS", as(x, "Cyclic"), x@.Data[ , i, drop = FALSE])
              else{ # x[i, ]
                  ## x@.Data[i, , ...]
                  stop("use x[][i, ] or x[][i,j] if you wish to use matrix indexing")
              }
          })

setMethod("[", c(x = "PeriodicMTS", i = "missing", j = "missing"), 
          function(x, i, j, ...){
              ## x[ ], x[ , ] - TODO: throw error for x[ , ]?
              x@.Data
          })

## setMethod("[", c(x = "PeriodicMTS", i = "ANY", j = "ANY"), 
##           function(x, i, j, ...){
##               ## x[1:2, 1:4], x[ , 1:4]
##               ## note: for x[ , 1:4], missing(i) is TRUE
##               x@.Data[ , , ...]
##           })

pctime2ind <- function(x, start, nseasons){
    (x[1] - start[1]) * nseasons + (x[2] - start[2]) + 1
}

ind2pctime <- function(x, start, nseasons){
    wrk <- x %% nseasons
    if(wrk == 0)
        wrk <- nseasons
    res <- c(start[1] + (x - 1) %/% nseasons, start[2] + wrk - 1)
    if(res[2] > nseasons)
                     # or more robustly: 
                     #    res + c(res[2] %/% nseasons, res2 %% nseasons)
        res + c(1,  res[2] - nseasons)
    else
        res
}

start.Cyclic <- function(x, ...){
    x@pcstart
}

end.PeriodicTimeSeries <- function(x, ...){
    ind2pctime(nTicks(x), x@pcstart, nSeasons(x))
}

window.PeriodicTS <- function(x, start = NULL, end = NULL, ...){
    nseas <- nSeasons(x)
    time1st <- x@pcstart
    begind <- if(is.null(start))
                  1
              else
                  pctime2ind(start, time1st, nseas)
        
    endind <- if(is.null(end))
                  nTicks(x)
              else
                  pctime2ind(end, time1st, nseas)
        
    cyc <- as(x, "Cyclic")
    if(!is.null(start))
        cyc@pcstart <- start

    new("PeriodicTS", cyc, .Data = x@.Data[begind:endind])
}

window.PeriodicMTS <- function(x, start = NULL, end = NULL, ...){
    nseas <- nSeasons(x)
    time1st <- x@pcstart
    begind <- if(is.null(start))
                  1
              else
                  pctime2ind(start, time1st, nseas)
        
    endind <- if(is.null(end))
                  nTicks(x)
              else
                  pctime2ind(end, time1st, nseas)
        
    cyc <- as(x, "Cyclic")
    if(!is.null(start))
        cyc@pcstart <- start

    new("PeriodicMTS", cyc, .Data = x@.Data[begind:endind, , drop = FALSE])
}

setMethod("head", "PeriodicTimeSeries",
          ## TODO: set default to  'n = nSeasons(x)' ?
          function(x, n = 6L, ...){
              ## adjust 'n' analogously to utils:::head.default()
              stopifnot(length(n) == 1L)
              n <- if(n < 0L) 
                       max(nTicks(x) + n, 0L)
                   else min(n, nTicks(x))

              start <- start(x)
              end <- ind2pctime(n, start, nSeasons(x))
              window(x, start = start, end = end)
          })

setMethod("tail", "PeriodicTimeSeries",
          function(x, n = 6L, ...){
              ## adjust 'n' analogously to utils:::tail.default()
              stopifnot(length(n) == 1L)
              xlen <- nTicks(x)
              n <- if (n < 0L) 
                       max(xlen + n, 0L)
                   else min(n, xlen)

              begind <- pctime2ind(end(x), start(x), nSeasons(x)) - n + 1
              start <- ind2pctime(begind, start(x), nSeasons(x))
              window(x, start = start)
          })

availStart <- function(x) UseMethod("availStart")
availStart.default <- function(x){
    ind <- match(FALSE, is.na(as.vector(x)))
    if(is.na(ind))
        stop("No non-missing values in x")
    ind2pctime(ind, start(x), nSeasons(x))
}

availStart.matrix <- function(x){
    m <- as.matrix(x)
    ind <- min(apply(m, 2, function(obj) match(FALSE, is.na(obj)) ))
    if(is.na(ind))
        stop("No non-missing values in x")
    ind2pctime(ind, start(x), nSeasons(x))
}

availEnd <- function(x) UseMethod("availEnd")
availEnd.default <- function(x){
    y <- rev(as.vector(x))
    ind <- match(FALSE, is.na(y))
    if(is.na(ind))
        stop("No non-missing values in x")
    ind <- length(y) - ind + 1
    ind2pctime(ind, start(x), nSeasons(x))
}

availEnd.matrix <- function(x){
    m <- as.matrix(x)
    ind <- min(apply(m, 2, function(obj) match(FALSE, is.na(rev(obj))) ))
    if(is.na(ind))
        stop("No non-missing values in x")
    ind <- nrow(m) - ind + 1
    ind2pctime(ind, start(x), nSeasons(x))
}

setMethod("plot", c(x = "PeriodicTS", y = "missing"),
          function(x, y, main = NULL, ...){
              ## for now just call the base "ts" method
              if(is.null(main))
                  ## not deparse(substitute(x)), since the method is nested
                  main <- deparse(substitute(x, parent.frame()))
              xts <- as.ts(x)
              plot(xts, main = main, ...)
              cycles <- start(x)[1] : end(x)[1]
              points(cycles, pcMatrix(x)[1, ], col = "blue") ## first season
          })

setMethod("plot", c(x = "PeriodicMTS", y = "missing"),
          function(x, y, main = NULL, ...){
              ## for now just call the base "ts" method
              if(is.null(main))
                  ## not deparse(substitute(x)), since the method is nested
                  main <- deparse(substitute(x, parent.frame()))
              xts <- as.ts(x)
              plot(xts, main = main, ...)
              if(nVariables(x) == 1){ ## copy the code from univariate, needs consolidation
                  cycles <- start(x)[1] : end(x)[1]
                  points(cycles, pcMatrix(x)[1, ], col = "blue") ## first season
              }
          })

setMethod("summary", c(object = "PeriodicTS"),
          function(object, alwaysNA = TRUE, ...){
              wrk <- summary(object@.Data)
              wrk <- as.data.frame(t(as.matrix(wrk)))
              if(alwaysNA && is.null(wrk$"NA's"))
                  wrk$"NA's" <- 0
              
              start = paste0(availStart(object), collapse = "_")
              end   = paste0(availEnd(object)  , collapse = "_")
              data.frame(availStart = start, availEnd = end, wrk, check.names = FALSE)
          })

setMethod("summary", c(object = "PeriodicMTS"),
          function(object, row.names = TRUE, ...){
              ## immitate lapply()
              nvars <- nVariables(object)
              wrk <- vector(nvars, mode = "list")
              for(i in 1:nvars){
                  wrk[[i]] <- summary(object[[i]], ...) # the method for PeriodicTS
              }
              if(isTRUE(row.names))
                  names(wrk) <- colnames(object)
              else if(is.numeric(row.names))
                  ## TODO: something more sophisticated here, use dataFranses1996 for testing
                  names(wrk) <- substr(colnames(object), start = 1, stop = row.names)
            
              do.call("rbind", wrk)
          })

setMethod("autocovariances", signature(x = "PeriodicTS"),
          function(x, maxlag, ...){
              ## TODO: It may make sense to give the user the choice of computing
              ##   non-periodic ones e.g. by a special value of nseasons (say 0 or 1)
              ##
              ##     if(!missing(nseasons)) # non-periodic
              ##        ... 
              # objectname <- deparse(substitute(x, parent.frame())) # deparse(substitute(x))
              objectname <- deparse(substitute(x))
              n <- nTicks(x)

              ## periodic
              m <- pcMatrix(x)
              res <- pc.acf(m, maxlag, ...)

              ## TODO: temporary patch, this needs more fundamental solution.
              ## DONE: now objects inherit from "Cyclic", see below, TODO: adapt the methods
              rownames(res) <- allSeasons(x, abb = TRUE)
                      # 2019-05-14 was:  new("Lagged2d", data = res)
              res <- new("SamplePeriodicAutocovariances", data = res, n = n, # varnames = varnames,
                                           objectname = objectname
                                           )
              modelCycle(res) <- x@cycle
              res
          })

setMethod("autocorrelations", signature(x = "PeriodicTimeSeries", maxlag = "ANY", lag_0 = "missing"),
          function(x, maxlag, ...){
              ## see .acv2acr() in PeriodicClasses.org
              # objectname <- deparse(substitute(x, parent.frame())) # deparse(substitute(x))
              objectname <- deparse(substitute(x))
              n <- nTicks(x)
              ## varnames <- colnames(x) # can be NULL, omit for now

              if(nVariables(x) > 1)
                  stop("Method not implemented yet for multivariate time series")
              acv <- autocovariances(x, maxlag = maxlag, ...)
              sd <- sqrt(acv[[0]])
              fac <- pc.sdfactor(sd, maxlag)[ , 1 + (0:maxlag)] # "1+" since "matrix"
              res <- acv / fac # this assumes that '/' is defined for 'acv'
                               # (which it is for Lagged objects)
                  # 2019-05-14 was:  res # a "Lagged2d" object
              res <- new("SamplePeriodicAutocorrelations", data = res, n = n, # varnames = varnames,
                                            objectname = objectname
                                            )
              modelCycle(res) <- x@cycle
              res
          })
