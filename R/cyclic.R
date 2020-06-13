#setOldClass("zoo")
setOldClass(c("zooreg", "zoo"))
setOldClass("xts")
setClassUnion("xtsORzoo", c("xts","zoo")) # see quantmod

setOldClass("Date")
setOldClass(c("POSIXct", "POSIXt"))
setOldClass(c("POSIXlt", "POSIXt"))
setClassUnion("AnyDateTime", c("POSIXct", "POSIXlt", "Pctime", "Date"))

## virtual class for signatures; all periodic time series classes are its descendants
##
setClass("PeriodicTimeSeries", contains = c("Cyclic", "VIRTUAL") )

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
##      > showMethods("coerce", classes = "PeriodicTS_ts", includeDefs = TRUE)
##      Function: coerce (package methods)
##      from="ts", to="PeriodicTS_ts"
##      function (from, to = "PeriodicTS_ts", strict = TRUE)
##      {
##          obj <- new("PeriodicTS_ts")
##          as(obj, "ts") <- from
##          obj
##      }

setAs("ts", "PeriodicTS", function(from){ pcts(from) } )
setAs("ts", "PeriodicMTS",
      function(from){
          wrk <- pcts(from)
          new("PeriodicMTS", as(wrk, "Cyclic"), matrix(wrk@.Data, ncol = 1))
      }
      )
setAs("ts", "PeriodicTS_ts", function(from){ new("PeriodicTS_ts", from) } )
setAs("ts", "PeriodicMTS_ts", function(from){ new("PeriodicMTS_ts", from) } )

setAs("mts", "PeriodicMTS", function(from){ pcts(from) } )
setAs("mts", "PeriodicTS",
      function(from){
          res <- pcts(from)
          if(nVariables(from) > 1)
              stop("the time series is multivariate")
          ## maybe never will come here, ts() gives "ts" class if there is only one time series
          res[[1]]
      }
      )

setAs("PeriodicTS", "ts", 
      function(from){
          ts(from@.Data, frequency = nSeasons(from), start = start(from))
      })
setAs("PeriodicMTS", "ts", 
      function(from){
          ts(from@.Data, frequency = nSeasons(from), start = start(from))
      })
as.ts.PeriodicTimeSeries <- function(x, ...) as(x, "ts")

## setAs("PeriodicTimeSeries", "Cyclic", 
##       function(from){
##           new("Cyclic", cycle = from@cycle, pcstart = from@pcstart)
##       })
setAs("PeriodicTS", "Cyclic", 
      function(from){
          new("Cyclic", cycle = from@cycle, pcstart = from@pcstart)
      })

setAs("PeriodicMTS", "Cyclic", 
      function(from){
          new("Cyclic", cycle = from@cycle, pcstart = from@pcstart)
      })

## as.data.frame.pcTimeSeries <- function(x, ...){
##     as.data.frame(coreMatrix(x))
## }
##

setMethod("pcCycle",  "Cyclic", function(x, type, ...) x@cycle)

setMethod("pcCycle",  c(x = "PeriodicTimeSeries", type = "missing"), 
    function(x, type, ...) x@cycle)
setMethod("pcCycle",  c(x = "PeriodicTimeSeries", type = "character"), 
    function(x, type, ...) x@cycle)

setMethod("pcCycle",  c(x = "ts", type = "missing"),
          function(x, type, ...){
              nseasons <- frequency(x)
              BuiltinCycle(nseasons, stop = FALSE)
          }
          )

setMethod("pcCycle",  c(x = "ts", type = "character"),
          function(x, type, ...){
              nseasons <- frequency(x)
              res <- BuiltinCycle(nseasons, stop = FALSE)
              if(type == "")
                type <- "BareCycle"
              as(res, type)
          }
          )

setGeneric("pcts", function(x, nseasons, start, ..., keep = FALSE){ standardGeneric("pcts") },
           signature = c("x", "nseasons") )

.pcts_finalize <- function(x, start, ...){
    if(missing(start))
        return(x)
    if(is.numeric(start) && length(start) == 2){
        x@pcstart <- start
    }else{
        ## TODO: !!! this may need the cycle in some cases
        date <- as_datetime(start) # as.POSIXct(start) # 2020-04-15 was: as.POSIXlt() # 2020-04-14 was: as.Date()
        x@pcstart <- .cycle_and_time2pair(x@cycle, date)
    }
    x
}

setMethod("pcts", c(x = "numeric", nseasons = "missing"),
          function(x, nseasons, start, ...){
              ## `x' should have  method for frequency() in this case
              ## NOTE: frequency() has a default method! (returns 1).
              ## TODO: maybe give warning or error if frequency is 1.

              ##  as.integer() since we don't accept fractional number of seasons
              nseasons <- as.integer(frequency(x))
              ## frequency() has a default method which returns 1
              if(nseasons <= 1)
                  stop("nseasons is missing and cannot be inferred")
              pcts(x, nseasons, start, ...)
          }
          )

setMethod("pcts", c(x = "matrix", nseasons = "missing"),
          function(x, nseasons, start, ...){
              ## `x' should have  method for frequency() in this case
              ##     see ote in the method for x = "numeric" above
              nseasons <- as.integer(frequency(x))
              if(nseasons <= 1)
                  stop("nseasons is missing and cannot be inferred")
              pcts(x, nseasons, start, ...)
          }
          )


setMethod("pcts", c(x = "numeric", nseasons = "numeric"),
          function(x, nseasons, start, ...){
              period <- new("SimpleCycle", nseasons = nseasons)
              wrk <- new("PeriodicTS", cycle = period, x)
              .pcts_finalize(wrk, start, ...)
          }
          )

setMethod("pcts", c(x = "matrix", nseasons = "numeric"),
          function(x, nseasons, start, ...){
              period <- new("SimpleCycle", nseasons = nseasons)
              wrk <- new("PeriodicMTS", cycle = period, x)
              .pcts_finalize(wrk, start, ...)
          }
          )

## todo: check if methods for nseasons "numeric" and "missing" are needed here
setMethod("pcts", "data.frame",
          function(x, nseasons, start, ...){
              pcts(as.matrix(x), nseasons, start, ...)
          }
          )

.guess_zoo_cycle <- function(x){ # x must inherit from zoo
    ## TODO: incomplete

    if(!is.regular(x))
        ## TODO: can this check give TRUE if times are not monotone?
        stop("currently pcts requires regular time intervals")
    ## ## frequency.zoo tries to compute the frequency even if the attribute is not set
    ## nseasons <- frequency(x)
    ## if(frequency > 1) could still be inferred
    index <- index(x)
    if(is.Date(index)){
        ## assume initially day of week
        pct <- Pctime(index, BuiltinCycle(7))
        ## !!! TODO: this needs cycle.Pctime
        cyc <- cycle(pct)
        cycle <- if(all(1:7 %in% unique(cyc)))
                     BuiltinCycle(7)
                 else
                     new("PartialCycle", orig = BuiltinCycle(7), 
                         subindex = as.integer(sort(unique(cyc))))
        Cyclic(cycle, start = pct[[1]])
    }else{
        stop("This branch not implemented yet - please contact the maintainer of package 'pcts'")
    }
}

setMethod("pcts", c(x = "xtsORzoo", nseasons = "missing"),
          function(x, nseasons, start, ...){
              ## ignore argument start for now

              cyclic <- .guess_zoo_cycle(x)
              start <- stats::start(cyclic) 
              cycle <- cyclic@cycle

              nseasons <- nSeasons(cycle)
              nseas <- if(is(cycle, "PartialCycle"))
                           nSeasons(cycle@orig)
                       else
                           nseasons

              x_ts <- as.ts(as.zooreg(x))
              if(nseas == nseasons){
                  pcts(x_ts, cycle, start = start, ...)
              }else{
                  res <- pcts(x_ts, cycle@orig, start = start, ...)
                  window(res, seasons = cycle@subindex)
              }
         } 
         )

.ts2periodic_ts <- function(x, cls, nseasons, ...){
    cyc <- if(!missing(nseasons)  &&  frequency(x) != nseasons){
               ## frequency(x) <- nseasons NOTE: no "ts" method for "frequency<-"
               pcCycle(nseasons) # creates a bare cycle
           }else{
               pcCycle(x)
           }
    new(cls, x, cycle = cyc, pcstart = start(x))
}

setMethod("pcts", c(x = "ts", nseasons = "missing"),
          function(x, nseasons, start, ..., keep){
              wrk <- if(keep)
                         new("PeriodicTS_ts", x)
                     else
                         .ts2periodic_ts(x, "PeriodicTS", nseasons)
              .pcts_finalize(wrk, start, ...)
          }
          )

setMethod("pcts", c(x = "ts", nseasons = "numeric"),
          function(x, nseasons, start, ..., keep){
              wrk <- if(keep){
                         if(frequency(x) != nseasons)
                             stop("please change the frequency of the ts object or use keep = FALSE")
                         new("PeriodicTS_ts", x)
                     }else
                         .ts2periodic_ts(x, "PeriodicTS", nseasons)
              .pcts_finalize(wrk, start, ...)
          }
          )

setMethod("pcts", c(x = "mts", nseasons = "missing"),
          function(x, nseasons, start, ..., keep){
              wrk <- if(keep)
                         new("PeriodicMTS_ts", x)
                     else
                         .ts2periodic_ts(x, "PeriodicMTS", nseasons)
              .pcts_finalize(wrk, start, ...)
          }
          )

setMethod("pcts", c(x = "mts", nseasons = "numeric"),
          function(x, nseasons, start, ..., keep){
              wrk <- if(keep){
                         if(frequency(x) != nseasons)
                             stop("please change the frequency of the ts object or use keep = FALSE")
                         new("PeriodicMTS_ts", x)
                     }else
                         .ts2periodic_ts(x, "PeriodicMTS", nseasons)
              .pcts_finalize(wrk, start, ...)
          }
          )

setMethod("pcts", c(x = "numeric", nseasons = "BasicCycle"),
          function(x, nseasons, start, ...){
              wrk <- new("PeriodicTS", cycle = nseasons, x)
              .pcts_finalize(wrk, start, ...)
          }
          )

setMethod("pcts", c(x = "matrix", nseasons = "BasicCycle"),
          function(x, nseasons, start, ...){
              wrk <- new("PeriodicMTS", cycle = nseasons, x)
              .pcts_finalize(wrk, start, ...)
          }
          )

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

setMethod("show", "PeriodicTS",
          function(object){
              .reportClassName(object, "PeriodicTS")
              ## show(as(object, "Cyclic"))
              cat("Slot \"cycle\": ")
              ## show(object@cycle)
              show(as(object, "Cyclic"))
              cat("\n")
              
              start <- start(object)
              end   <- end(object)
              nseas <- nSeasons(object)

              wrk <- seq(start(object)[1], end(object)[1])
              wrk2 <- rep(wrk, each = nSeasons(object))
              cycles <- wrk2[seq(start(object)[2], length = nTicks(object))]
              cycles_prefix <- substring(unitCycle(object), 1, 1)

              cyc <- cycle(object)

              data <- object@.Data
              if(start[2] > 1)
                  data <- c(rep(NA_real_, start[2] - 1), data)
              if(end[2] < nseas)
                  data <- c(data, rep(NA_real_, nseas - end[2]))

              data <- object@.Data
              if(length(data) %% nseas == 0  && start[2] == 1){
                  data <- matrix(data, ncol = nSeasons(object), byrow = TRUE)
                  rownames(data) <- paste0(cycles_prefix, start[1]:end[1])
                  ## TODO: sort out the method for "Cyclic" to work with abb = TRUE
                  ##     colnames(data) <- allSeasons(object, abb = TRUE)
                  colnames(data) <- allSeasons(object@cycle, abb = TRUE)
                  print(data)
              }else{
                  data <- c(rep(NA_real_, start[2] - 1), data, rep(NA_real_, nseas - end[2]))
                  data <- matrix(data, ncol = nSeasons(object), byrow = TRUE)
                  wrk <- format(data)
                  wrk[1, seq_len(start[2] - 1)] <- ""
                  if(end[2] < nseas)
                      wrk[nrow(wrk), (end[2] + 1) : nseas] <- ""
                  rownames(wrk) <- paste0(cycles_prefix, start[1]:end[1])
                  colnames(wrk) <- allSeasons(object@cycle, abb = TRUE)
                  print(wrk, quote = FALSE)
              }
          }
          )

## An object of class "Cyclic"
## Slot "cycle":
## Object from built-in class 'MonthYearCycle'
## Cycle start: January

setMethod("show", "PeriodicMTS",
          function(object){
              .reportClassName(object, "PeriodicMTS")
              ## show(as(object, "Cyclic"))
              cat("Slot \"cycle\": ")
              show(object@cycle)
              cat("\n")

              wrk <- seq(start(object)[1], end(object)[1])
              wrk <- rep(wrk, each = nSeasons(object))
              cycles <- wrk[seq(start(object)[2], length = nTicks(object))]
              cycles_prefix <- substring(unitCycle(object), 1, 1)

              cyc <- cycle(object)

              data <- object@.Data
              rownames(data) <- paste0(cycles_prefix, cycles, "_", cyc)
              print(data)
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

# nTicks <- function(x, ...)
#     NROW(x)
setGeneric("nTicks", function(x){ standardGeneric("nTicks") })
setMethod("nTicks", "numeric", function(x) length(x) )
setMethod("nTicks", "matrix", function(x) nrow(x) )
setMethod("nTicks", "PeriodicTimeSeries", function(x) NROW(x) )
setMethod("nTicks", "Cyclic", function(x) 1 ) ## todo: currently always one.

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

setMethod("[", c(x = "PeriodicTS", i = "missing", j = "missing"), 
          function(x){
              x@.Data
          })

setMethod("[", c(x = "PeriodicTS", i = "AnyDateTime", j = "missing"), 
          function(x, i){
              ## TODO: is this reliable?
              ind <- which(as_datetime(x)  %in% as_datetime(i))
              x@.Data[ind]
          })

setMethod("[[", c(x = "PeriodicMTS"), 
          function(x, i){
              if (length(i) != 1) 
                  ## call. = FALSE here since otherwise the error message starts
                  ## with "Error in .local(x, i, ...) :" which is non-informative.
                  stop("for [[ the length of argument i must be equal to one", 
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
                  ## 2020-04-19: allowing matrix indexing
                  ## stop("use x[][i, ] or x[][i,j] if you wish to use matrix indexing")
                  j <- 1:ncol(x@.Data)
                  x@.Data[i, j, ...]
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

setMethod("[", c(x = "PeriodicMTS", i = "ANY", j = "ANY"), 
          function(x, i, j, ...){
              x@.Data[i, j, ...]
          })

setMethod("[", c(x = "PeriodicMTS", i = "AnyDateTime", j = "missing"), 
          function(x, i){
              ## TODO: is this reliable?
              ind <- which(as_datetime(x)  %in% as_datetime(i))
              x@.Data[ind, ]
          })

setMethod("[", c(x = "PeriodicMTS", i = "AnyDateTime", j = "ANY"), 
          function(x, i, j){
              ## TODO: is this reliable?
              ind <- which(as_datetime(x)  %in% as_datetime(i))
              x@.Data[ind, j]
          })

start.Cyclic <- function(x, ...){
    x@pcstart
}

## end.PeriodicTimeSeries 
end.Cyclic <- function(x, ...){
    ind2pctime(nTicks(x), x@pcstart, nSeasons(x))
}

window.PeriodicTS <- function(x, start = NULL, end = NULL, seasons = NULL, ...){
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

    if(!is.null(seasons)){
        ## TODO: check validity of 'seasons'
        lind <- logical(nTicks(x))
        lind[begind:endind] <- TRUE
        lind <- lind & (cycle(x) %in% seasons)

        wrkdata <- x@.Data[lind]

        ind.1st <- which(lind)[1]
        newstart <- ind2pctime(ind.1st, start(x), nseas)
        newstart[2] <- which(seasons == newstart[2])

        ## TODO: more is needed here
        cy <- ## if(is(x@cycle, "DayWeekCycle"))
              ##     new("PartialDayWeekCycle", subindex = seasons)
              ## else
                  new("PartialCycle", orig = x@cycle, subindex = seasons) 
#browser()
        ## ## TODO: doesn't work, apparently "Cyclic" needs to pass on .Data
        ## ##     res <- new("PeriodicTS", cycle = cy, pcstart = newstart, .Data = wrkdata)
        ## ## so do it this way:
        ##     cyc <- new("Cyclic", cycle = cy, pcstart = newstart)
        ##     res <- new("PeriodicTS", cyc, .Data = wrkdata)
        ## no, argument .Data seems the culprit;
        ## this works:
        ##    new("PeriodicTS", cycle = BareCycle(4), pcstart = c(1, 1), 1:12)
        ## but this doesn't:
        ##    new("PeriodicTS", cycle = BareCycle(4), pcstart = c(1, 1), .Data = 1:12)
        ## so, just give the data unnamed (also below):
        res <- new("PeriodicTS", cycle = cy, pcstart = newstart, wrkdata)
        return(res)
    }

    ## new("PeriodicTS", cyc, .Data = x@.Data[begind:endind])
    new("PeriodicTS", cyc, x@.Data[begind:endind])
}

window.PeriodicMTS <- function(x, start = NULL, end = NULL, seasons = NULL, ...){
    ## this is almost identical to window.PeriodicTS,
    ##      could be made common:  (1) new is called with  PeriodicTS or PeriodicMTS
    ##                             (2) access x@.Data with the equialent of coredata functions
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

    if(!is.null(seasons)){
        ## TODO: check validity of 'seasons'
        lind <- logical(nTicks(x))
        lind[begind:endind] <- TRUE
        lind <- lind & (cycle(x) %in% seasons)

        wrkdata <- x@.Data[lind, , drop = FALSE]

        ind.1st <- which(lind)[1]
        newstart <- ind2pctime(ind.1st, start(x), nseas)
        newstart[2] <- which(seasons == newstart[2])

        ## TODO: more is needed here
        cy <- ## if(is(x@cycle, "DayWeekCycle"))
              ##     new("PartialDayWeekCycle", subindex = seasons)
              ## else
                  new("PartialCycle", orig = x@cycle, subindex = seasons) 
        res <- new("PeriodicMTS", cycle = cy, pcstart = newstart, wrkdata)
    
        return(res)
    }


    new("PeriodicMTS", cyc, x@.Data[begind:endind, , drop = FALSE])
}

`window<-.PeriodicTS` <- function(x, start = NULL, end = NULL, ..., value){
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

    ## TODO: check lengths?
    x@.Data[begind:endind] <- value
    x
}

`window<-.PeriodicMTS` <- function(x, start = NULL, end = NULL, ..., value){
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

    ## TODO: check lengths?
    x@.Data[begind:endind, ] <- value
    x
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

availStart <- function(x, any = TRUE) UseMethod("availStart")
availStart.default <- function(x, any = TRUE){
    ind <- match(FALSE, is.na(as.vector(x)))
    if(is.na(ind))
        stop("No non-missing values in x")
    ind2pctime(ind, start(x), nSeasons(x))
}

availStart.matrix <- function(x, any = TRUE){
    m <- as.matrix(x)
    ind <- if(any)
               min(apply(m, 2, function(obj) match(FALSE, is.na(obj)) ))
           else # all
               match(TRUE, complete.cases(m))
    
    if(is.na(ind))
        stop(if(any) "No non-missing values in x" else "No complete cases in x" )
    
    ind2pctime(ind, start(x), nSeasons(x))
}

availEnd <- function(x, any = TRUE) UseMethod("availEnd")
availEnd.default <- function(x, any = TRUE){
    y <- rev(as.vector(x))
    ind <- match(FALSE, is.na(y))
    if(is.na(ind))
        stop("No non-missing values in x")
    ind <- length(y) - ind + 1
    ind2pctime(ind, start(x), nSeasons(x))
}

availEnd.matrix <- function(x, any = TRUE){
    m <- as.matrix(x)
    ## TODO: use complete.cases()
    ind <- if(any)
               min(apply(m, 2, function(obj) match(FALSE, is.na(rev(obj))) ))
           else
               match(TRUE, rev(complete.cases(m)))

    if(is.na(ind))
        stop(if(any) "No non-missing values in x" else "No complete cases in x" )

    ind <- nrow(m) - ind + 1
    ind2pctime(ind, start(x), nSeasons(x))
}

na.trim.PeriodicTS <- function (object, sides = c("both", "left", "right"), ...){
    switch(match.arg(sides),
           both = window(object, start = availStart(object), end = availEnd(object)),
           left = window(object, start = availStart(object)),
           right = window(object, end = availEnd(object))
           )
}

na.trim.PeriodicMTS <-
    function (object, sides = c("both", "left", "right"), is.na = c("any", "all"), ...){
        any <- match.arg(is.na) == "all"
        sides <- match.arg(sides)
        if(sides != "right")  start <- availStart(object, any)
        if(sides != "left")   end <- availEnd(object, any)
        switch(sides,
               both = window(object, start = start, end = end),
               left = window(object, start = start),
               right = window(object, end = end)
               )
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

## as.Date.PeriodicTimeSeries
setMethod("as_date", "PeriodicTimeSeries",
          function(x, ...){
              as_date(as_datetime(x, ...))
          })

setMethod("as_datetime", "PeriodicTimeSeries",
          function(x, ...){
              startdate <- as_datetime(as(x, "Cyclic")) # as.Date(as(x, "Cyclic"))

              n <- nTicks(x)
              nseas <- nSeasons(x)
              seasind <- 1:nSeasons(x)

              units <- .get_period_units(x@cycle)
              plen <- .get_period_length(x@cycle)
              p <- period(plen, units)
              cls <- class(x@cycle)

              shiftall <- .cycle_offsets(x@cycle, n, start(x)[2])
              
                  # without as.Date it's class is [1] "POSIXct" "POSIXt" 
                  # as.Date(startdate + p * shiftall)
              startdate + p * shiftall
          })

as.POSIXct.PeriodicTimeSeries <- function(x, ...){
    as_datetime(x, ...)
}

as.Date.PeriodicTimeSeries <- function(x, ...){
    as_date(as_datetime(x, ...))
}

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

setMethod("pcMean", signature("numeric"),
          function(object, nseasons, ...){
              structure(pc_mean(object, nseasons, ...), 
                        names = allSeasons(BareCycle(nseasons)))
          }
          )

setMethod("pcMean", signature("matrix"),
          function(object, nseasons, ...){
              nc <- ncol(object)
              nr <- nrow(object)
              if(nr %% nseasons == 0){
                  dim(object) <- c(nseasons, nr / nseasons, nc)
                  res <- apply(object, c(1, 3), mean, ...)
              }else{
                  res <- sapply(seq_len(nc), function(i) pc_mean(object[ , i], nseasons, ...))
              }
              colnames(res) <- colnames(object)
              rownames(res) <- allSeasons(BareCycle(nseasons))
              res
          }
          )

setMethod("pcMean", signature("PeriodicTS"),
          function(object, ...){
              structure(pc_mean(object, nSeasons(object), ...), names = allSeasons(object))
          }
          )

setMethod("pcMean", signature("PeriodicMTS"),
          function(object, ...){
              nc <- ncol(object)
              ## nseas <- nSeasons(object)
              res <- sapply(seq_len(nc), function(i) pcMean(object[[i]], ...))
              colnames(res) <- colnames(object)
              res
          }
          )

setMethod("pcApply", signature("numeric"),
          function(object, nseasons, FUN, ...){
              structure(pc_apply(object, nseasons, FUN, ...),
                        names = allSeasons(BareCycle(nseasons)))
          }
          )

setMethod("pcApply", signature("matrix"),
          function(object, nseasons, FUN, ...){
              nc <- ncol(object)
              ## nseas <- nSeasons(object)
              res <- sapply(seq_len(nc), 
                            function(i) pc_apply(object[ , i], nseasons, FUN, ...))
              colnames(res) <- colnames(object)
              res
          }
          )

setMethod("pcApply", signature("PeriodicTS"),
          function(object, FUN, ...){
              structure(pc_apply(object, nSeasons(object), FUN, ...),
                        names = allSeasons(object))
          }
          )

setMethod("pcApply", signature("PeriodicMTS"),
          function(object, FUN, ...){
              nc <- ncol(object)
              ## nseas <- nSeasons(object)
              res <- sapply(seq_len(nc), function(i) pcApply(object[[i]], FUN, ...))
              colnames(res) <- colnames(object)
              res
          }
          )

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
              fac <- pc_sdfactor(sd, maxlag)[ , 1 + (0:maxlag)] # "1+" since "matrix"
              res <- acv / fac # this assumes that '/' is defined for 'acv'
                               # (which it is for Lagged objects)
                  # 2019-05-14 was:  res # a "Lagged2d" object
              res <- new("SamplePeriodicAutocorrelations", data = res, n = n, # varnames = varnames,
                                            objectname = objectname
                                            )
              modelCycle(res) <- x@cycle
              res
          })
