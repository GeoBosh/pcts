## Define the core Cycle classes
BareCycle <- setClass("BareCycle", slots = c(nseasons = "integer"))

setClass("SimpleCycle", contains = "BareCycle",
         slots = c(cycle = "character", season = "character",
                   seasons = "character", abbreviated = "character"
                   ),
         prototype = list(cycle = "Cycle", season = "Season",
                          seasons = character(0), abbreviated = character(0))
         )

setClass("BuiltinCycle", contains = "VIRTUAL",
         slots = c(first = "integer"),
         prototype = list(first = 1L)
         )
## TODO: "RegularCycle" - SimpleCycle + vector of nested periods

## setClassUnion("BasicCycle", c("BareCycle", "BuiltinCycle", "PartialDayWeekCycle"))
setClassUnion("BasicCycle", c("BareCycle", "BuiltinCycle"))

setClass("PartialCycle", slots = c(orig = "BasicCycle", subindex = "integer") )
setIs("PartialCycle", "BasicCycle")

## builtin cycles
setClass("QuarterYearCycle", contains = "BuiltinCycle")
setClass("MonthYearCycle", contains = "BuiltinCycle")
setClass("DayWeekCycle", contains = "BuiltinCycle")
setClass("FiveDayWeekCycle", contains = "BuiltinCycle") ## deprecated
setClass("OpenCloseCycle", contains = "BuiltinCycle")
setClass("Every30MinutesCycle", contains = "BuiltinCycle")

## setGeneric("nSeasons", def = function(x, ...){ standardGeneric("nSeasons") })
## setGeneric("nSeasons<-", def = function(x, ..., value){ standardGeneric("nSeasons<-") })

setGeneric("unitSeason", function(x){ "Season" })  # "pc.unit.season"
setGeneric("unitCycle", function(x){ "Cycle" })     # "pc.unit.epoch"
setGeneric("unitSeason<-",
           function(x, ..., value){ standardGeneric("unitSeason<-") })
setGeneric("unitCycle<-",
           function(x, ..., value){ standardGeneric("unitCycle<-") })

setGeneric("allSeasons",
           function(x, abb = FALSE, prefix = "S", ...){ standardGeneric("allSeasons") })
setGeneric("allSeasons<-",
           function(x, abb = FALSE, ..., value){ standardGeneric("allSeasons<-") })

setGeneric("seqSeasons", function(x){ standardGeneric("seqSeasons") }) # "pc.seasons"?

setMethod("seqSeasons", "BasicCycle", function(x) 1:nSeasons(x) )

## currently the first half contains cycles, the second half - years
.ind_of_seasons_in_pairs <- function(pairs){
    n <- length(pairs)
    if(n == 2)
        2
    else if(n %% 2 == 0)
        (1 + n / 2):n
    else
        stop("the length of pairs must be even")
}

.mark_invalid_seasons_in_pairs <- function(pairs, subindex){
    flags <- rep(FALSE, length(pairs))
    ind <- .ind_of_seasons_in_pairs(pairs)

    flags[ind] <- !(pairs[ind] %in% subindex)
    if(any(flags)){   # pair[2] > 5
        pairs[flags] <- NA
    }

    pairs
}

## for PartialCycle - convert from 1:nseasons to the original indexes of seasons in orig
## TODO: the inverse (original indexes of seasons in orig to 1:nseasons, see format.Pctime
.recode_seasons_in_pairs <- function(pairs, subindex, nseasons){
    # ## TODO: what if the first season in the PartialCycle is not 1, may need ar. 'first'
    # ##
    # ## TODO: what is the relation between the first season in PartialCycle
    # ##       and that in the derived class?
    
    ind <- .ind_of_seasons_in_pairs(pairs)

    pairs[ind] <- subindex[pairs[ind]]
    pairs
}

## roughly the inverse of the above, 
## see  format.Pctime, TODO: replace the code there with a call to this
.recode_seasons_from_orig <- function(pairs, cycle){
    if(!is(cycle, "PartialCycle"))
        return(pairs)
        
    ind <- .ind_of_seasons_in_pairs(pairs)
    seasons <- pairs[ind]

    recodeind <- rep(NA_integer_, nSeasons(cycle@orig))
    recodeind[cycle@subindex] <- seq(along = cycle@subindex)
    seasons <- recodeind[seasons]

    pairs[ind] <- seasons
    pairs
}

pctime2ind <- function(x, start, nseasons){
    ## 'start' is a single pair
    ## 'x' is numeric of length 2 times the number of pairs
    ## the result is of length length(x)/2
    if(length(x) == 2)
        (x[1] - start[1]) * nseasons + (x[2] - start[2]) + 1
    else{
        ind <- .ind_of_seasons_in_pairs(x)
        (x[-ind] - start[1]) * nseasons + (x[ind] - start[2]) + 1
    }
}

ind2pctime <- function(x, start, nseasons){
    ## TODO: for now start is assumed to be just a single pair
    stopifnot(is.numeric(start) && length(start) == 2)

    seasons <- x %% nseasons
    seasons <- ifelse(seasons == 0, nseasons, seasons)

    seasons <- start[2] + seasons - 1
    cycflags <- seasons > nseasons & !is.na(seasons)
    
    cycles <- start[1] + (x - 1) %/% nseasons

    if(any(cycflags)){
        seasons[cycflags] <- seasons[cycflags] - nseasons
        cycles[cycflags] <- cycles[cycflags] + 1
    }
    
    c(cycles, seasons)
}

## based on  as.Date.PeriodicTimeSeries
.cycle_offsets <- function(cycle, n, seas1st){  # seas1st <- start(x)[2]
    nseas <- nSeasons(cycle)
    seasind <- 1:nseas

    switch(class(cycle),
           "PartialCycle" = {
               nseas.parent <- nSeasons(cycle@orig)
               seasind <- cycle@subindex
           },
           "FiveDayWeekCycle" = {
               nseas.parent <- 7
               seasind <- 1:5
           },
           ## default
           {
               nseas.parent <- nseas
               seasind <- 1:nSeasons(cycle)
           }
           )

    if(nseas.parent == nseas){
        shiftall <- 0:(n - 1)
    }else{
        ## seas1st <- start(x)[2]
        seas1stind <- seasind[seas1st]

        if(seas1stind > 1){
            seasind <- ifelse(seasind >= seas1stind, seasind, seasind + nseas.parent)
            seasind <- sort(seasind) # actually, rotate left by seas1st - 1
        }
        
        shift <- seasind - seasind[1]
        indall <- rep(shift, length.out = n)
        m <- 1 + length(indall) %/% length(shift)
        shift2 <- rep(0:m, each = length(shift), length.out = n)
        shiftall <- indall + nseas.parent * shift2
    }

    ## as.Date(startdate + p * shiftall) # without as.Date it's class is [1] "POSIXct" "POSIXt" 
    shiftall
}

setGeneric(".get_period_units", function(cycle) "seconds")
setGeneric(".get_period_length", function(cycle) 1)

.get_period <- function(cycle){
    period(.get_period_length(cycle), .get_period_units(cycle))
}

.get_origin <- function(cycle) stop("origin for class ", class(cycle), " not defined")
setGeneric(".get_origin")

## to allow pairs for monthyear, quarteryear to have first component equal to the year
setGeneric(".get_offset", function(cycle) 0)

.nperiods <- function(cycle, date){
    p <- .get_period(cycle)
    int <- lubridate::interval(.get_origin(cycle), date)
    res <- int  %/% p
    ## TODO: check if integer? or round? Maybe add an argument to control this.
    res
}
setGeneric(".nperiods", signature = "cycle")

.cycle_and_time2pair <- function(cycle, date){
    n <- .nperiods(cycle, date)
    nseas <- nSeasons(cycle)
    ncycles <- n %/% nseas
    seasons <- n %% nseas
    seasons <- ifelse(seasons == 0, nseas, seasons)

    c(.get_offset(cycle) + ncycles + (seasons != nseas), seasons)
}
setGeneric(".cycle_and_time2pair", signature = "cycle")

.cycle_and_pair2time <- function(cycle, pair){
    indseas <- .ind_of_seasons_in_pairs(pair) # pair can represent more than one pair. TODO: Rename?
    cycles <- pair[-indseas]
    seasons <- pair[indseas]
    
    p <- .get_period(cycle)
    res <- .get_origin(cycle) + p * (nSeasons(cycle) * (cycles - 1 - .get_offset(cycle)) + 
                                     seasons)
    res
}
setGeneric(".cycle_and_pair2time", signature = "cycle",
         function(cycle, pair) {                                    # non-standard generic
           value <- standardGeneric(".cycle_and_pair2time")
           if(!inherits(value, "POSIXt")) ## TODO: POSIXct ?
             stop("objects returned by .cycle_and_pair2time methods must inherit POSIXt")
           value
           })

setMethod(".get_origin", c(cycle = "BareCycle"), function(cycle) lubridate::origin )
setMethod(".nperiods", c(cycle = "BareCycle"), function(cycle, date) as.numeric(date) )
## setMethod(".cycle_and_time2pair", c(cycle = "BareCycle"), 
##           function(cycle, date) lubridate::origin + as.numeric(date) )

setMethod("allSeasons", "BasicCycle",
          function(x, abb, prefix = "S", ...){ ## TODO: currently ignores 'abb'
              nseas <- nSeasons(x)
              if(length(nseas) > 0)
                  paste0(prefix, 1:nseas) # TODO: "Season_1", abb S1, etc. ?
              else
                  character(0)
          }
          )

setMethod("allSeasons", "SimpleCycle",
          function(x, abb, prefix, ...){
              if(abb)
                  x@abbreviated
              else
                  x@seasons
          }
          )

setReplaceMethod("allSeasons", "SimpleCycle",
          function(x, abb, prefix, ..., value){
              stopifnot(length(value) == nSeasons(x))
              if(abb)
                  x@abbreviated <- value
              else
                  x@seasons <- value
              x
          }
          )

setMethod("unitSeason", "SimpleCycle", function(x) x@season )
setMethod("unitCycle", "SimpleCycle", function(x) x@cycle )

setReplaceMethod("unitSeason", "SimpleCycle",
          function(x, ..., value){
              stopifnot(length(value) == 1)
              x@season <- value
              x
          }
          )

setReplaceMethod("unitCycle", "SimpleCycle",
          function(x, ..., value){
              stopifnot(length(value) == 1)
              x@cycle <- value
              x
          }
          )

.bc_index <- function(x, i, abb = FALSE){
    seasons <- allSeasons(x, abb = isTRUE(abb)) # full or abbreviated names
    res <- seasons[i]

    if(is.logical(abb))
        res
    else if(is.numeric(abb)){
        if(abb >= 0)   ## todo: should this be >0 ?
            substring(res, 1, abb)
        else if(abb < 0)                          # takes last abb chars from the strings
            substring(res, nchar(res) + abb + 1)
    }else if(is.function(abb))# todo: is this ok?
        abb(res)
    else
        stop("invalid abbreviation requested")
}

setMethod("[", c(x = "BasicCycle", i = "missing", j = "missing"),   # drop is not used
          function(x, i, j, abb = FALSE, ..., drop){
              i <- seq(length = nSeasons(x))
              .bc_index(x, i, abb)
          }
          )

setMethod("[", c(x = "BasicCycle", i = "ANY", j = "missing"),   # drop is not used
          function(x, i, j, abb = FALSE, ..., drop){
              .bc_index(x, i, abb)
          }
          )

setReplaceMethod("[", c(x = "BasicCycle", i = "ANY", j = "missing"),
          function(x, i, j, abb = FALSE, ..., value){
              allSeasons(x, abb = abb)[i] <- value
              x
          }
          )

setReplaceMethod("[", c(x = "BasicCycle", i = "missing", j = "missing"),
          function(x, i, j, abb = FALSE, ..., value){
              allSeasons(x, abb = abb) <- value
              x
          }
          )

setMethod("nSeasons", "BareCycle", function(object) object@nseasons)

setMethod("initialize", signature(.Object = "BareCycle"),
          function(.Object, nseasons, ...){
              .Object <- if(missing(nseasons))
                             callNextMethod(.Object, ...)
                         else{
                             ## TODO: this rounds nseasons if not integer,
                             ##       should give error if that is the case.
                             nseasons <- as.integer(nseasons)
                             callNextMethod(.Object, nseasons = nseasons, ...)
                         }
              .Object
          })

setMethod("initialize", signature(.Object = "BuiltinCycle"),
          function(.Object, first, ...){
              ## TODO: instead of 'stopifnot()', set up a validity function
              .Object <- if(missing(first))
                             callNextMethod(.Object, ...)
                         else{
                             ## TODO: this rounds first if not integer,
                             ##       should give error if that is the case.
                             first <- as.integer(first)
                             stopifnot(first >= 1)
                             callNextMethod(.Object, first = first, ...)
                         }
              stopifnot(.Object@first <= nSeasons(.Object))

              .Object
          })

setMethod("initialize", c(.Object = "SimpleCycle"),
          function(.Object, ...){
              ## .Object <- if(length(list(...)) == 0)
              ##                callNextMethod(.Object, nseasons = 1L)
              ##            else
              ##                callNextMethod(.Object, ...)

              .Object <- callNextMethod(.Object, ...)

              nseas <- .Object@nseasons
              len.nseas <- length(nseas)

              len.seas <- length(.Object@seasons)
              len.abb  <- length(.Object@abbreviated)

              ## 2019-04-17 TODO: The code below is rather erratic. Consolidate!
              ##    For now patching.
              ##
              ## 'nseas == len.seas' maybe of length > 1 (if 'nseas' is).
              ##
              ## Replacing 'len.nseas > 0' with 'len.nseas == 1' in the first inequality of
              ## the following if:
              if(len.nseas == 1 && nseas == len.seas  &&  nseas == len.abb
                 || len.nseas == 0 && len.seas == 0  &&  len.abb == 0     )
                  return(.Object)

              ## make .Object@seasons and .Object@abbreviated of equal length (or stop)
              if(len.seas == 0 && len.abb == 0){# nseas must have positive length here
                  if(len.nseas == 1){
                      if(nseas >= 1){
                          .Object@seasons <- paste0("Season_", seq(length = nseas))
                          .Object@abbreviated <- paste0("S", seq(length = nseas))
                      }else # nseasons < 1
                          stop("'nseasons' must be a positive integer")
                  }else if(len.nseas == 0)
                      stop("nseasons not supplied and cannot be inferred.")
                  else # len.nseas > 1
                      stop("currently 'nseasons' should have length one")
              }else if(len.seas == 0){
                  .Object@seasons <- .Object@abbreviated
              }else if(len.abb == 0){
                  .Object@abbreviated <- .Object@seasons
              }else{ # both, seasons and abbreviated, have positive lengths
                  if(length(.Object@seasons) != length(.Object@abbreviated))
                      stop("lengths of seasons and their abbreviations are not equal")
              }

              ## infer nseasons from the labels, if not set
              if(len.nseas == 0)
                  .Object@nseasons <- length(.Object@seasons)
              else if(.Object@nseasons != length(.Object@seasons))
                  stop("contradictory arguments, lengths do not match nseasons")

              .Object
          })

setAs("BareCycle", "SimpleCycle", function(from) new("SimpleCycle", nseasons = nSeasons(from)))

## 2016-04-03 new implementation of builtin cycle classes
.allseas <- function(x, seasons){
    if(x@first == 1L)
        seasons
    else
        gbutils::shiftleft(seasons, x@first - 1L)
}

BuiltinCycle <- function(n, coerce = FALSE, first = 1, stop = TRUE){
    res <- switch(as.character(n),
                  "2" = new("OpenCloseCycle", first = first),
                  "4" = new("QuarterYearCycle", first = first),
                      # 2020-04-11 was: "5" = new("FiveDayWeekCycle", first = first),
                  "5" = new("PartialCycle", orig = new("DayWeekCycle", first = first), 
                                            subindex = 1:5 ),
                  "7" = new("DayWeekCycle", first = first),
                  "12" = new("MonthYearCycle", first = first),
                  "48" = new("Every30MinutesCycle", first = first),
                  ## default
                  { if(stop)
                        stop("currently there is no builtin class with ", n, " seasons")
                    else
                        new("BareCycle", nseasons = n)
                  }
                  )
    if(coerce)
        as(res, "SimpleCycle")
    else
        res
}

setAs("BuiltinCycle", "BareCycle", function(from) new("BareCycle", nseasons = nSeasons(from)))

setAs("BuiltinCycle", "SimpleCycle",  ## convert any builtin cycle to simple cycle
      function(from){
          new("SimpleCycle",
              cycle = unitCycle(from),
              season = unitSeason(from),
              seasons = allSeasons(from),
              abbreviated = allSeasons(from, abb = TRUE))
      }
      )

setMethod("unitSeason", "Every30MinutesCycle", function(x) "30 Minutes")
setMethod("unitCycle", "Every30MinutesCycle", function(x) "Day")

## a period object from package 'lubridate'
.hm48_period <- (hm("00:30") + minutes((0:47)*30))
## add arbitrarily to 2007-07-01 to convert to duration, then format as string
.hm48Abb <- format(ymd(070101) + .hm48_period, "%H:%M")
## for now full and abbreviated are the samae
.hm48 <- .hm48Abb

setMethod("nSeasons", "Every30MinutesCycle",   function(object) 48L)
setMethod("allSeasons", c(x = "Every30MinutesCycle", abb = "missing"),
          function(x) .allseas(x, .hm48)
          )
setMethod("allSeasons", c(x = "Every30MinutesCycle", abb = "logical"),
          function(x, abb) .allseas(x, if(abb) .hm48Abb else .hm48)
          )

setMethod(".get_period_units", c(cycle = "Every30MinutesCycle"), function(cycle) "minutes")
setMethod(".get_period_length", c(cycle = "Every30MinutesCycle"), function(cycle) 30)

setMethod(".get_origin", c(cycle = "Every30MinutesCycle"), function(cycle) .e30minutes_origin)

setMethod("unitSeason", "QuarterYearCycle", function(x) "Quarter")
setMethod("unitCycle", "QuarterYearCycle", function(x) "Year")

.quarters <- paste0("Quarter_", 1:4)
.quartersAbb <- paste0("Q", 1:4)

setMethod("nSeasons", "QuarterYearCycle",   function(object) 4L)
setMethod("allSeasons", c(x = "QuarterYearCycle", abb = "missing"),
          function(x) .allseas(x, .quarters)
          )
setMethod("allSeasons", c(x = "QuarterYearCycle", abb = "logical"),
          function(x, abb) .allseas(x, if(abb) .quartersAbb else .quarters)
          )

setMethod(".get_period_units", c(cycle = "QuarterYearCycle"), function(cycle) "months")
setMethod(".get_period_length", c(cycle = "QuarterYearCycle"), function(cycle) 3)

## setMethod(".cycle_and_pair2time", c(cycle = "QuarterYearCycle"),
##           function(cycle, pair) 
##               as.Date(paste(pair[1], 1 + 3 * (pair[2] - 1), "01", sep = "-"))
##           )

setMethod(".get_origin", c(cycle = "QuarterYearCycle"), 
          function(cycle) lubridate::origin - months(3) )
setMethod(".get_offset", c(cycle = "QuarterYearCycle"), function(cycle) 1969 )

setMethod("unitSeason", "MonthYearCycle", function(x) "Month")
setMethod("unitCycle", "MonthYearCycle", function(x) "Year")

setMethod("nSeasons", "MonthYearCycle",   function(object) 12L)
setMethod("allSeasons", c(x = "MonthYearCycle", abb = "missing"),
          function(x) .allseas(x, month.name)
          )
setMethod("allSeasons", c(x = "MonthYearCycle", abb = "logical"),
          function(x, abb) .allseas(x, if(abb) month.abb else month.name)
          )

setMethod(".get_period_units", c(cycle = "MonthYearCycle"), function(cycle) "months")

## setMethod(".cycle_and_pair2time", c(cycle = "MonthYearCycle"),
##           function(cycle, pair) 
##               as.Date(paste(pair[1], pair[2], "01", sep = "-"))
##           )

setMethod(".get_origin", c(cycle = "MonthYearCycle"), 
          function(cycle) lubridate::origin - months(1) )
setMethod(".get_offset", c(cycle = "MonthYearCycle"), function(cycle) 1969 )

setMethod("unitSeason", "DayWeekCycle", function(x) "Day")
setMethod("unitCycle", "DayWeekCycle", function(x) "Week")

.days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
.daysAbb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

setMethod("nSeasons", "DayWeekCycle",   function(object) 7L)
setMethod("allSeasons", c(x = "DayWeekCycle", abb = "missing"),
          function(x) .allseas(x, .days)
          )
setMethod("allSeasons", c(x = "DayWeekCycle", abb = "logical"),
          function(x, abb) .allseas(x, if(abb) .daysAbb else .days)
          )

setMethod(".get_period_units", c(cycle = "DayWeekCycle"), function(cycle) "days")

## setMethod(".cycle_and_pair2time", c(cycle = "DayWeekCycle"),
##           function(cycle, pair) 
##               pcweek2date(pair)
##           )
## setMethod(".cycle_and_time2pair", c(cycle = "DayWeekCycle"), 
##           function(cycle, date){
##               .date2pcweek(date)
##           }
##           )

setMethod(".get_origin", c(cycle = "DayWeekCycle"), function(cycle) .week_origin)

setMethod("initialize", signature(.Object = "FiveDayWeekCycle"),
          function(.Object, first, ...){
              stop("New objects from class 'FiveDayWeekCycle' can no longer be created.\n",
                   "Use 'BuiltinCycle(5)' to create objects with completely equivalent functionality."
                  )
          })

setMethod("unitSeason", "FiveDayWeekCycle", function(x) "Day")
setMethod("unitCycle", "FiveDayWeekCycle", function(x) "Week")

.days5 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
.days5Abb <- c("Mon", "Tue", "Wed", "Thu", "Fri")

setMethod("nSeasons", "FiveDayWeekCycle",   function(object) 5L)
setMethod("allSeasons", c(x = "FiveDayWeekCycle", abb = "missing"),
          function(x) .allseas(x, .days5)
          )
setMethod("allSeasons", c(x = "FiveDayWeekCycle", abb = "logical"),
          function(x, abb) .allseas(x, if(abb) .days5Abb else .days5)
          )

setMethod(".get_period_units", c(cycle = "FiveDayWeekCycle"), function(cycle) "days")

setMethod(".cycle_and_pair2time", c(cycle = "FiveDayWeekCycle"),
          function(cycle, pair){
              ## TODO: not sure how to treat this
              ## NA seems sensible:
              pair <- .mark_invalid_seasons_in_pairs(pair, 1:5) # pair[2] > 5
              res <- .cycle_and_pair2time(BuiltinCycle(7), pair)  # pcweek2date(pair)

              res
          }
          )
setMethod(".cycle_and_time2pair", c(cycle = "FiveDayWeekCycle"), 
          function(cycle, date){
              pair <- .cycle_and_time2pair(BuiltinCycle(7), date)  # .date2pcweek(date)
                      # if(res[2] > 5) 
                      #     res[2] <- NA # 5day week doesn't have Sat and Sun
              pair <- .mark_invalid_seasons_in_pairs(pair, 1:5)

              pair
          }
          )

setMethod(".get_origin", c(cycle = "FiveDayWeekCycle"), function(cycle) .week_origin)

setMethod("unitSeason", "PartialCycle", function(x) unitSeason(x@orig))
setMethod("unitCycle", "PartialCycle", function(x) unitCycle(x@orig))

setMethod("nSeasons", "PartialCycle", function(object) length(object@subindex) )
setMethod("allSeasons", c(x = "PartialCycle", abb = "missing"),
          function(x) allSeasons(x@orig)[x@subindex]
          )
setMethod("allSeasons", c(x = "PartialCycle", abb = "logical"),
          function(x, abb) allSeasons(x@orig, abb = abb)[x@subindex]
          )

setMethod(".get_period_units", c(cycle = "PartialCycle"), 
          function(cycle) .get_period_units(cycle@orig)
          )

setMethod(".get_period_length", c(cycle = "PartialCycle"), 
          function(cycle) .get_period_length(cycle@orig)
          )

setMethod(".cycle_and_pair2time", c(cycle = "PartialCycle"),
          function(cycle, pair){
              ## was: pair[2] <- cycle@subindex[pair[2]]
              nseas.orig <- nSeasons(cycle@orig)
              pair <- .recode_seasons_in_pairs(pair, cycle@subindex, nseas.orig)
              .cycle_and_pair2time(cycle@orig, pair)
          }
          )

setMethod(".cycle_and_time2pair", c(cycle = "PartialCycle"),
          function(cycle, date){pairs  <- .cycle_and_time2pair(cycle@orig, date)
              ind <- .ind_of_seasons_in_pairs(pairs)
              pairs[ind] <- ifelse(pairs[ind] %in% cycle@subindex, pairs[ind], NA)
              ## TODO: !!! Should this convert the seasons to 1,2,...
              ##       eg if the cycle is Weekend, 6 to 1 and 7 to 2 ?
              ##   This should be documented explicitly, or an argument controlling 
              ##   this could be added

              pairs
          }
          )

setMethod(".get_origin", c(cycle = "PartialCycle"), function(cycle) .get_origin(cycle@orig))
setMethod(".get_offset", c(cycle = "PartialCycle"), function(cycle) .get_offset(cycle@orig))

setMethod("unitSeason", "OpenCloseCycle", function(x) "OpenOrClose")
setMethod("unitCycle", "OpenCloseCycle", function(x) "HalfDay")

.openorclose <- c("Open", "Close")
.openorcloseAbb <- c("O", "C")

setMethod("nSeasons", "OpenCloseCycle",   function(object) 2L)
setMethod("allSeasons", c(x = "OpenCloseCycle", abb = "missing"),
          function(x) .allseas(x, .openorclose)
          )
setMethod("allSeasons", c(x = "OpenCloseCycle", abb = "logical"),
          function(x, abb) .allseas(x, if(abb) .openorcloseAbb else .openorclose)
          )

setMethod(".get_period_units", c(cycle = "OpenCloseCycle"), function(cycle) "hours")
setMethod(".get_period_length", c(cycle = "OpenCloseCycle"), function(cycle) 12 )


## this is such that .openclose_origin + period(hours = 12) gives c(1, 1)
## twelvehours <- period(hours = 12)
## .openclose_origin <- .week_origin + period(hours = 21) # Sunday 21:00, ie 9pm
## pc_openclose2date <- function(x){
##     .openclose_origin + days(x[1] - 1) + twelvehours * x[2] 
## }

## setMethod(".cycle_and_pair2time", c(cycle = "OpenCloseCycle"),
##           function(cycle, pair) 
##               pc_openclose2date(pair)
##           )

setMethod(".get_origin", c(cycle = "OpenCloseCycle"), function(cycle) .openclose_origin)

objDayWeekCycle <- new("DayWeekCycle")

setGeneric("pcCycle", def = function(x, type, ...){ standardGeneric("pcCycle") })

setMethod("pcCycle", c(x = "numeric", type = "missing"),
          function(x, ...){
              if(length(list(...)) == 0)
                  new("BareCycle", x)
              else
                  new("SimpleCycle", x, ...)
          }
          )

setMethod("pcCycle", c(x = "numeric", type = "character"),
          function(x, type, ...){
              res <- pcCycle(x, ...)
              as(res, type)
          }
          )

setMethod("pcCycle", c("character", type = "missing"),
          function(x, ...){
              new(x, ...)
          }
          )

setMethod("pcCycle", c(x = "character", type = "character"),
          function(x, type, ...){
              res <- new(x, ...)
              as(res, type)
          }
          )

setMethod("pcCycle", c(x = "BasicCycle", type = "missing"),
          function(x, unitCycle, unitSeason, allSeasons, abb){
              res <- as(x, "SimpleCycle")
              if(!missing(unitCycle)){
                  stopifnot(length(unitCycle) == 1)
                  res@cycle <- unitCycle
              }
              if(!missing(unitSeason)){
                  stopifnot(length(unitSeason) == 1)
                  res@season <- unitSeason
              }
              if(!missing(allSeasons)){
                  stopifnot(length(allSeasons) == res@nseasons)
                  res@seasons <- allSeasons
              }
              if(!missing(abb)){
                  stopifnot(length(allSeasons) == res@nseasons)
                  res@abbreviated <- abb
              }
              res
          }
          )

## this method is not applicable currently as only "SimpleClass" has "<-" methods...
setMethod("pcCycle", c(x = "BasicCycle", type = "character"),
          function(x, type, unitCycle, unitSeason, allSeasons, abb){
              res <- as(x, type)
              if(!missing(unitCycle)){
                  stopifnot(length(unitCycle) == 1)
                  unitCycle(res) <- unitCycle
              }
              if(!missing(unitSeason)){
                  stopifnot(length(unitSeason) == 1)
                  unitSeason(res) <- unitSeason
              }
              if(!missing(allSeasons)){
                  stopifnot(length(allSeasons) == nSeasons(res))
                  allSeasons(res) <- allSeasons
              }
              if(!missing(abb)){
                  stopifnot(length(allSeasons) == nSeasons(res))
                  allSeasons(res, abb = TRUE) <- abb
              }
              res
          }
          )

setMethod("show", "BareCycle",
          function(object){
              cat("Object from class ", "'", class(object), "'", "\n", sep = "")
              cat("Number of seasons:", object@nseasons, "\n")
          }
          )

setMethod("show", "SimpleCycle",
          function(object){
              callNextMethod()
              cat("Seasons:", object@seasons, "\n")
              cat("Abbreviated:", object@abbreviated, "\n")
          }
          )

setMethod("show", "BuiltinCycle",
          function(object){
              cat("Object from built-in class ", "'", class(object), "'", "\n", sep = "")
              cat("Cycle start:", object[1], "\n")
          }
          )

setMethod("show", "PartialCycle",
          function(object){
              cat("Object from class ", "'", class(object), "'", "\n", sep = "")
              ## TODO: special case when @orig is BuiltinCycle?
              cat("    partial cycle of ", "'", class(object@orig), "'", 
                  ## TODO: (abbreviations of) seasons' names                 
                  ", seasons: ", paste(object@subindex, sep = "", collapse = ", "),
                  "\n", sep = "")
              cat("Cycle start:", object[1], "\n")
          }
          )

setClass("Cyclic", slots = c(cycle = "BasicCycle", pcstart = "ANY"), 
         prototype = list(pcstart = c(1, 1)) )

setMethod("nSeasons", "Cyclic",   function(object) nSeasons(object@cycle))
setMethod("unitCycle", "Cyclic",  function(x)      unitCycle(x@cycle))
setMethod("unitSeason", "Cyclic", function(x)      unitSeason(x@cycle))
setMethod("allSeasons", "Cyclic", function(x, abb = FALSE, ...)     
                                      allSeasons(x@cycle, abb = abb, ...))
setMethod("seqSeasons", "Cyclic", function(x)      seqSeasons(x@cycle))

## setReplaceMethod("nSeasons", "Cyclic", function(x) nSeasons(x@cycle))
setReplaceMethod("unitCycle", "Cyclic", 
                 function(x, ..., value){
                     unitCycle(x@cycle) <- value
                     x
                 }
                 )
setReplaceMethod("unitSeason", "Cyclic",
                 function(x, ..., value){
                     unitSeason(x@cycle) <- value
                     x
                 }
                 )
setReplaceMethod("allSeasons", "Cyclic", 
                 function(x, abb, ..., value){
                     allSeasons(x@cycle, abb , ...) <- value
                 }
                 )

.week_origin <- as_datetime(as_date("1970-01-01") - 4) # Sunday

twelvehours <- period(hours = 12)
.openclose_origin <- .week_origin + period(hours = 21) # Sunday 21:00, ie 9pm

.e30minutes_origin <- .week_origin + period(days = 1) # Monday 0am

.date2pcweek <- function(date){
    if(!is.Date(date))
        date <- as_date(date)

    ndays <- difftime(date, .week_origin, units = "days")
    ## could obtain nweeks as
    ##    as.numeric(floor(difftime(tmp2, .week_origin, units = "weeks")))
    nweeks <- as.numeric(ndays) %/% 7
    season <- as.numeric(ndays) %% 7
    ## if(season == 0)   ## TODO: use 'ifelse' to vectorise !!!
    ##     season <- 7
    season <- ifelse(season == 0, 7, season)
    ## TODO: the above is vectorised but the retun value is not!
    ##       Assume for now that the caller know how to intepret this. 
    c(nweeks + (season != 7), season)
}

pcweek2date <- function(x){
    .week_origin + weeks(x[1] - 1) + days(x[2])
}

Cyclic <- function(cycle, start = NULL, ...){
    ## ensure cycle is from a cycle class
    if(inherits(cycle, "Pctime")){
        if(is.null(start))
            start <- cycle
        cycle <- attr(cycle, "cycle")
    }else if(!is(cycle, "BasicCycle"))
        cycle <- pcCycle(cycle, ...)

    ## ensure 'start' is a cycle-season pair
    if(is.null(start)){
        start <- c(1,1)
    }else if(is.numeric(start)){
        if(length(start) != 2)
            stop("numeric 'start' must have length 2")
    }else{
        if(!inherits(start, "POSIXct"))
            start <- as_datetime(start)
        if(length(start) > 1){
            warning("got datetime start with length > 1, using first")
            start <- start[1]
        }
        start <- .cycle_and_time2pair(cycle, start)
	start <- .recode_seasons_from_orig(start, cycle) # in case PartialCycle
    }

    new("Cyclic", cycle = cycle, pcstart = start)
}

setMethod("show", "Cyclic",
          function(object){
              ## cat("Object from class ", "'", class(object), "'", "\n", sep = "")
              cat("Start: ", object@pcstart[1], " ", object@cycle[object@pcstart[2]], "\n")
              cat("Cycle: ", class(object@cycle), "\n")
              cat("Number of seasons:", nSeasons(object), "\n")
          }
          )

as.Date.Cyclic <- function(x, ...){
    as_date(as_datetime(x, ...)) # as.Date(as.POSIXct(x, ...))
}

as.POSIXct.Cyclic <- function(x, ...){
    .cycle_and_pair2time(x@cycle, x@pcstart)
}

setMethod("as_date", "Cyclic",
          function(x, ...){
              as_date(as_datetime(x, ...))
          })

Pctime <- function(x, cycle, ...){
    if(missing(cycle)){
        if(is(x, "Cyclic")){ # TODO: separate handling for PeriodicTimeSeries
            cycle <- x@cycle
            x <- as_datetime(x) # start(x)
        }else if(inherits(x, "Pctime"))
            return(x)
        else
            stop("cycle not specified and cannot be inferred")
    }

    if(inherits(x, "matrix"))
        x <- as.vector(x)
    
    res <- if(is.numeric(x)){
               ## pc pairs
               .cycle_and_pair2time(cycle, x)
           }else{
               ## TODO: replace everywhere as.POSIXct with this:
               ##       it has a default origin for numeric x,
               ##       though here this doesn't matter since numeric is handled by the if
               ##       clause
               ##  
               as_datetime(x)   # was: as.POSIXct(x)
           }

    structure(res, cycle = cycle, class = c("Pctime", class(res)))
}

str.Pctime <- function (object, ...){
    NextMethod("str")
    cat("Attributes: ")
    str(attributes(object))
    ## invisible()
}

format.Pctime <- function(x, ...){
    cycle <- attr(x, "cycle")

    pairs <- .cycle_and_time2pair(cycle, x)
    seasnames <- allSeasons(cycle, abb = TRUE)
    cycname <- substr(unitCycle(cycle), 1, 1)

    ind <- .ind_of_seasons_in_pairs(pairs)
    cycles <- pairs[-ind]
    seasons <- pairs[ind]

    if(is(cycle, "PartialCycle")){
        ## patch the seasons returned by .cycle_and_time2pair are those of orig
        ind <- rep(NA_integer_, nSeasons(cycle@orig))
        ind[cycle@subindex] <- seq(along = cycle@subindex)
	seasons <- ind[seasons]
    }

    paste0(cycname, cycles, " ", seasnames[seasons])
}

## need this method since otherwise an invalid object results
##    (attribute 'cycle' is dropped but the class remains Pctime
`[.Pctime` <- 
function (x, i, j, drop = TRUE) 
{
    if(missing(i))
        return(x)

    y <- NextMethod("[")
    stopifnot(inherits(y, "Pctime")) ## this is the reason that the method is needed    
    attr(y, "cycle") <- attr(x, "cycle")
    y
}

`[[.Pctime` <- 
function (x, ..., drop = TRUE) 
{
    y <- NextMethod("[[")
    stopifnot(inherits(y, "Pctime")) ## this is the reason that the method is needed    
    class(y) <- class(y)[-1]
    y
}

as.POSIXct.Pctime <- function(x, tz = "", ...){
    class(x) <- class(x)[-1]
    attr(x, "cycle") <- NULL
    x
}

## as.Date.Pctime() is not needed since the inherited method as.Date.POSIXlt seems to do the
##                job as expected

seq.Pctime <- function (from, to, by, length.out = NULL, along.with = NULL, ...) {
    if(!missing(to)  ||  !missing(by)  || !missing(along.with))
        stop("currently only 'from' and length.out'  are allowed for 'seq.Pctime'")

    ## TODO: check that 'from' has length = 1
    cycle <- attr(from, "cycle")
    p <- .get_period(cycle)

    ## temporary patch
    pairs <- .cycle_and_time2pair(cycle, from)
    seas1st <- pairs[2]

    if(is(cycle, "PartialCycle")){
        seas1st <- which(cycle@subindex == seas1st)    
    }

    ## was: shift <- 0:(length.out - 1)
    shift <- .cycle_offsets(cycle, n = length.out, seas1st = seas1st)

    res <- as_datetime(from) + p * shift

    structure(res, cycle = cycle, class = c("Pctime", class(res)))
}

cycle.Pctime <- function(x, ...){
    ## TODO: needs consolidation, repeated code, see format.Pctime
    cycle <- attr(x, "cycle")

    pairs <- .cycle_and_time2pair(cycle, x)
    ind <- .ind_of_seasons_in_pairs(pairs)
    ## cycles <- pairs[-ind]
    seasons <- pairs[ind]

    if(is(cycle, "PartialCycle")){
        ## patch the seasons returned by .cycle_and_time2pair are those of orig
        ind <- rep(NA_integer_, nSeasons(cycle@orig))
        ind[cycle@subindex] <- seq(along = cycle@subindex)
	seasons <- ind[seasons]
    }
    seasons
}

as_Pctime <- function(x, ...){ UseMethod("as_Pctime") }

as_Pctime.Cyclic <- function(x, ...){ 
    Pctime(x@pcstart, x@cycle)
}

as_Pctime.PeriodicTimeSeries <- function(x, ...){ 
    ## TODO: this needs changing, probably as.Date should use as_Pctime instead.
    res <- as_datetime(x) # as.Date(x)
    Pctime(res, x@cycle)
}

frequency.Cyclic <- function(x, ...) nSeasons(x)
deltat.Cyclic    <- function(x, ...) 1 / nSeasons(x)
cycle.Cyclic     <- function(x, ...){
    ## don't document for now, could change the class of result
    start(x)[2]   ##  new("PeriodicTS", as(x, "Cyclic"), start(x)[2])
}

date.BasicCycle <- function(x) 
    stop("date is undefined for Cycle objects")

date.Cyclic <- function(x) 
    as_date(x)

date.PeriodicTimeSeries <- function(x) 
    as_date(x)


## "date<-" is S4 generic
setMethod("date<-", signature("BasicCycle"), function(x, value) {
    stop("date<- is undefined for Cycle objects")    
})

setMethod("date<-", signature("Cyclic"), function(x, value) {
    x@pcstart <- value  # TODO: convert from date, etc.
    x
})

setOldClass("Pctime")
