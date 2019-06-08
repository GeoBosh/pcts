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

setClassUnion("BasicCycle", c("BareCycle", "BuiltinCycle"))

## builtin cycles
setClass("QuarterYearCycle", contains = "BuiltinCycle")
setClass("MonthYearCycle", contains = "BuiltinCycle")
setClass("DayWeekCycle", contains = "BuiltinCycle")
setClass("FiveDayWeekCycle", contains = "BuiltinCycle")
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

BuiltinCycle <- function(n, coerce = FALSE, first = 1){
    res <- switch(as.character(n),
                  "2" = new("OpenCloseCycle", first = first),
                  "4" = new("QuarterYearCycle", first = first),
                  "5" = new("FiveDayWeekCycle", first = first),
                  "7" = new("DayWeekCycle", first = first),
                  "12" = new("MonthYearCycle", first = first),
                  "48" = new("Every30MinutesCycle", first = first),
                  ## default
                  stop("currently there is no builtin class with ", n, " seasons")
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

setMethod("unitSeason", "MonthYearCycle", function(x) "Month")
setMethod("unitCycle", "MonthYearCycle", function(x) "Year")

setMethod("nSeasons", "MonthYearCycle",   function(object) 12L)
setMethod("allSeasons", c(x = "MonthYearCycle", abb = "missing"),
          function(x) .allseas(x, month.name)
          )
setMethod("allSeasons", c(x = "MonthYearCycle", abb = "logical"),
          function(x, abb) .allseas(x, if(abb) month.abb else month.name)
          )

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
              res <- as(res, type)
              res
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
              res <- as(res, type)
              res
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

setClass("Cyclic", slots = c(cycle = "BasicCycle", pcstart = "ANY"), 
         prototype = list(pcstart = c(1, 1)) )

setMethod("nSeasons", "Cyclic",   function(object) nSeasons(object@cycle))
setMethod("unitCycle", "Cyclic",  function(x)      unitCycle(x@cycle))
setMethod("unitSeason", "Cyclic", function(x)      unitSeason(x@cycle))
setMethod("allSeasons", "Cyclic", function(x, abb = FALSE, ...)     
                                      allSeasons(x@cycle, abb = abb, ...))
setMethod("seqSeasons", "Cyclic", function(x)      seqSeasons(x@cycle))

## setReplaceMethod("nSeasons", "Cyclic", function(x) nSeasons(x@cycle))
setReplaceMethod("unitCycle", "Cyclic", function(x, ..., value) unitCycle(x@cycle) <- value)
setReplaceMethod("unitSeason", "Cyclic", function(x, ..., value) unitSeason(x@cycle) <- value)
setReplaceMethod("allSeasons", "Cyclic", 
    function(x, abb, ..., value) allSeasons(x@cycle, abb , ...) <- value)
