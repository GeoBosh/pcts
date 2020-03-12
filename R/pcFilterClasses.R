## TODO: currently not used; remove?
setClass("VirtualPeriodicMonicFilter", contains = c("VIRTUAL"))

setClass("PeriodicMonicFilterSpec", contains = c("VirtualMonicFilterSpec", "VIRTUAL"),
         slots = c(coef = "matrix"),
         prototype = list(coef = matrix(0, nrow = 0, ncol = 0), order = numeric(0))
         )

setClass("PeriodicBJFilter", contains = c("PeriodicMonicFilterSpec", "VirtualBJFilter"))
setClass("PeriodicSPFilter", contains = c("PeriodicMonicFilterSpec", "VirtualSPFilter"))

setMethod("initialize", "PeriodicMonicFilterSpec",
          function(.Object, coef, order, ...){
              .Object <- callNextMethod()

              ## TODO: write a validity function, as well?
              nr <- nrow(.Object@coef)
              len <- length(.Object@order)
              if(nr != len){
                  if(len == 0) # nr > 0 here
                      .Object@order <- rep(ncol(.Object@coef), nr)
                  else if(len == 1) # if nr = 0 here, order will ecome numeric(0)
                      .Object@order <- rep(.Object@order, nr)
                  else if(nr == 0)
                      .Object@coef <- matrix(NA_real_, nrow = len, ncol = max(.Object@order))
                  else
                      stop("Invalid filter specification.")
              }

              .Object
          }
          )

setAs("matrix", "PeriodicBJFilter", function(from) new("PeriodicBJFilter", coef = from))
setAs("matrix", "PeriodicSPFilter", function(from) new("PeriodicSPFilter", coef = from))

setAs("PeriodicBJFilter", "PeriodicSPFilter",
      function(from) new("PeriodicSPFilter", coef = - from@coef, order = from@order))

setAs("PeriodicSPFilter", "PeriodicBJFilter",
      function(from) new("PeriodicBJFilter", coef = - from@coef, order = from@order))

setMethod("nSeasons", "PeriodicMonicFilterSpec", function(object) length(object@order) )

setMethod("filterCoef", c("PeriodicBJFilter", "character"),
          function(object, convention){
              switch(convention,
                  "BJ" = , "--" = , "-"  = object@coef,
                  "SP" = , "++" = , "+"  = - object@coef,
                  stop("invalid value for argument `convention'.")
		  )
          })

setMethod("filterCoef", c("PeriodicSPFilter", "character"),
          function(object, convention){
              switch(convention,
                  "BJ" = , "--" = , "-"  = - object@coef,
                  "SP" = , "++" = , "+"  = object@coef,
                  stop("invalid value for argument `convention'.")
                  )
          })

setMethod("filterPoly", "PeriodicBJFilter", #as_polylist(apply(filterCoef(bj1), 1, polynom))
          function(object, lag_0 = TRUE){
                  as_polylist(apply(cbind(1, - object@coef), 1, polynom))
          })
setMethod("filterPoly", "PeriodicSPFilter",
          function(object, lag_0 = TRUE){
                  as_polylist(apply(cbind(1,   object@coef), 1, polynom))
          })

setMethod("filterPolyCoef", "PeriodicBJFilter",
          function(object, lag_0 = TRUE){
              if(lag_0)
                  cbind(1, - object@coef)
              else
                  - object@coef
          })

setMethod("filterPolyCoef", "PeriodicSPFilter",
          function(object, lag_0 = TRUE){
              if(lag_0)
                  cbind(1, object@coef)
              else
                  object@coef
          })

setClass("PeriodicArmaFilter", contains = "VirtualArmaFilter",
         slots = c(ar = "PeriodicBJFilter", ma = "PeriodicSPFilter"),
         prototype = list(ar = new("PeriodicBJFilter"), ma = new("PeriodicSPFilter"))
         )

setClass("PeriodicArFilter", contains = "PeriodicArmaFilter")
setClass("PeriodicMaFilter", contains = "PeriodicArmaFilter")

## TODO: missing ar or ma (or both but what would be the number of seasons then?)
setMethod("initialize", "PeriodicArmaFilter",
          function(.Object, ..., ar, ma, nseasons){
              if(!missing(ar)){
                  .Object@ar <- ## as(ar, "PeriodicBJFilter")
                      if(is(ar, "PeriodicBJFilter"))
                          ar
                      else
                          as(ar, "PeriodicBJFilter")

                  .Object@ma <- if(missing(ma))
                                    new("PeriodicSPFilter",
                                        order = numeric(length(.Object@ar@order)))
                                else{ # as(ma, "PeriodicSPFilter")
                                    if(is(ma, "PeriodicSPFilter"))
                                        ma
                                    else
                                        as(ma, "PeriodicSPFilter")
                                }
              }else if(!missing(ma)){# ar is missing in this case
                  .Object@ma <- # as(ma, "PeriodicSPFilter")
                      if(is(ma, "PeriodicSPFilter"))
                          ma
                      else
                          as(ma, "PeriodicSPFilter")

                  .Object@ar <- new("PeriodicBJFilter",
                                    order = numeric(length(.Object@ma@order)))
              }else if(!missing(nseasons)){
                  .Object@ar <- new("PeriodicBJFilter", order = numeric(nseasons))
                  .Object@ma <- new("PeriodicSPFilter", order = numeric(nseasons))
              }# else leave as initialised from the prototype

              .Object <- callNextMethod(.Object, ...)
                                        # validity function is not applied automatically in the presence of
                                        # "initialize" methods, apply it now:
              # validObject(.Object)

              .Object
          }
          )

setMethod("initialize", "PeriodicArFilter",
          function(.Object, ...){
              .Object <- callNextMethod()
              validObject(.Object)
              .Object
          })
setMethod("initialize", "PeriodicMaFilter",
          function(.Object, ...){
              .Object <- callNextMethod()
              validObject(.Object)
              .Object
          })

setValidity("PeriodicArmaFilter",
            function(object){
                ok <- nSeasons(object@ar) == nSeasons(object@ma)
                if(ok) TRUE
                else   "Unequal number of seasons of 'ar' and 'ma'"
            }
            )

setValidity("PeriodicArFilter",
            function(object){
                ok <- all(object@ma@order == 0)
                if(ok) TRUE
                else   "Non-trivial moving average part."
            })

setValidity("PeriodicMaFilter",
            function(object){
                ok <- all(object@ar@order == 0 )
                if(ok) TRUE
                else   "Non-trivial autoregressive part."
            })

setAs("PeriodicArmaFilter", "PeriodicArFilter",
      function(from){
          if(any(from@ma@order > 0))
              stop("Non-trivial moving average part.")
          new("PeriodicArFilter", from)
      })

setAs("PeriodicArmaFilter", "PeriodicMaFilter",
      function(from){
          if(any(from@ar@order > 0))
              stop("Non-trivial autoregressive part.")
          new("PeriodicMaFilter", from)
      })

setMethod("show", signature(object = "PeriodicMonicFilterSpec"),
          function (object)
          {
              cat("order: ", object@order, "\n")
              cat("Coefficients:")
              if(length(object@order) == 0  ||  max(object@order) == 0)
                  cat(" <None>", "\n")
              else{
                  cat("\n")
                  print(object@coef)
              }
          }
          )

setMethod("show", signature(object = "PeriodicBJFilter"),
          function (object)
          {
              .reportClassName(object, "PeriodicBJFilter")
              callNextMethod()
          })

setMethod("show", signature(object = "PeriodicSPFilter"),
          function (object)
          {
              .reportClassName(object, "PeriodicSPFilter")
              callNextMethod()
          })


setMethod("show", signature(object = "PeriodicArmaFilter"),
          function (object)
          {
              .reportClassName(object, "PeriodicArmaFilter")
              callNextMethod()
          })

setMethod("show", "PeriodicArFilter",
          function(object){
              .reportClassName(object, "PeriodicArFilter")
              callNextMethod()
          })

setMethod("show", "PeriodicMaFilter",
          function(object){
              .reportClassName(object, "PeriodicMaFilter")
              callNextMethod()
          })

setMethod("maxLag", c(object = "PeriodicArmaFilter"), 
    function(object) 
        length(filterOrder(object)$ar)
)
