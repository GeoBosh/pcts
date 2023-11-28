setClassUnion("SLTypeMatrix", c("slMatrix", "Lagged2d"))

setClass("VirtualPeriodicModel", contains = c("VIRTUAL"))

setMethod("nSeasons", c(object = "VirtualPeriodicModel"), 
          ## subclasses should really define this method
          function (object){
              cycle <- modelCycle(object)
              if(is.null(cycle))
                  stop("number of seasons not computable",
                       ", need a method for 'nSeasons()' or 'modelCycle()' with signature '",
                       class(object), "'.")
              else 
                  nSeasons(cycle)
          })

.get_cycle_prop <- function (.f, x, ...){
    .f <- match.fun(.f)
    cycle <- modelCycle(x)
    if(is.null(cycle))
        ## succeeds if there is applicable method for nSeasons()
        .f(new("BareCycle", nSeasons(x)), ...)
    else
        .f(cycle, ...)
}

setMethod("allSeasons", c(x = "VirtualPeriodicModel"), 
          function (x, abb, ...) .get_cycle_prop("allSeasons", x, abb, ...) )
setMethod("unitSeason", c(x = "VirtualPeriodicModel"), 
          function (x) .get_cycle_prop("unitSeason", x) )
setMethod("unitCycle", c(x = "VirtualPeriodicModel"), 
          function (x) .get_cycle_prop("unitCycle", x) )
setMethod("seqSeasons", c(x = "VirtualPeriodicModel"), 
          function (x) .get_cycle_prop("seqSeasons", x) )

setClass("VirtualPeriodicAutocovariances", contains = c("VirtualPeriodicModel", "VIRTUAL"))
setClass("VirtualPeriodicAutocorrelations", contains = c("VirtualPeriodicModel", "VIRTUAL"))

## based on .acv2acr()
.acvLagged2acr <- function(x, maxlag, ...){  ## TODO: include arg. lag_0 ?
    ## assumes that x[...], x[[]]  respect the conventions of indexing for Lagged
    ## this function is separate for now  to avoid repeating it in methods.

    if(missing(maxlag))
        maxlag <- maxLag(x) # TODO: document that this assumes suitable maxLag() method.

    sd <- sqrt(x[[0]])
        # 2020-06-11: The note below is baffling as the line following 'was:' is still in 
        #     the code. Removing '[...]' now.
        # 
        # 2019-05-15 was: pc_sdfactor(sd, maxlag)[ , 1 + (0:maxlag)] # "1+" since "matrix"
        #     but it is redundant to subset the result of  pc_sdfactor() !
        #     (and [ , ...] will throw error if maxlag = 0)

    fac <- pc_sdfactor(sd, maxlag)
    acv <- x[0:maxlag] # autocovariances(x, maxlag, ...)

    acf <- acv / fac # "matrix"
    new("Lagged2d", data = acf)
}

setMethod("autocorrelations",
          signature(x = "VirtualPeriodicAutocovariances", maxlag = "ANY", lag_0 = "missing"),
          function(x, maxlag, ...){  ## TODO: include arg. lag_0 ?
              res <- if(missing(maxlag))
                         .acvLagged2acr(x, ...)
                     else
                         .acvLagged2acr(x, maxlag, ...)
              ## setting a more specialised class is left to methods in subclasses
              ## .set_acf_class(x, res)
              res
          })

.set_acf_class <- function(x, res, src_pat = "PeriodicAutocovariances",
                           replacement = "PeriodicAutocorrelations"){
    if(grepl(src_pat, class(x))){
        newcls <- sub(src_pat, replacement, class(x))
        new(newcls, data = res, x)
    }else{
        res
    }
}

setMethod("autocovariances", "VirtualPeriodicAutocovariances",
          function(x, maxlag, ...){
              if(!missing(maxlag) && maxlag != maxLag(x))
                  maxLag(x) <- maxlag
              x ## to do: so, the result keeps the full structure of x. 
                ##       Maybe should just return x[0:maxLag]
                ## Decision: This is fine but should be documented.
                ##           Otherwise subclasses would need to define methods just to 
                ##           put back the class and potentially other information.
          })

setMethod("partialVariances", "VirtualPeriodicAutocovariances",
          function(x){
              alg1utilNew_new(x, "fv")
          })

setMethod("backwardPartialVariances", "VirtualPeriodicAutocovariances",
          function(x){
              alg1utilNew_new(x, "bv")
          })


setMethod("partialAutocorrelations", "VirtualPeriodicAutocovariances",
          function(x, lag_0 = "var"){     # use lag_0="var" if you wish parcor_k(0)=R_k(0)
              alg1utilNew_new(x, "be", lag_0)
          })

setMethod("partialAutocovariances", "VirtualPeriodicAutocovariances",
          function(x, ...){
              ## note: res will have some partialAutocorrelations class!
              res <- partialAutocorrelations(x)
                  # 2019-05-14 was:  res[, 0]
                  #   should work with both Lagged2d and slMatrix in ('lagged' v. >= 0.2.2)
              var <- res[[0]] # assumes variances are in lag 0
              ## TODO (2019-05-17): is pc_sdfactor applicable to partial autocorrelations?
              ##       maybe this is somewhere in my old notes?
              fac <- pc_sdfactor( sqrt(var), maxLag(res) )
              ## 2019-05-17 was:  res <- res[] * fac
              ##    but surely the lag 0 entries should not be changed? 
              ##    It would be ok if lag 0 values were equal to 1 but the variances
              ##    are there already.
              res <- res[] # make a "matrix"
              res[ , -1] <- (res * fac)[ , -1]
              new("Lagged2d", data = res)
          })

setMethod("pc.phis2", "VirtualPeriodicAutocovariances",
          function(x, p){
              res <- calc_pc.phis2(x[], p)
              res$phi <- new("Lagged2d", data = res$phi) # slMatrix(res$phi)
              res   # list(phi = res, s2 = res$innov)
          })

setMethod("pc.fL", "VirtualPeriodicAutocovariances",
          function(x, p, from = 1, to = 6){
              calc_pc.fL(x[], p, from, to)
          })

setMethod("pc.bU", "VirtualPeriodicAutocovariances",
          function(x, p, from = 1, to = 6){
              calc_pc.bU(x[], p, from, to)
          })

## TODO: think this over, "predictionCoefficients" here are PAR coefficients. Is this the
##       intent? Maybe should clear the terminology.
##
## Answer: Actually, this is a function with second argument p (integer vector), so clearly
##         it asks for PAR coefficients. Other types of p may ask for other things.

setMethod("partialCoefficients", "VirtualPeriodicAutocovariances",
          function(x, p){
              acv <- x[] # as.matrix(x)
              res <- calc_predictionCoefficients(acv, p)
              new("Lagged2d", data = res) # slMatrix(res)
          })

setMethod("backwardPartialCoefficients", "VirtualPeriodicAutocovariances",
          function(x, p){             # handle properly the case p[i]=0
              res <- calc_backwardPredictionCoefficients(x[], p)
              new("Lagged2d", data = res) # slMatrix(res)
          })

setClass("VirtualPeriodicMeanModel",  contains = c("VirtualPeriodicModel", "VIRTUAL"))

setClass("VirtualPeriodicAutocovarianceModel", contains = c("VirtualPeriodicModel", "VIRTUAL"))

setMethod("[[", signature(x = "VirtualPeriodicAutocovarianceModel", i = "numeric"),
          function(x, i){
              drop(x[i])
          })

setMethod("[", signature(x = "VirtualPeriodicAutocovarianceModel",
                             i = "numeric", j = "numeric"),
          function(x, i, j, ..., drop = FALSE){
                                        # TODO: arbitrary constant, 10, in max(10, j)
              wrk <- autocovariances(x, maxlag = max(10, j))
               if(!is(wrk, "slMatrix"))
                  wrk <- slMatrix(as.matrix(wrk))
              wrk[i, j, ..., drop = drop]
          })

setMethod("[", signature(x = "VirtualPeriodicAutocovarianceModel", i = "numeric",
                                                                   j = "missing"),
          function(x, i, j, ..., drop = FALSE){
              wrk <- autocovariances(x)
              ## if(!is(wrk, "slMatrix"))
              ##     wrk <- slMatrix(as.matrix(wrk))
              ## wrk[i, ..., drop = drop]
              wrk[i, ..., drop = drop]
          })

setMethod("[", signature(x = "VirtualPeriodicAutocovarianceModel", i = "missing",
                                                                   j = "numeric"),
          function(x, i, j, ..., drop = FALSE){
                                        # todo: arbitrary constant, 10, in max(10, j)
              wrk <- autocovariances(x, maxlag = max(10, j))
              if(!is(wrk, "slMatrix"))
                  wrk <- slMatrix(as.matrix(wrk))
              wrk[ , j, ..., drop = drop]
          })

setMethod("[", signature(x = "VirtualPeriodicAutocovarianceModel", i = "missing",
                                                                   j = "missing"),
          function(x, i, j, ..., drop = FALSE){
              as.matrix(autocovariances(x))
          })

setMethod("autocorrelations",
          signature(x = "VirtualPeriodicAutocovarianceModel", maxlag = "ANY", lag_0 = "missing"),
          function(x, maxlag, ...){  ## TODO: include arg. lag_0 ?
              acv <- if(missing(maxlag)) autocovariances(x, ...)
                     else  autocovariances(x, maxlag, ...)
              autocorrelations(acv)
          })

setClass("VirtualPeriodicStationaryModel", 
         contains = c("VirtualPeriodicAutocovarianceModel", "VirtualPeriodicMeanModel",
                      "VIRTUAL"))

setClass("VirtualPeriodicWhiteNoiseModel",
         contains = c("VirtualPeriodicStationaryModel", "VIRTUAL"))

setClass("VirtualPeriodicFilterModel",  contains = c("VirtualPeriodicModel", "VIRTUAL"))

setClass("VirtualPeriodicArmaModel", 
         contains = c("VirtualPeriodicFilterModel", "VirtualPeriodicStationaryModel", 
                      "VIRTUAL"))

setClass("VirtualPeriodicArModel", contains = c("VirtualPeriodicArmaModel", "VIRTUAL"))
setClass("VirtualPeriodicMaModel", contains = c("VirtualPeriodicArmaModel", "VIRTUAL"))

setClass("ModelCycleSpec", slots = c(modelCycle = "BasicCycle"))

setMethod("modelCycle", "ModelCycleSpec", 
          function(object){
              if(length(nSeasons(object@modelCycle)) > 0)
                  object@modelCycle
              else
                  NULL
          })

setReplaceMethod("modelCycle", "ModelCycleSpec",
                 function(object, ..., value){
                     nseas <- nSeasons(object@modelCycle)
                     ## allow change if nseas is zero, to be able to initialise;
                     if(length(nseas) > 0  &&  nSeasons(object) != nSeasons(value))
                         stop("attempt to change the number of seasons")
                     object@modelCycle <- value
                     object
                 })

setClass("PeriodicInterceptSpec", contains = c("ModelCycleSpec", "InterceptSpec"),
         prototype = list(center = numeric(0), intercept = numeric(0), sigma2 = numeric(0))
         )

.concord_cis <- function(.Object, nseasons){
    if(missing(nseasons))
        nseasons <- max(length(.Object@center),
                        length(.Object@intercept),
                        length(.Object@sigma2))
    ## nseas <- nSeasons(.Object@modelCycle)
    ## if(length(nseas) > 0){
    ##     if(nseasons <= 1)
    ##         nseasons <- nseas
    ##     else if(nseasons != nseas)
    ##         stop("inconsistent specification of the number of seasons")
    ## }else if(nseasons > 0)
    ##     .Object@modelCycle <- new("BareCycle", nseasons)
        
    if(nseasons == 0)
         return(.Object)

    if(length(.Object@sigma2) == 0)
        .Object@sigma2 <- rep(NA_real_, nseasons)

    for(nam in c("center", "intercept", "sigma2")){
        switch(1 + length(slot(.Object, nam)),
               ## 0
               slot(.Object, nam) <- rep(0, nseasons),
               ## 1
               slot(.Object, nam) <- rep(slot(.Object, nam), nseasons),
               ## > 1
               if(length(slot(.Object, nam)) != nseasons)
                   stop(paste0("length of '", nam, "' is not equal to nseasons'"))
               )
    }
    .Object
}

## TODO: this is horribly convoluted!
setMethod("initialize", "PeriodicInterceptSpec",
            function(.Object, ..., nseasons){
                .Object <-  callNextMethod(.Object, ...)
                .Object <- .concord_cis(.Object, nseasons)
                if(length(nSeasons(.Object@modelCycle)) == 0)
                    .Object@modelCycle <- new("BareCycle", length(.Object@center))
                .Object
            }
            )

setMethod("nSeasons", "PeriodicInterceptSpec",
          function(object){
                  # this was a check if --as-cran complains if a slot that might not exist is
                  # accessed
                  #
                  # if(.hasSlot(object, "cycle")){
                  #     cat("Has slot 'cycle'!\n")
                  #     if(length(nSeasons(slot(object, "cycle"))) > 0){
                  #         cat("nSeasons(cycle)' has positive length!\n")
                  #         nSeasons(slot(object, "cycle"))
                  #     }else{
                  #         length(object@center)
                  #     }
                  # }else
                  #     length(object@center) 
              length(object@center)  # or: nSeasons(modelCycle)
          }
          )

## setMethod("allSeasons", c(x = "PeriodicInterceptSpec"), 
##           function (x, abb, ...){
##               allSeasons(x@modelCycle, abb, ...)
##           }
##           )

setMethod("sigmaSq", "PeriodicInterceptSpec", function(object) object@sigma2 )

setClass("PeriodicArmaSpec", contains = c("PeriodicArmaFilter", "PeriodicInterceptSpec"))

## TODO: missing ar or ma (or both but what would be the number of seasons then?)
setMethod("initialize", "PeriodicArmaSpec",
          function(.Object, ..., mean){
              .Object <-  callNextMethod(.Object, ...)

              ## Note: it is tempting to think that after the above call the constructor for
              ## PeriodicInterceptSpec has already been called but this seems not to be the
              ## case. In particular, the code below for setting the mean cannot assume that
              ## center, intercept and sigma2 have the same length

              nseas <- nSeasons(.Object@ar) # should be equal to nSeasons(.Object@ma)

              .Object <- .concord_cis(.Object, nseasons = nseas)

              if(missing(mean)){
                  if(length(.Object@center) != nseas){      # adjust the number of seasons
                          obj <- as(.Object, "PeriodicInterceptSpec")
                          as(.Object, "PeriodicInterceptSpec") <-
                                   new("PeriodicInterceptSpec", obj, nseasons = nseas)
                  }
              }else{
                  if(length(mean) != nseas){
                      if(length(mean) == 1)
                          mean <- rep(mean, nseas)
                      else
                          stop("'mean' should have length 'nseasons' or one")
                  }

                      # for easier handling of NA's, using unique() rather than all() or any()
                      #        all(.Object@center == 0) && all(.Object@intercept == 0)
                  u <- unique(c(.Object@center, .Object@intercept))
                  if(length(u) == 0){ # all components are numeric(0) here, including sigma2
                      as(.Object, "PeriodicInterceptSpec") <-
                                            new("PeriodicInterceptSpec", center = mean)
                  }else if(length(u) == 1 && !is.na(u) && u == 0){
                      if(length(.Object@center) == nseas)
                          .Object@center[] <- mean
                      else{
                          obj <- as(.Object, "PeriodicInterceptSpec")
                          as(.Object, "PeriodicInterceptSpec") <-
                                            new("PeriodicInterceptSpec", obj, center = mean)
                          ## the above is essentially:
                          ##     .Object@center <- mean
                          ##     .Object@intercept <- rep(0, nseas)
                          ##     .Object@sigma2 <- rep(.Object@sigma2, nseas)
                          ## plus consistency checking.
                      }
                  }else
                      stop(paste0("Use argument 'mean' only when 'center' and 'intercept' ",
                                  "are missing or zero"))
              }
              .Object
          }
          )

## TODO: pcMean, pcIntercept - ne, tezi may sa za "PeriodicArmaModel"

setClass("PeriodicAutocovariances",
         contains = c("ModelCycleSpec", "FlexibleLagged", "VirtualPeriodicAutocovariances"))
setClass("PeriodicAutocorrelations", 
         contains = c("ModelCycleSpec", "FlexibleLagged", "VirtualPeriodicAutocorrelations"))
setClass("PartialPeriodicAutocorrelations",
         contains = c("ModelCycleSpec", "FlexibleLagged", "VirtualPeriodicAutocorrelations"))

## setClass("PartialPeriodicAutocovariances",  contains = c("FlexibleLagged"))
## setClass("PartialPeriodicVariances",        contains = c("FlexibleLagged"))
## setClass("ComboPeriodicAutocovariances",    contains = c("FlexibleLagged"))
## setClass("ComboPeriodicAutocorrelations",   contains = c("FlexibleLagged"))

setMethod("autocorrelations",
          signature(x = "PeriodicAutocovariances", maxlag = "ANY", lag_0 = "missing"),
          function(x, maxlag, ...){  ## TODO: include arg. lag_0 ?
              wrk <- callNextMethod()
              new("PeriodicAutocorrelations", data = wrk)
          })

setMethod("partialAutocorrelations",
          signature(x = "PeriodicAutocovariances", maxlag = "ANY", lag_0 = "missing"),
          function(x, maxlag, ...){  ## TODO: include arg. lag_0 ?
              wrk <- callNextMethod()
              new("PartialPeriodicAutocorrelations", data = wrk)
          })

setClass("SamplePeriodicAutocovariances",         
         contains = c("PeriodicAutocovariances",         "Fitted"))
setClass("SamplePeriodicAutocorrelations",        
         contains = c("PeriodicAutocorrelations",        "Fitted"))
## setClass("SamplePartialPeriodicAutocovariances",  
##          contains = c("PartialPeriodicAutocovariances",  "Fitted"))
## setClass("SamplePartialPeriodicAutocorrelations", 
##          contains = c("PartialPeriodicAutocorrelations", "Fitted"))
## setClass("SamplePartialPeriodicVariances",        
##          contains = c("PartialPeriodicVariances",        "Fitted"))
## TODO: FittedComboXX variants?

setMethod("autocorrelations",
          signature(x = "SamplePeriodicAutocovariances", maxlag = "ANY", lag_0 = "missing"),
          function(x, maxlag, ...){  ## TODO: include arg. lag_0 ?
              wrk <- callNextMethod()
                                   # or just 'x' instead of 'as(x, "Fitted")'
              new("SamplePeriodicAutocorrelations", wrk, as(x, "Fitted"))
          })

setClass("PeriodicArmaModel",
         contains = c("VirtualPeriodicArmaModel", "PeriodicArmaSpec")
        )

setClass("PeriodicArModel", contains = c("VirtualPeriodicArModel", "PeriodicArmaModel"))
setClass("PeriodicMaModel", contains = c("VirtualPeriodicMaModel", "PeriodicArmaModel"))

setClass("PeriodicFilterModel",
         contains = c("VirtualPeriodicFilterModel", "PeriodicArmaSpec"))

setClass("PeriodicIntegratedArmaSpec",
         slots = c(pcmodel = "PeriodicArmaModel")
         )

setMethod("nSeasons", "PeriodicIntegratedArmaSpec",
          function(object) nSeasons(object@pcmodel) )
setMethod("sigmaSq", "PeriodicIntegratedArmaSpec",
          function(object) sigmaSq(object@pcmodel) )

setClass("SiPeriodicArmaModel",
         contains = c("VirtualPeriodicFilterModel", "PeriodicIntegratedArmaSpec"),
         slots = c(iorder = "numeric", siorder = "numeric")
         )
setClass("SiPeriodicArModel", contains = "SiPeriodicArmaModel" ) # , validity = .valid_par
setClass("SiPeriodicMaModel", contains = "SiPeriodicArmaModel")  # , validity = .valid_pma

setClass("PiPeriodicArmaModel",
         contains = c("VirtualPeriodicFilterModel", "PeriodicIntegratedArmaSpec"),
         slots = c(piorder = "numeric", picoef = "matrix")
         )

## TODO: is it possible to specialize pcmodel = "PeriodicArModel" and "PeriodicMaModel" here?
setClass("PiPeriodicArModel", contains = "PiPeriodicArmaModel") # , validity = .valid_par
setClass("PiPeriodicMaModel", contains = "PiPeriodicArmaModel") # , validity = .valid_pma

  setClass("LegacyPeriodicFilterModel", contains = c("VirtualPeriodicFilterModel"),
           slots = c(intercept = "numeric",
                     arFilter = "PeriodicBJFilter",
                     maFilter = "PeriodicSPFilter",
                     scalesq = "numeric", # changed from pwn = "PeriodicWhiteNoiseModel"
                     jordan = "ANY"
                     ## , cycle = "BasicCycle" - dropping but need nSeasons()
           )
	   )

## TODO: numeric(0) is not the most appropriate default here
setGeneric("PeriodicArmaModel",
           ## needs more thought, also what arguments to allow
           function(object, mean, ar = numeric(0), ma = numeric(0), ...)
               standardGeneric("PeriodicArmaModel"),
           signature = c("object"))

##### setMethod("PeriodicArmaModel", signature = c(object = "missing"),
#####           function(object, mean, ar, ma, ...){
#####               spec <- new("PeriodicArma", ar = ar, ma = ma)
#####               if(missing(mean))
#####                   new("PeriodicArmaModel", acv = spec)
#####               else
#####                   new("PeriodicArmaModel", acv = spec, mean = mean)
#####           }
#####           )
#####
##### setMethod("PeriodicArmaModel", signature = c(object = "PeriodicArmaVirtual"),
#####           function(object, mean){
#####               if(missing(mean))
#####                   new("PeriodicArmaModel", acv = object)
#####               else
#####                   new("PeriodicArmaModel", acv = object, mean = mean)
#####           }
#####           )

setGeneric("PeriodicArModel",
           ## 2017-06-03 was: function(object, sigma2, mean, intercept, order, ...)
           function(object, ...)
               standardGeneric("PeriodicArModel"),
           signature = c("object"))

setMethod("PeriodicArModel", signature = c(object = "VirtualPeriodicArmaModel"),
          function(object){
              as(object, "PeriodicArModel")
          }
          )

setMethod("PeriodicArModel", signature = c(object = "PeriodicMonicFilterSpec"),
          function(object, ...){
              new("PeriodicArModel", ar = object, ...)
          }
          )

setMethod("PeriodicArModel", signature = c(object = "numeric"), # object is the order here
          function(object, ...){
              arfilt <- new("PeriodicBJFilter",  order = object)
              PeriodicArModel(arfilt, ...)
          }
          )

setMethod("PeriodicArModel", signature = c(object = "matrix"), # object is coef here
          function(object, ..., order){
              arfilt <- if(missing(order))
                            new("PeriodicBJFilter",  coef = object)
                        else
                            new("PeriodicBJFilter",  coef = object, order = order)
                  # TODO: replace with: new("PeriodicArModel", ar = arfilt, ...) ?
              PeriodicArModel(arfilt, ...)
          }
          )

setGeneric("PeriodicMaModel",
           function(object, ...)
               standardGeneric("PeriodicMaModel"),
           signature = c("object"))

setMethod("PeriodicMaModel", signature = c(object = "VirtualPeriodicArmaModel"),
          function(object){
              as(object, "PeriodicMaModel")
          }
          )

setMethod("PeriodicMaModel", signature = c(object = "PeriodicMonicFilterSpec"),
          function(object, ...){
              new("PeriodicMaModel", ma = object, ...)
          }
          )

setMethod("PeriodicMaModel", signature = c(object = "numeric"), # object is the order here
          function(object, ...){
              filt <- new("PeriodicSPFilter",  order = object)
              PeriodicMaModel(filt, ...)
          }
          )

setMethod("PeriodicMaModel", signature = c(object = "matrix"), # object is coef here
          function(object, ..., order){
              filt <- if(missing(order))
                          new("PeriodicSPFilter",  coef = object)
                      else
                          new("PeriodicSPFilter",  coef = object, order = order)
                  # TODO: replace with: new("PeriodicMaModel", ar = arfilt, ...) ?
              PeriodicMaModel(filt, ...)
          }
          )

## ## Methods for "PeriodicWhiteNoiseModel"
## setMethod("sigmaSq", "PeriodicWhiteNoiseModel",
##           function(object) object@scalesq)

#####     setMethod("pcMean", signature("PeriodicMeanModel"),
#####               function(object)
#####                   pcMean(object@mean)
#####              )
#####
#####     setMethod("pcMean", signature("PeriodicMean"),
#####               function(object)
#####                   as(object, "numeric")
#####              )

#####     ## Tryabva da ima i model, za da mozhe da napravi tova.
#####     ## (ili da ima argument, koyto da predostavya modela)
#####     ##
#####     setMethod("pcMean", signature("PeriodicIntercept"),
#####               function(object)
#####                   stop("Need model to compute this, not defined on its own.")
#####              )

setMethod("pcMean", signature("VirtualPeriodicArmaModel"),
          function(object){
              value <- if(all(object@intercept == 0))
                           ## this is by definition of the specification of the model
                           ## TODO: document
                           object@center
                       else{
                           ## wasteful but general
                           intercept <- pcIntercept(object)
                           intercept2permean(intercept,
                                             coef = filterCoef(object@ar),
                                             order = filterOrder(object@ar))
                       }
              structure(value, names = allSeasons(object))
          }
          )

setMethod("pcIntercept", signature("VirtualPeriodicArmaModel"),
          function(object){
              value <- if(all(object@center == 0))
                           object@intercept
                       else
                           object@intercept +
                               permean2intercept(object@center,
                                                 coef = filterCoef(object@ar),
                                                 order = filterOrder(object@ar) )
              structure(value, names = allSeasons(object))
          }
          )

setMethod("autocovariances", "PeriodicArModel",
          function(x, maxlag, ...){
              sigma2 <- sigmaSq(x)
              coef <- filterCoef(x@ar) # was: arCoef(x)
              p <- filterOrder(x@ar) # arOrder(x)
              if(missing(maxlag))
                  maxlag <- max(10L, p)

		         # res is a "matrix"
              res <- pcAR2acf(coef, sigma2, p, maxlag)
                         # 2017-05-31: it was returning the raw 'res' before
              res <- new("Lagged2d", data = res)
                      # 2019-05-14 was:  res              
              new("PeriodicAutocovariances", data = res)
          })

## TODO: this is before version 0.7-0
setMethod("partialCoefficients", "PeriodicArModel",
          function(x, p){
              if(all(p >= filterOrder(x@ar) ))  # was: arOrder(x)
                  filterCoef(x@ar) # was: arCoef(x)
              else
                  callNextMethod()
          })

setMethod("autocovariances", "PeriodicArmaModel",
          function(x, maxlag, ...){
              sigma2 <- sigmaSq(x)

              coef <- filterCoef(x@ar) # was: arCoef(x)
              p <- filterOrder(x@ar) # was: arOrder(x)

              macoef <- filterCoef(x@ma) # was: maCoef(x)
              q <- filterOrder(x@ma) # was: maOrder(x)


              if(missing(maxlag)) # TODO: tova izglezhda kato krapka
                  maxlag <- max(10L, p)

		      # res is "matrix"
              res <- pcArma2acf(coef, sigma2, p, macoef, q, maxlag)
                      # 2017-05-31: it was returning the raw 'res' before
              res <- new("Lagged2d", data = res)
                      # 2019-05-14 was:  res              
              new("PeriodicAutocovariances", data = res)
          })

## "PeriodicArmaModel"
setAs("PeriodicArmaModel", "list",
      function(from){
          list(ar = filterCoef(from@ar), # was: arCoef(from),
               ma = filterCoef(from@ma), # was: maCoef(from),
               mean = from@mean,
               signConvention = "BD" ## cannot give parameters to setAs to change this.
               )
      }
      )

setAs("PeriodicArmaModel", "PeriodicArModel",
      function(from){
          if(max(filterOrder(from@ma)) > 0)  # was: maOrder(from)
              stop("Moving average order is not zero.")
          # 2017-06-04 was:
          #     new("PeriodicArModel", ar = from@ar, const = from@const, sigma2 = from@sigma2)
          new("PeriodicArModel", from)
      }
      )

## setMethod("show", "VirtualPeriodicAutocorrelations",
##     function (object){
##         ## .reportClassName(object, "Fitted")
##         cat('An object of class "', class(object), '"\n', sep = "")
##         callNextMethod()
##     })

setMethod("show", "PartialPeriodicAutocorrelations",
    function (object){
        .reportClassName(object, "PartialPeriodicAutocorrelations")
        callNextMethod()
    })

setMethod("show",
    signature(object = "PeriodicInterceptSpec"),
    function (object){
        .reportClassName(object, "PeriodicInterceptSpec")
        ## .print_cis(object, unconditional = TRUE)
        callNextMethod()
    })

setMethod("show",
    signature(object = "PeriodicArmaSpec"),
    function (object){
        .reportClassName(object, "PeriodicArmaSpec")

        ## TODO: use this to name the means and s.d.'s below?
        ## seas_ind <- allSeasons(object, abb = TRUE)

        ## TODO: .print_mis(object)
        cat("Number of seasons: ", nSeasons(object), "\n")
        if(all(object@intercept == 0))
            cat("Mean:    ", object@center,    "\n")
        else if(all(object@center == 0))
            cat("Intercept: ", object@intercept, "\n")
        else{ # both have non-zero values
            cat("Center:    ", object@center,    "\n")
            cat("Intercept: ", object@intercept, "\n")
        }
        cat("SigmaSq:   ", object@sigma2,    "\n")
        cat("\n")
        ## 2019-05-21: was:  callNextMethod()
        ##     Note: could move this code to PeriodicArmaFilter and call that.
        ## TOD0: need better control of dimnames of @coef slots
        mat <- matrix(NA_real_, nrow = nSeasons(object), ncol = 0)
        arma_flag <- c(FALSE, FALSE)

	seas_names <- allSeasons(object, abb = TRUE)

        if(max(object@ar@order) > 0){
            arma_flag[1] <- TRUE
            ar_mat <- object@ar@coef
            dimnames(ar_mat) <- list(season = seas_names,
                                    lag = paste0("ar", 1:ncol(ar_mat)) )
            mat <- cbind(mat, ar_mat)
        }
        if(max(object@ma@order) > 0){
            arma_flag[2] <- TRUE
            ma_mat <- object@ma@coef
            dimnames(ma_mat) <- list(season = seas_names,
                                    lag = paste0("ma", 1:ncol(ma_mat)) )
            mat <- cbind(mat, ma_mat)
        }
        ## TODO: print blanks for when i > p[i]
        ##       (don't replace all NA's since some may be for i <= p[i]
        ##
        ## TODO: cater for the case when all orders are zero.
	arma <- paste0(c(c(paste0("AR(", paste0(object@ar@order, collapse = ","), ")")),
                         c(paste0("MA(", paste0(object@ma@order, collapse = ","), ")"))
                         )[arma_flag], collapse = ", ")
        cat("Periodic order: ", arma, "\n")
        cat("\n")
        print(mat)


        invisible() # todo: return 'object' even though default show doesn't?                            
    })

setMethod("show", "PeriodicFilterModel",
          function(object){
              .reportClassName(object, "PeriodicFilterModel")
              callNextMethod()
              #### cat("Number of seasons: ", nSeasons(object), "\n")
              #### cat("Intercept: ", object@intercept, "\n")
              #### cat('\nslot "arFilter":\n')
              #### show(object@arFilter)
              #### cat('\nslot "maFilter":\n')
              #### show(object@maFilter)
              #### cat('\nslot "pwn":\n')
              #### show(object@pwn)
              ## cat("Slot 'jordan' is not shown here.\n")
              ## cat("Call 'showDefault' to see all info stored in the object.\n")
          })

setMethod("show",
    signature(object = "PeriodicArmaModel"),
    function (object){
        .reportClassName(object, "PeriodicArmaModel")
        ## TODO: .print_mis(object)
        callNextMethod()
        ## cat("\n")
        ## cat('\nslot "ar":  ')
        ## show(object@ar)
        ## cat('\nslot "ma":  ')
        ## show(object@ma)
    })

setMethod("show",
    signature(object = "PeriodicArModel"),
    function (object){
        .reportClassName(object, "PeriodicArModel")
        ## TODO: avoid printing the ma slot?
        callNextMethod()
    })

setMethod("show",
    signature(object = "PeriodicMaModel"),
    function (object)
    {
        .reportClassName(object, "PeriodicMaModel")
        ## TODO: avoid printing the ar slot?
        callNextMethod()
    })

setMethod("show",
    signature(object = "PiPeriodicArmaModel"),
    function (object){
        .reportClassName(object, "PiPeriodicArmaModel")
        picoef <- object@picoef
        rownames(picoef) <- allSeasons(object)

        cat("PI order: ", object@piorder, "\n")
        cat("PI coefficients:\n")
        print(picoef)
        cat("PAR part of the model, ")
        # callNextMethod()
        show(object@pcmodel)
    })

setMethod("show",
    signature(object = "PiPeriodicArModel"),
    function (object){
        .reportClassName(object, "PiPeriodicArModel")
        callNextMethod()
    })


setMethod("show",
    signature(object = "SiPeriodicArModel"),
    function (object){
        .reportClassName(object, "SiPeriodicArModel")
        cat("iorder: ", object@iorder, ", siorder: ", object@siorder, "\n", sep = "")
        cat("PAR part of the model, ")
        show(object@pcmodel)
    })

#### setMethod("show", "PeriodicWhiteNoiseModel",
####           function(object){
####               .reportClassName(object, "PeriodicWhiteNoiseModel")
####               ## cat("period: ", nSeasons(object), "\n")
####               cat("Periodic white noise:",
####                   "\n\tstd. dev. = ", object@scale,
####                   "\n\tvariance  = ", object@scalesq, "\n")
####           }
####           )

#### setMethod("show", "PeriodicArModel",
####           function(object){
####               .reportClassName(object, "PeriodicArModel")
####               cat("Number of seasons: ", nSeasons(object), "\n")
####               cat("Intercept: ", object@intercept, "\n")
####               cat("\nPAR filter:\n")
####               callNextMethod()
####               ## show(object@arFilter)
####               ## show(object@pwn)
####               ## cat("Slot 'jordan' is not shown here.\n")
####               cat("Call 'showDefault' to see all info stored in the object.\n")
####           }
####           )

#### setMethod("show", "SiParModel",
####           function(object){
####               cat("An object of class SiParModel", "\n")
####               cat("Integration Order: ", object@iorder, "\n")
####               cat("Seasonal Integration Order: ", object@siorder, "\n")
####               cat("\n")
####               cat("PAR component: ", "\n")
####               show(object@par)
####               cat("\n")
####               cat("Expanded form of the model:", "\n")
####               callNextMethod()
####           }
####           )

setMethod("plot", c(x = "PeriodicAutocorrelations", y = "missing"),
          function(x, y, main = NULL, oma, mar, ...){
              ## for now just call the base "ts" method
              if(is.null(main))
                  ## not deparse(substitute(x)), since the method is nested
                  main <- deparse(substitute(x, parent.frame()))
              nseas <- nSeasons(x)
              seasnames <- allSeasons(x, abb = TRUE)
              acrv <- x[]

              oldpar <- par(mfrow = c(nseas, 1))  # TODO: split when large?
              on.exit(par(oldpar))


              if(missing(oma)){
                  if(nseas > 6)
                      oma <- c(0, 0, 0, 0)
                  else
                      oma <- c(4, 0, 2, 0)
              }              
              if(missing(mar)){
                  if(nseas > 6)
                      mar <- c(0, 4.1, 0, 0)
                  else
                      mar <- c(5.1, 4.1, 4.1, 2.1) # default margins
              }              

              par(oma = oma) # outer margins
              par(mar = mar)

              maxlag <- maxLag(x)
              lags <- 0:maxlag

              top_text <- paste0("Periodic autocorrelations of object ", x@objectname)

              if(is(x, "SamplePeriodicAutocorrelations")){
                  flag_fitted <- TRUE
                  n <- x@n / nseas # TODO: need to consider individual seasons
                                #  but pcacf_pwn_var() needs adjustment too.
                  se <- sqrt( pcacf_pwn_var(n, nseas, lags, 1:nseas) )
              }else
                  flag_fitted <- FALSE


              for(i in nseas:1){
                  plot(lags, acrv[i, ],  
                        axes = FALSE, 
                        type = "h", ylim = c(-1, 1), 
                        ylab = seasnames[i],
                        xlab = ""
                        , xaxs = "i", yaxs = "i" 
                       # ann = FALSE
                         )
                  lines(c(0, maxlag), c(0,0))
                  if(flag_fitted){
                      lines(0:maxlag,  qnorm(0.975) * se[i, 1:(maxlag + 1)], col = "blue")
                      lines(0:maxlag, -qnorm(0.975) * se[i, 1:(maxlag + 1)], col = "blue")
                  }
                  # box(col = "grey")
              }
              mtext(top_text, side = 3, outer = TRUE)
              ## draw x-axis for the bottom plot
              axis(1, at = 0:maxlag, 0:maxlag, outer = TRUE)
            invisible()

          })
