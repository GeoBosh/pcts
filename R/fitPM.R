## fitPM <- function(model, x, ...){
##     stop("Function 'fitPM' doesn't have a method for 'model' of class ",
##          class(model), "." )
## }
setGeneric("fitPM", function(model, x, ...){ standardGeneric("fitPM") })

setMethod("fitPM", signature(model = "numeric", x = "ANY"), # simplified call for PAR
          function(model, x, ...){
              if(any(model - round(model) != 0))
                  stop("The PAR orders must be non-negative integer numbers.")
              if(length(model) == 1)
                  ## try to infer the number of seasons
                  model <- rep(model, nSeasons(x))

              model <- PeriodicArModel(model)
              fitPM(model, x, ...)
          })

setMethod("fitPM", signature(model = "PeriodicArModel", x = "ANY"),
          function(model, x, ...){
              if(is.matrix(x) && ncol(x) > 1)
                  stop("multivariate PAR fitting not implemented yet")
              ## convert 'x' to PeriodicTS, throw error if unsuccessful
              x <- pcts(as.numeric(x), nseasons = nSeasons(model)) 
              fitPM(model, x, ...)
          })

setMethod("fitPM", signature(model = "PeriodicArModel", x = "PeriodicMTS"),
          function(model, x, ...){
              if(nVariables(x) != 1)
                  stop("Multivariate case not implemented yet")
              
              x <- x[[1]]
              fitPM(model, x, ...)
          })

setMethod("fitPM", signature(model = "PeriodicArModel", x = "PeriodicTS"),
          function(model, x, ...){
              x_orig <- x
              period <- nSeasons(model)  # for now ignore the content, estimate all coeff.
              p <- filterOrder(model@ar)
              x <- as.numeric(x)

              mo <- num2pcpar(x, order = p, period = period, ...)
              m <- - mo$co@m[ , -1, drop = FALSE] # num2pcpar and pc.fcoeff() return coef in
                                                  # filter form, so, drop the fist column and
                                                  # negate

              arfi <- new("PeriodicBJFilter", coef = m, order = p)
              par  <- new("PeriodicArModel", ar = arfi, sigma2 = mo$scale^2, mean = mo$mean)
              modelCycle(par) <- as(x_orig, "Cyclic")@cycle

              ## TODO: need classes for fitted models
              ## TODO: parcovmatlist() and pcacfMat() need adaptation to the new classes
              n <- length(x)
              ## TODO: this is crude, essentially assumes that x[1] is not missing, etc.
              ns <- rep(n %/% period, period)
              if((r <- n %% period) != 0)
                  ns[1:r] <- ns[1:r] + 1

              ## assemble the object; TODO: this needs streamlining                        
              asyCov_means <- meancovmat(par, n = n, result = "")
              asyCov_par <- parcovmatlist(par, n = n, result = "Matrix")
              asyCov <- .bdiag(list(asyCov_means, asyCov_par))

              ## TODO: this chunk should be a single call to new():
              res <- as(par, "FittedPeriodicArModel")
              res@asyCov <- asyCov
              res@ns <- ns
              res@theTS <- x_orig
              
              res
          })

setMethod("fitPM", signature(model = "SiPeriodicArModel", x = "ANY"),
          function(model, x, ...){
              d <- model@iorder
              if(d > 0){
                  x <- diff(as.numeric(x), differences = d)
                  x <- c(rep(NA, d), x)
              }

              ds <- model@siorder
              if(ds > 0){
                  x <- diff(as.numeric(x), lag = nSeasons(model) , differences = ds)
                  # x <- c(rep(NA, ds*nSeasons(model)), x)
              }

              if(is.numeric(x)){
                  indx <- max(which(is.na(x)))
                  if(indx > 0)
                      x <- x[-(1:indx)]
                  wrk <- length(x) %% nSeasons(model)
                  if(wrk>0)
                      x <- x[-(1:wrk)]
              }
              ## 2019-06-02 added mean = FALSE; this relies on the current fitPM passing it on
              ##     TODO: change the call when the complete procedure for fixing parameters
              ##           is implemented. 
              wrkmodel <- fitPM(model@pcmodel, x, mean = FALSE)
              new("SiPeriodicArModel", iorder = d, siorder = ds, pcmodel = wrkmodel)
          })

## pclspiar <- function(x, d, p, icoef = NULL, parcoef = NULL,
##                      sintercept = NULL,
##                      seasonof1st = 1,
##                      weights = TRUE,
##                      itol = 1e-7, maxniter = 1000)

setMethod("fitPM", signature(model = "PiPeriodicArModel", x = "ANY"),
          function(model, x, ...){
              d <- nSeasons(model)
              order <- filterOrder(model@pcmodel)$ar # filterOrder(model@ar) # arOrder(model)
              icoef <- model@picoef
              ## :TODO: :TODO:
              ##
              ## parcoef
              ## sintercept <- model@intercept
              ## seasonof1st
              piorder <- NCOL(icoef)
              p_piar <- piorder + max(order)  ## TODO: just for testing! !!!!!!!!!!!!!!!!!
#browser()
              wrkmodel <- pclspiar(x, d, p_piar, ...)
              intercept <- if(!is.null(wrkmodel$sintercept))
                               wrkmodel$sintercept
                           else
                               rep(0,d)

              parmodel <- PeriodicArModel(wrkmodel$parcoef, # coef
                                          order = order,
                                          intercept = intercept,
                                          sigma2 = wrkmodel$sigma2hat
                                          )

              res <- new("PiPeriodicArModel", 
                         piorder = piorder,
                         picoef = as.matrix(wrkmodel$icoef), # TODO: krapka, the constructor requires it
                         pcmodel = parmodel)
              res
          })

# setMethod("fitPM", signature(model = "PeriodicFilterModel", x = "ANY"),
#           function(x, model, ...){
#
#               ??? unfinished!
#
#               ## for now ignore the content, estimate all coeff.
#               period <- nSeasons(model)
#               p <- model@order
#
#               mo <- num2pcpar(x, order = p, period=period)
#               # browser()
#
#               m <- - mo$co@m[ , -1] # num2pcpar and pc.fcoeff() return coef in filter form,
#                                     # so, drop the fist column and negate
#                                     #
#                                     # TODO: all this needs consolidation!
#               res <- PeriodicArModel(m, order = p, mean = mo$mean, scale=mo$scale )
#
#               res
#           }
#           )

setMethod("fitPM", signature(model = "mcSpec", x = "ANY"),
          function(model, x, init, control = list(), optim.method = "minim", ...){
              ## 2014-08-18 krapka,
              ##    (1) predi arg "init" beshe zadalzhitelen
              ##    (2) argument "..." was ignored
              ##    (3) dobavyam argument "control"
              ##    (4) dobavyam argument "optim.method"
              ##
              ## Note that mC.ss() has argument "..." but the way it is written only "init"
              ## is not in the call to xx.ss().
              ##
              ##            was: optim.lst <- mC.ss(model, init = init)
              optim.lst <- if(missing(init)){
                  mC.ss(model)
              }else{
                  mC.ss(model, init = init)
              }

              optim.env <- optim.lst$env

              ## 2014-08-18  was:
              ##         optim.res <- optim.env[["minim"]](x, control=list(maxit=10000))
              if(is.null(control$maxit))  # 2014-08-18
                  control$maxit <- 10000
              optim.res <- optim.env[[optim.method]](x, control=control, ...) # fit the model

              period <-  optim.env$model$period     # or: model@mo
              p      <-  optim.env$model$p          # or: optim.env[["filter.order"]]
              coef   <-  optim.env$model$phi
              intercept <- 0 #todo: currently the above fitting procedure does not handle it.
              nseasons <- period
              mcparam <- optim.env$wrkmodel
              scalesq <- optim.env$mcsigma2(x)
                                        # TODO: temporary patch, needs more thought
              ## TODO: adapt the rest of this function for the new periodic filter model
              ##
              wrkspec <- list(fitspec = model
                              , mcparam = mcparam
                              , optim.res = optim.res
                              )

                  ## res <- PeriodicFilterModel(list(phi = coef, order = p, si2 = scalesq,
                  ##                                 order = nseasons),
                  ##                            intercept = intercept, jordan = wrkspec)

              res <- new("LegacyPeriodicFilterModel",
                         arFilter = new("PeriodicBJFilter", coef = coef, order = p),
                         maFilter = new("PeriodicSPFilter", order = rep(0, nseasons)),
                         scalesq = scalesq,
                         intercept = intercept,
                         jordan = wrkspec)

              res
          })

## residuals.FittedPeriodicArModel
residuals.FittedPeriodicArModel <- function (object, ...){ 
    model <- list(phi       = object@ar@coef,
                  p         = object@ar@order,
                  mean      = object@center,
                  intercept = object@intercept,
                  period = nSeasons(object)
                  )
    ts <- theTS(object)
    x <- as.numeric(ts)
    ## TODO: passing eps = NULL since it is not a default in pc.filter.
    ##       check if I have done this on purpose!
    wrk <- pc.filter(model = model, x = x, eps = NULL, whiten = TRUE)
                # TODO: omit the first few values?
                #       or, optionally set them to the exact residuals
                #       or, optionally, standardise?
                #       or, optionally, return a (periodic) time series?
    ts@.Data <- wrk
    theTS(object) <- ts
    ts
}

theTS <- function(object, ...){
    object@theTS
}

"theTS<-" <- function(object, ..., value){
    object@theTS <- value
}

setGeneric("as_pcarma_list", 
    function(object, ...){  standardGeneric("as_pcarma_list") })

setMethod("as_pcarma_list", "FittedPeriodicArModel",
          function (object, ...){ 
              list(phi       = object@ar@coef,
                   p         = object@ar@order,
                   mean      = object@center,
                   intercept = object@intercept,
                   period    = nSeasons(object)
                   )
          }
          )

setMethod("as_pcarma_list", "FittedPeriodicArmaModel",
          function (object, ...){ 
              list(phi       = object@ar@coef,
                   p         = object@ar@order,
                   theta     = object@ma@coef,
                   q         = object@ma@order,
                   mean      = object@center,
                   intercept = object@intercept,
                   period    = nSeasons(object)
                   )
          }
          )
