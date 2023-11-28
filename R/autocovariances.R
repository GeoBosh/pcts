## TODO: add a similar method for "ts" ?
setMethod("autocovariances", signature(x = "numeric"),
          function(x, maxlag, nseasons, ...){
              if(missing(nseasons)) # non-periodic
                  callNextMethod(x, maxlag, ...)
              else{ # periodic
                  objectname <- deparse(substitute(x, parent.frame())) # deparse(substitute(x))
                  n <- length(x)

                  x <- matrix(x, nrow = nseasons)  # !!! TODO: silently assumes n = N*d !!!
                  res <- pc.acf(x, maxlag, ...)
                       # 2017-05-31: it was returning the raw 'res' before

                  ## TODO: temporary patch, this needs more fundamental solution.
                  rownames(res) <- allSeasons(new("BareCycle", nseasons  = nseasons))
                  
                      # 2019-05-14 was:  new("Lagged2d", data = res)
                  new("SamplePeriodicAutocovariances", data = res, n = n, # varnames = varnames,
                                               objectname = objectname)
              }
          }
          )

setMethod("autocorrelations", signature(x = "numeric", maxlag = "ANY", lag_0 = "missing"),
          function(x, maxlag, nseasons, ...){
              if(missing(nseasons)) # non-periodic
                  callNextMethod(x, maxlag, ...)
              else{ # periodic
                  objectname <- deparse(substitute(x, parent.frame())) # deparse(substitute(x))
                  n <- length(x)

                  ## see .acv2acr() in PeriodicClasses.org
                  acv <- autocovariances(x, maxlag = maxlag, nseasons = nseasons, ...)
                  sd <- sqrt(acv[[0]])
                  fac <- pc_sdfactor(sd, maxlag)[ , 1 + (0:maxlag)] # "1+" since "matrix"
                  res <- acv / fac 
                             # TODO: Should return "SamplePeriodicAutocorrelations or similar?"
                      # 2019-05-14 was:  res # a "Lagged2d" object
                  new("SamplePeriodicAutocorrelations", data = res, n = n, # varnames = varnames,
                                               objectname = objectname)
                  
              }
          }
          )

setMethod("autocovariances", signature(x = "matrix"),
          function(x, maxlag, nseasons, ...){
              if(nrow(x) < ncol(x))
                  stop("Expected 'nrow(x) > ncol(x)', not true")

              if(missing(nseasons)){ # non-periodic
                  callNextMethod(x, maxlag, ...)
              }else{ # periodic, TODO: experimental; needs testing
                  wrk <- t(x)
                  nvar <- nrow(wrk) # = ncol(x), number of variables
                  x <- matrix(wrk, nrow = nvar * nseasons)
                  res <- pc.acf(x, maxlag, ...)
                       # 2017-05-31: it was returning the raw 'res' before
                  new("Lagged2d", data = res)
              }
          }
          )
