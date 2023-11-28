## setClass("Fitted",
##          slots = c(n = "numeric", varnames = "character", objectname = "character" )
##          )

setMethod("show", "Fitted",
    function (object){
        .reportClassName(object, "Fitted")
        callNextMethod()
    })

## This chunk is old, before October 2016, was file perclsfit.R
## 2015-02-11 TODO: asymptotic s.e. of means/intercepts?
##
## 2019-05-23 new slot "ns" (a vector of length nSeasons() with the number of obs used in
##            estimation for each season.
FittedPeriodicArModel <-
    setClass("FittedPeriodicArModel",
                 # asyCov is a matrix but maybe from package Matrix
             slots = c(theTS = "PeriodicTS", asyCov = "ANY", ns = "numeric"), 
             contains = c("PeriodicArModel")
             )

## TODO: introduce more general super class "FittedModel"?  It makes more sense to have a
##       small package where to define time series classes in the spirit of the periodic
##       classes here and include a few things like "FittedModel". Then also move the
##       generics for "autocovariances" and "autocorrelations" there.

setMethod("show", "FittedPeriodicArModel",
          function(object){
              cat("An object of class FittedPeriodicArModel", "\n")
              callNextMethod()
              cat("\n")
              cat("number of obs. for each season:\n",
                  "    ", object@ns, "\n")

              ## TODO: add diagnostics
              
              # cat("\n")
              # cat("The asymptotic covariance matrix: is in slot 'asyCov'.\n")
          })

setMethod("summary", c(object = "FittedPeriodicArModel"),
          function(object, ...){
              seas_ind <- allSeasons(object, abb = TRUE)  # was: 1:nrow(ar)
              ## TODO: is there a method to extract this?
              ##       it should take account also for @intercept.
              means <- object@center
              names(means) <- paste0("mu_", seas_ind) # was: 1:length(means)
              ## TODO: currently always keep all means
              ##       but this allows flexibility
              keep_mean <- rep(TRUE, length(means))

              ar <- object@ar@coef
              names_ar <- outer(seas_ind, 1:ncol(ar), 
                                function(x, y) paste0("ar_", x, "_", y))
              ar_vec <- as.vector(t(ar))
              names(ar_vec) <- as.vector(t(names_ar))
              ## TODO: need more careful procedure for dropping NA's;
              ##       use the order of the model to do this!
              keep_par <- !is.na(ar_vec)

              keep <- c(keep_mean, keep_par)

              ## create table similar to summary.lm()
              est  <- c(means, ar_vec)[keep]
              se   <- sqrt(diag(object@asyCov)[keep])
              zval <- est / se
              pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)

              co <- cbind(Estimate = est, `Std. Error` = se, 
                          `z value` = zval, `Pr(>|z|)` = pval)

              ts <- theTS(object)
              resid <- residuals(object)
              maxlag <- floor(10 * (log10(length(ts))))
              resid_acf <- autocorrelations(resid, maxlag = maxlag)
              use = unique( c(pmin(maxlag, c(max(object@ar@order) + 1, 10, 20))) )
              pwn_test <- pwn_McLeodLjungBox_test(resid_acf, nepoch = length(ts), 
                  use = use, fitdf = object@ar@order)
              colnames(pwn_test$pval) <- paste0("Q(", pwn_test$hlags, ")")
              rownames(pwn_test$pval) <- seas_ind # paste0("Season_", 1:nSeasons(ts))

              ## BIC McLeod, p.225, eqs. (2.3)-(2.4)
              ns <- object@ns
              aic <- 
              bic <- - (ns /2) * log(object@sigma2)
              aic <- aic + 2 * object@ar@order
              bic <- bic + log(ns) * object@ar@order   

              ## for now, start by printing the object:
              show(object)
              cat("\n")
              ## cat("An object of class FittedPeriodicArModel", 
              ##     paste0("(", paste0(object@ar@order, collapse = ","), ")"),
              ##     "\n", sep = "")
            
              printCoefmat(co)

              cat("\n")
              cat("McLeod_LBtest (p-values):\n")
              print(pwn_test$pval)

              cat("\n")
              cat("Information criteria: ")
              cat("AIC = ", sum(aic), ",  BIC = ", sum(bic), sep = "")
              cat("\n")
              invisible()
          })

## slot "ns" is a vector of length nSeasons() with the number of obs used in
##            estimation for each season.
FittedPM <-
    setClass("FittedPM",
                 # asyCov is a matrix but maybe from package Matrix
             slots = c(theTS = "PeriodicTS", asyCov = "ANY", ns = "numeric")
             )

.whiten <- function (object, ...){
    model <- as_pcarma_list(object)

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

.predict <- function (object, n.ahead = 1L, ...){
    model <- as_pcarma_list(object)

    ts <- theTS(object)
    x <- as.numeric(ts)
    ## TODO: passing eps = NULL since it is not a default in pc.filter.
    ##       check if I have done this on purpose!
    eps <- pc.filter(model = model, x = x, eps = NULL, whiten = TRUE)
                # TODO: omit the first few values?
                #       or, optionally set them to the exact residuals
                #       or, optionally, standardise?
                #       or, optionally, return a (periodic) time series?
     
    wrk <- pc.filter(model = model, x = c(x, rep(0, n.ahead)), 
                                    eps = c(eps, rep(0, n.ahead)), whiten = TRUE)

    ## TODO: make a time series from the predictions
    ##       (the last h.agead elements of wrk)
    ##    Done below but this needs documented and maybe helper functions, too. 
    new_start <- end(ts)
    if(new_start[2] == nSeasons(ts))
        new_start <- c(new_start[1] + 1, 1)
    else
        new_start[2] <- new_start[2] + 1

    ts@pcstart <- new_start
    ts@.Data <- wrk[-(1:length(x))]

    theTS(object) <- ts
    ts
}

residuals.FittedPM <- function (object, ...){
    .whiten(object, ...)
}

## TODO: maybe calculate directly?
fitted.FittedPM <- function (object, ...){
    theTS(object) - residuals(object)
}

## se.fit for now ignored
predict.FittedPM <- function(object, n.ahead = 1, se.fit = TRUE, ...){
    .predict(object, n.ahead, ...)
}

## FittedPeriodicArmaModel <-
setClass("FittedPeriodicArmaModel",
    ## asyCov is a matrix but maybe from package Matrix
    ## slots = c(theTS = "PeriodicTS", asyCov = "ANY", ns = "numeric"), 
    contains = c("PeriodicArmaModel", "FittedPM")
)

setMethod("show", "FittedPeriodicArmaModel",
          function(object){
              cat("An object of class FittedPeriodicArmaModel", "\n")
              callNextMethod()
              cat("\n")
              cat("number of obs. for each season:\n",
                  "    ", object@ns, "\n")

              ## TODO: add diagnostics
              
              ## cat("\n")
              ## cat("The asymptotic covariance matrix: is in slot 'asyCov'.\n")
          })
