periodic_acf1_test <-        # McLeod Q1 statistic, see the theory in ...
  function(acf, nepochs){
    stat <- max(nepochs) * sum(acf[,1]^2)    # note: in pear there are two related max-nepochs
                                             #   nyrs.max and max(nyrs). They may differ in
                                             #   some circumstances. nyrs.max >= max(nyrs)
                                             #   McLeod uses nyrs.max. This may be related to
                                             #   the way he computes acf.
    pvalue <- 1 - pchisq(stat, nSeasons(acf))
    list(statistic = stat, pvalue = pvalue)
  }

                                       # 2016-01-27 renamed the function; changed the order of
                                       #    the arguments, was: (lag, season, nepoch, period);
pcacf_pwn_var <- function(nepoch, period, lag, season = 1:period){
    nr <- length(season)                    # In this form the result is for n full years.
    nc <- length(lag)                       # TODO: modify for incomplete years?
    if(nr*nc>1){
        lag    <-  matrix(lag,    nrow = nr, ncol = nc, byrow = TRUE)
        season <-  matrix(season, nrow = nr, ncol = nc) # For season and nepoch the recycling
        nepoch <-  matrix(nepoch, nrow = nr, ncol = nc) # rule should work but this is clearer
    }
                                                 # McLeod 1994, p.229, eq.(4.3)
    res <- ifelse((lag %% period) == 0,
                  (nepoch - lag/period)/(nepoch * (nepoch + 2)),
                  (nepoch - trunc((lag - season + period) / period)) / nepoch^2 )
    res
}

                            # acf[ , k] should give the lag k acf coefs
                            # So, acf can be an "acf"-type object with the lag 0 coefs in the
                            # first column, if it handles indexing properly.  If acf is an
                            # ordinary matrix or similar, it should not have zero lag column.

# updated 2014-01-07; 2013-12-31; renamed on 2016-01-27
pwn_McLeodLjungBox_test <- function(acf, nepoch, use = 1:maxlag,
                                    maxlag = ncol(as.matrix(acf)) - 1,  # '-1' in case acf is
                                                                        # an ordinary matrix
                                    period = nrow(as.matrix(acf)),
                                    fitdf = numeric(period) ){
    ## McLeod 1994, p.229, eq.(4.5)
    ## 2022-02-26 Note: the formula eq. (4.5) has a typo - denominator shouldn't have sqrt()!
    ##    This was noted in:a correction to the above paper later TODO: cite!
    ##    (spotted by Yueyun)
    wrk <- acf[ , 1:maxlag]^2 / pcacf_pwn_var(nepoch, period, 1:maxlag, 1:period)
    stat <- apply(wrk, 1, cumsum)             # assumes result is (period x maxlag) matrix

                                         # for testing purposes, i think should never trigger.
                                         # !!!!! what if maxlag=1 ?
    if(!is.matrix(stat) || ncol(stat) != period || nrow(stat) != maxlag)
        stop(" in pwn_McLeodLjungBox_test result of apply is not a matrix of appropriate dimension.")

    stat <- t(stat)
    ## 2019-05-24 was: df <- col(stat)
    ##                 df <- df - fitdf
    all_hlags <- col(stat)
    df <- all_hlags - fitdf # correct for residuals of fitted model.
                            # Note: this uses recycling rule to subtract fitdf from each column.

    df[df <= 0] <- NA # give NA for negative df's

    pval <-  mapply(function(x, d) pchisq(x, df = d, lower.tail = FALSE),
                    stat, df)
    pval <- matrix(pval, nrow = period)

    list(statistic = stat[ , use, drop = FALSE],
         df        = df[   , use, drop = FALSE],
         pval      = pval[ , use, drop = FALSE],
         hlags     = use # 2019-05-24 - new component
         )
}


setMethod("pcTest", signature(x = "ANY", nullmodel = "character"),
          function(x, nullmodel, nseasons, ...){
              switch(nullmodel,
                     "wn" = Box.test(x, ...),
                     "piar" = test_piar(x, nseasons, ...),
                     # default
                       # do.call(nullmodel, list(x, ...), quote = TRUE)
                       # todo: think about this?
                     do.call(nullmodel, list(quote(x), nseasons, ...))
                     )
          }
          )


setMethod("pcTest", signature(x = "numeric", nullmodel = "character"),
          function(x, nullmodel, nseasons, ..., maxlag){
              switch(nullmodel,
                     "wn" = Box.test(x, ...),
                     "piar" = test_piar(x, nseasons, ...),
                         "pwn" = {
                              acr <- autocorrelations(x, nseasons = nseasons, maxlag = maxlag)
                              acrsl <- slMatrix(as.matrix(acr))
                              pcTest(acrsl, nullmodel, nepoch = floor(length(x)/nseasons), ...)
                         },
                     # default
                       # do.call(nullmodel, list(x, ...), quote = TRUE)
                       # todo: think about this?
                     do.call(nullmodel, list(quote(x), nseasons, ...))
                     )
          }
          )


setMethod("pcTest", signature(x = "PeriodicTimeSeries", nullmodel = "character"),
          function(x, nullmodel, ..., maxlag){ # TODO: arg. for a specific column in multivar case?
              nseas <- nSeasons(x)
              if(nVariables(x) == 1){
                  ## 2019-04-26 was:  pcTest(coreVector(x), nullmodel, nseas, ...)
                  switch(nullmodel,
                         "pwn" = {
                              acr <- autocorrelations(x, maxlag)
                              acrsl <- slMatrix(as.matrix(acr))
                              pcTest(acrsl, nullmodel, nepoch = nCycles(x), ...)
                         },
                         ## default
                         pcTest(as(x, "vector"), nullmodel, nseas, ...)
                         )
              }else{ # for now just do the test for each variable separately
                      # 2019-04-26 was:  m <- coreMatrix(x)
                      # 2020-03-15 commented out: m <- as(x, "matrix")
                  res <- vector(nVariables(x), mode = "list")
                  names(res) <- colnames(x)
                  ## TODO: names of variables
                  for(i in seq(along = res))
                         # 2020-03-15 was: 
                         #   res[[i]] <- pcTest(m[ , i], nullmodel, nseas, maxlag = maxlag, ...)
                      res[[i]] <- pcTest(x[[i]], nullmodel, maxlag = maxlag, ...)
                  res
              }
          }
          )


setMethod("pcTest", signature(x = "slMatrix", nullmodel = "character"),
          function(x, nullmodel, ...){
              switch(nullmodel,
                     "pwn" = pwn_McLeodLjungBox_test(x, ...),
                     "periodicity" = periodic_acf1_test(x, ...),
                     ## default
                     do.call(nullmodel, list(x, ...))
                     )
          }
          )
