permean2intercept <- function(mean, coef, order, nseasons = nrow(coef)){
    n <- max(order - 0:(nseasons-1))

    wrkseas <- rev(rep(nseasons:1, length.out = n + nseasons))
    wrkmean <- mean[wrkseas]

    res <- sapply(1:nseasons,
           function(s)
                  sum( wrkmean[(n+s):(n+s-order[s])] * c(1, -coef[s, seq_len(order[s])]) )
           )
    res
}

## 2016-08-08 New - inverse of permean2intercept()
intercept2permean <- function(intercept, coef, order, nseasons = nrow(coef)){
    mat <- cbind(1, -coef)

    A <- matrix(0, nrow = nseasons, ncol = nseasons)
    i <- 1
    for(t in nseasons:1){
        wrk <- c(numeric(nseasons - t), mat[t, ]) # mult. by B^(d-t)
        A[i, ] <- pc_sum(wrk, nseasons)
        i <- i + 1
    }

    if(length(intercept) != nseasons)
        intercept <- rep(intercept, nseasons)

        # 2018-10-18 was: mu <- solve(A, intercept)
    mu <- solve(A, rev(intercept))

    rev(mu)
}

permodelmf <- function(permodel, update = TRUE){
    ## TODO: currently doesn't work with update = TRUE
    order <-  permodel@ar@order
    co <- permodel@ar@coef  # 2020-04-19 was:  co <- permodel@coef
    co[is.na(co)] <- 0

    ## patch for the case pmax (i.e. max(order)) < nseasons
    ## TODO: this is a job  for the MultiFilter constructor.
    if(ncol(co) < nrow(co))
        co <- cbind(co, matrix(rep(0, (nrow(co) - ncol(co)) * nrow(co)), nrow = nrow(co)))

    mf <- new("MultiFilter", coef = co, order = order)

    if(update){
        permodel@jordan$mf <- mf
        permodel # return the model with multifilter form recorded in it
    }else
        mf       # return only the multi-filter
}

# if res0 is missing returns  r[k,nshifted] + sum
# otherwise returns                    res0 + sum
sumafr <-
function(af,r,k,n,per,res0){
   nshifted <- n+1
   if(missing(res0))
     res0 <- r[k,nshifted]

   res <- res0
   if(n > 1){
     for(j in 1:(n-1)){
       res <- res + af[k,j] * r[toSeason(k-j,per), nshifted-j]
     }
   }
   res
}

## ptildeorders <- function(...){
##     .Deprecated(msg = "Please use 'pdSafeParOrder' (the new name of 'ptildeorders')")
##     pdSafeParOrder(...)
## }

pdSafeParOrder <- function(p) {
    kmax <- which.max(p);
    ptilde <- numeric(length(p))

    pold <- ptilde[kmax] <- p[kmax]
    for(k in rev( shiftleft(1:length(p),kmax) )[-1]){
        pold <- ptilde[k] <- max(p[k],pold-1)
    }
    ptilde
}

#
# smenyam af[k,n,j] s af[k,j], af[k,n-1,j] s afold[k,j];
# smenyam ab[k,n,j] s ab[k,j], ab[k,n-1,j] s abold[k,j];
#
# 30/06/2005 - smenyam po-dolu (k-1)[T] na (k-n)[T} - izglezhda v statiyata e greshno!
alg1 <-
function(r,p){
  maxrlag <- ncol(r);                               # r e matritsa
  period <- nrow(r);   # old:  period  <- length(p);

  if(period != length(p))        # if the length of p is not equal to the period
    p <- rep(p[1],period)        # set all orders to the first element of p

  pkmax   <- max(p);                    #   kmax <- which.max(p);

  ptilde  <- pdSafeParOrder(p);
  fvar <- bvar <- beta <- matrix(nrow = period, ncol = pkmax+1);
  aflst <- ablst <- vector(pkmax,mode="list");

  lag <- 0;
  for(k in 1:period){
    beta[k,lag+1] <- fvar[k,lag+1] <- bvar[k,lag+1] <- r[k,lag+1];
  };

  afold <- abold <- matrix(nrow = period, ncol = 1);     # not used for n=1 below!
  for(n in 1:pkmax ){
    af <- ab <- matrix(nrow = period, ncol = n);

    nshifted <- n+1;       # nshifted is used for vectors originally indexed from zero
    for(k in 1:period){
      rknsum <- sumafr(afold,r,k,n,period)
      if(n <= p[k] && n <= ptilde[k]){

#           beta[k,nshifted] <- ( r[k,nshifted] + sumafr(afold,r,k,n,period,res0=0) ) /
#                                ( sqrt(fvar[k               ,nshifted-1])*
#                                  sqrt(bvar[toSeason(k-1,period),nshifted-1])  );

#           beta[k,nshifted] <- (                 rknsum                   ) /
#                                ( sqrt(fvar[k               ,nshifted-1])*
#                                  sqrt(bvar[toSeason(k-1,period),nshifted-1])  );

        beta[k,nshifted] <- (                 rknsum                   )   /
                              ( sqrt(fvar[k                   ,nshifted-1] *
                                     # bvar[toSeason(k-1,period),nshifted-1]  ) );
                                     bvar[toSeason(k-n,period),nshifted-1]  ) );
      };

      # 29/06/2005 - smenyam tozi if, vizh komentara malko po-dolu
      if(n > p[k] && n <= ptilde[k]){
      # if(n > p[k] ){
        beta[k,nshifted] <- 0;
        r[k,nshifted] <- - sumafr(afold,r,k,n,period,res0=0);
      };
      # 29/06/2005 - smenyam tozi if, vizh komentara malko po-dolu
      if(n <= ptilde[k]){
      # if(TRUE){
        fvar[k,nshifted] <- (1-abs(beta[k,nshifted])^2)*fvar[k               ,nshifted-1];
  # bvar[k,nshifted] <- (1-abs(beta[k,nshifted])^2)*bvar[toSeason(k-1,period),nshifted-1];
        bvar[toSeason(k-n,period),nshifted] <- (1-abs(beta[k,nshifted])^2)*bvar[toSeason(k-n,period),nshifted-1];

        af[k,n] <- - beta[k,nshifted]*sqrt(fvar[k,nshifted-1]) /
                                       # sqrt(bvar[toSeason(k-1,period),nshifted-1]);
                                       sqrt(bvar[toSeason(k-n,period),nshifted-1]);
        # ab[k,n] <- - beta[k,nshifted]*sqrt(bvar[toSeason(k-1,period),nshifted-1]) /
        # ab[k,n] <- - beta[k,nshifted]*sqrt(bvar[toSeason(k-n,period),nshifted-1]) /
        ab[toSeason(k-n,period),n] <-
                     - beta[k,nshifted]*sqrt(bvar[toSeason(k-n,period),nshifted-1]) /
                                       sqrt(fvar[k,nshifted-1]);

#  equivalently:
#        af[k,n] <- - rknsum  / bvar[toSeason(k-1,period),nshifted-1]
#        ab[k,n] <- - rknsum  / fvar[k,nshifted-1]
# but note that if n>p[k] && n<=ptilde[k]
#        the equivalence does not hold!

        if(n>1){
          for(j in 1:(n-1)){
            # af[k,j] <- afold[k,j] + af[k,n]*abold[toSeason(k-1,period),n-j];
            af[k,j] <- afold[k,j] + af[k,n]*abold[toSeason(k-n,period),n-j];
            # 29/06/2005 - the first term on the right of <- should be abold!
            # ab[k,j] <- afold[toSeason(k-1,period),j] + ab[k,n]*afold[k,n-j];
            # ab[k,j] <- abold[toSeason(k-1,period),j] + ab[k,n]*afold[k,n-j];
            # ab[k,j] <- abold[toSeason(k-n,period),j] + ab[k,n]*afold[k,n-j];
            # ab[k,j] <- abold[k,j] + ab[k,n]*afold[toSeason(k+n,period),n-j];
            # ab[k,j] <-
            #        abold[k,j] + ab[toSeason(k-n,period),n]*afold[toSeason(k+n,period),n-j];
            ab[toSeason(k-n,period),j] <-
                     abold[toSeason(k-n,period),j] + ab[toSeason(k-n,period),n]*afold[k,n-j];
          }
        }
      }else{  # n>ptilde[k]   dobavyam tova na 19/03/2007
        beta[k,nshifted] <- 0
        fvar[k,nshifted] <- fvar[k,nshifted-1];
        bvar[toSeason(k-n,period),nshifted] <- bvar[toSeason(k-n,period),nshifted-1];
        af[k,n] <- 0
        ab[toSeason(k-n,period),n] <- 0
        if(n>1){
          af[k,1:(n-1)] <- afold[k,1:(n-1)]
          ab[toSeason(k-n,period),1:(n-1)] <- abold[toSeason(k-n,period),1:(n-1)]
        }
      }
    }
    afold <- aflst[[n]] <- af;
    abold <- ablst[[n]] <- ab;
  }
  res = list(orders = p, be = beta, fv = fvar, bv = bvar,af = aflst, ab=ablst);
  res
}

## this is essentially method for "pc.fcoeffs()" for class "pc.Model.WeaklyStat"
## BUT here the first argument and the result are ordinary matrices.
## (see setMethod("predictionCoefficients", "PeriodicAutocovariance", ...)
calc_predictionCoefficients <- function(m, p, s){
    s <- "af"
    if(length(p) != nrow(m))
        p <- rep(p[1], nrow(m))

    wrk <- alg1(m, p)
    allco <- wrk[[s]]
    res <- matrix(NA, nrow = length(p), ncol = 1 + max(p))  # put 1 at zero index
    for(i in 1:length(p)){
        res[i, 1] <- 1
        if(p[i] > 0){                            # 20/03/2007
            tmp <- allco[[p[i]]][i, ]
            res[i, 1 + (1:length(tmp))] <- tmp
        }
    }
    res
}

calc_backwardPredictionCoefficients <- function(m, p){ # handle properly the case p[i]=0
    s <- "ab"
    ## 2019-05-17 new: adding the 'if', as for the forward coef above.
    if(length(p) != nrow(m))
        p <- rep(p[1], nrow(m))

    wrk <- alg1(m, p)
    allco <- wrk[[s]]
    res <- matrix(NA, nrow = length(p), ncol = 1 + max(p))  # put 1 at zero index
    for(i in 1:length(p)){
        tmp <- allco[[p[i]]][i, ]    # note: backward orders may not be correct!
        res[i,1] <- 1                 #       do it properly!
        res[i,1+(1:length(tmp))] <- tmp
    }
    res
}

calc_pc.fL <- function(m, p, from = 1, to = 6){          # handle propery the case p[i]=0
    s <- "af"
    wrk <- alg1(m, p)
    allco <- wrk[[s]]

    n <- to - from + 1
    nseas <- length(p)

    res <- matrix(0, nrow = n, ncol = n)
    kvec <- rep(1:nseas, 2)[from:(from + nseas - 1)] # valid for small from!

    for(i in 1:nseas){
        k <- kvec[i]     # k-th season
        for(row in seq(from = i, to = n, by = nseas)){
            j <- min(row - 1, p[k])
            res[row, row] <- 1
            if(j > 0){
                tmp <- allco[[j]][k, ]  # get j-th order coeffs for season k
                res[row, (row - length(tmp)):(row - 1)] <- rev(tmp)
            }
        }
    }
    res
}

## the calculation part of pc.bU()
calc_pc.bU <- function(m, p, from = 1, to = 6){# TODO: (old comment) handle propery the
    s <- "ab"                                  #       case p[i]=0
    wrk <- alg1(m, p)
    allco <- wrk[[s]]

    n <- to - from + 1
    nseas <- length(p)

    res <- matrix(0, nrow = n, ncol = n)
    kvec <- rep(1:nseas, 2)[from:(from + nseas - 1)] # valid for small from!

    for(i in 1:nseas){
        k <- kvec[i]     # k-th season
        for(row in seq(from = i, to = n, by = nseas)){
            j <- min(n - row, p[k])
            res[row, row] <- 1
            if(j > 0){
                tmp <- allco[[j]][k, ]  # get j-th order coeffs for season k
                res[row, (row+1):(row+length(tmp))] <- tmp
            }
        }
    }
    res  # res <- slMatrix(res) - rezultatat ne e za slMatrix
}

calc_pc.phis2 <- function(m, p){
    s <- "af"
    s2 <- "fv"
    wrk <- alg1(m, p)
    allco <- wrk[[s]]
    allfvars <- wrk[[s2]]
    res <- matrix(NA, nrow = length(p), ncol = 1 + max(p))  # put 1 at zero index
    innov <- numeric(length(p))
    for(i in 1:length(p)){
        innov[i] <- allfvars[i, p[i] + 1] # ponezhe alg1 vrasta obiknovena matritsa
        res[i, 1] <- 1
        if(p[i] > 0){                            # 20/03/2007
            tmp <- allco[[p[i]]][i, ]
            res[i, 1 + (1:length(tmp))] <- tmp
        }
    }
    list(phi = res, s2 = innov)
}

## TODO: This quietly relies on generic functions autocovariances() and maxLag()
##        (autocovariances() to extract the autocovariances).
##
## TODO: Remove this after testing of the new version further below.
alg1util <-
function(x,s,at0=1){
    m <- as.matrix(autocovariances(x)) # m <- x@acf@m
    pmax <- maxLag(x)
    wrk <- alg1(m, c(pmax,pmax)) # ??? raboti samo za dva sezona?
    res <- wrk[[s]]
    if(!identical(at0,"var"))  # alg1 slaga dispersiite v nuleviya lag
                                        # also "fv" i t.n. Opravi! ???
        res[,1] <- 1           # m[,1]
    res <- slMatrix(res)
    res
}

alg1utilNew <- function(x, s, lag_0 = 1){
    m <- as.matrix(autocovariances(x))
    pmax <- maxLag(x)
    wrk <- alg1(m, c(pmax,pmax)) #??? raboti samo za dva sezona? - Tazi zabelezhka e mnogo
                                 #    stara. zasto ne prosto pmax? alg1 vse edno vzema max(p)
                                 # poyasnenieto e ot 2016-01-10
    res <- wrk[[s]]

    if(isTRUE(lag_0))               # alg1 slaga dispersiite v nuleviya lag
        res[ , 1] <- 1                    # also "fv" i t.n. Opravi! ???
    else if(identical(lag_0, FALSE))
        as.matrix(res)[ , -1]   # drop the zero lag.
    else{
        slMatrix(res)
    }
}

alg1utilNew_new <- function(x, s, lag_0 = TRUE){
    m <- x[]   # as.matrix(autocovariances(x))
    pmax <- maxLag(x)
    wrk <- alg1(m, c(pmax,pmax)) #??? raboti samo za dva sezona? - Tazi zabelezhka e mnogo
                                 #    stara. zasto ne prosto pmax? alg1 vse edno vzema max(p)
                                 # poyasnenieto e ot 2016-01-10
    res <- wrk[[s]]

    if(isTRUE(lag_0)){               # alg1 slaga dispersiite v nuleviya lag
        res[ , 1] <- 1                    # also "fv" i t.n. Opravi! ???
        new("Lagged2d", data = res)
    }else if(isFALSE(lag_0)) # 2019-05-17 was: (identical(lag_0, FALSE))
        as.matrix(res)[ , -1, drop = FALSE]   # drop the zero lag; 2019-05-15 new: drop = FALSE
    else{
        new("Lagged2d", data = res) # slMatrix(res)
    }
}

######################################## auxilliary functions ##########
## 2017-06-05 Note: make 'maxlag' depend on 'order'
pcAR2acf <- function(coef, sigma2, p, maxlag = 10){                        # new 2013-09-23
    phi <- slMatrix(init= cbind(rep(1,nrow(coef)), coef))  # prepend 1's
    period <- nrow(coef)
    if(missing(sigma2)  || length(sigma2) == 0)
        sigma2 <- rep(1, period)
    sigma2 <- PeriodicVector(sigma2) # was:  sVector(sigma2)

    if(missing(p))
        p <- ncol(coef)

    if(missing(maxlag))
        maxlag <- ncol(coef)

    dummytheta <- slMatrix( init=matrix(c(rep(1,period), rep(0, 2*period)), nrow=period) )

    ## todo: check if pcarma_acvf_lazy works with p of unequal length

    phi@m[is.na(phi@m)] <- 0

    f.acf <- pcarma_acvf_lazy(phi, dummytheta, sigma2, p=p, 0, period=period, maxlag=maxlag)
    f.acf(1:period, 0:maxlag)
}




#### this seems tied to the old model classes

# temporary - stava za template za "PeriodicAutocovariance"
pc.acf.parModel <- function(parmodel, maxlag = NULL){             # 2013-10-24 new

    ## 2016-01-31 krapka: adding the following 2 lines and ignoring the body of the function.
    res <- pcAR2acf(filterCoef(parmodel@ar), # was: arCoef(parmodel),
                    sigmaSq(parmodel),
                    filterOrder(parmodel@ar), # was: arOrder(parmodel),
                        # bugfix 2018-10-21 was: maxlag = 10    ## TODO: arbitrary constant 10 here!
                    maxlag = if(is.null(maxlag)) 10 else maxlag)

    return(slMatrix(res))

    ## !!! 2016-08-06 note the above return() statement.
    ##            The rest was possibly intended to avoid calling pcarma_acvf_lazy
    wrk <- autocovariances(parmodel@acf)

    if(is.null(maxlag))
        return(wrk)

    curmaxlag <- maxLag(wrk)
    if(maxlag <= curmaxlag)
        res <- slMatrix(wrk[ , 0:maxlag])
    else{
        order <- filterOrder(parmodel@ar) # was: arOrder(parmodel) # parmodel@order
        coef <- filterCoef(parmodel@ar) # arCoef(parmodel)    # parmodel@coef
        nseas <- nSeasons(parmodel) # parmodel@nseasons

            # res <- cbind(wrk@m, matrix(0, nrow = nseas, ncol = maxlag - curmaxlag))
        res <- cbind(as.matrix(wrk), matrix(0, nrow = nseas, ncol = maxlag - curmaxlag))
        res <- slMatrix(res) # lazy
        for(k in (curmaxlag+1):maxlag){
            for(s in 1:nseas){
                if(order[s] > 0){
                    su <- 0
                    for(i in 1:order[s])
                        su <- su + coef[s,i] * res[s-i,k-i, type="tl"] # lazy
                    res[s,k] <- su
                }

            }
        }
    }
    res
}



######################################## end auxilliary functions ##########

######################################## auxilliary functions ##########
## modification of pcAR2acf() for pcArma
pcArma2acf <- function(arcoef, sigma2, p, macoef, q, maxlag=10){            # new 2016-08-06
    period <- max(nrow(arcoef), nrow(macoef))  ## TODO: need more care here

    if(missing(sigma2)  || length(sigma2) == 0)
        sigma2 <- rep(1, period)
    sigma2 <- PeriodicVector(sigma2) # was:  sVector(sigma2)

    if(missing(p))
        p <- ncol(arcoef)

    if(missing(q))
        q <- ncol(macoef)

    if(missing(maxlag))
        maxlag <- max(ncol(arcoef), ncol(macoef))

    ## prepend 1's to the coefficient matrices
    phi <- slMatrix(init = cbind(rep(1, nrow(arcoef)), arcoef))
    theta <- slMatrix(init = cbind(rep(1, nrow(macoef)), macoef))

    ## replace NA coefficients with 0's
    phi@m[is.na(phi@m)] <- 0
    theta@m[is.na(theta@m)] <- 0


    ## krapka
    if(length(p) == 0)
        p <- 0
    if(length(q) == 0)
        q <- 0


    ## todo: check if pcarma_acvf_lazy works with p of unequal length
    f.acf <- pcarma_acvf_lazy(phi, theta, sigma2, p=p, q=q, period=period, maxlag=maxlag)
    f.acf(1:period, 0:maxlag)
}
