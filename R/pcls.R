pclsdf <- function(x, d, lags = integer(0), sintercept = TRUE, sslope = FALSE,
                   intercept = FALSE, slope = FALSE,
                   xreg, contrasts = NULL, seasonof1st = NULL, coefonly = FALSE
                   ){             # unconstrained optim. here, so no need for seasonal weights
    if(is.null(seasonof1st))    # check also x, it may contain this infor, e.g. if of class ts
        seasonof1st <- 1

    if(sintercept && intercept)       ## 2015-07-31 to force identity contrasts
        intercept <- FALSE            ##    need additional work (:TODO:) to support other

    if(sslope && slope)               ## ditto
        slope <- FALSE

    seasyear <- c(1:d,1:d)[1:d + (seasonof1st - 1)]
                         # 2015-07-31 seas <- rep(as.factor(seasyear), length.out = length(x))
    seas <- rep(seasyear, length.out = length(x))
    seas <- as.factor(seas)
    if(!is.null(contrasts))
        seas <- C(seas, contrasts)

    TimeIndex <- 1:length(x)

    res <- data.frame(Season = seas, TimeIndex = TimeIndex)


    fochar <- paste("x ~", if(intercept) "+1" else "-1"                  # intercept
                         , if(sintercept) "+ Season" else "- Season"     # seasonal intercept
                         , if(slope)  "+ TimeIndex"  else ""
                         , if(sslope) "+ TimeIndex:Season" else ""
                  , sep = " ")

    if(length(lags)>0){
        lagged <- sapply(lags, function(lag)  c(rep(NA,lag), x[1:(length(x)-lag)] ) )
        colnames(lagged) <- paste("Lagged_", lags, sep="")

        res <- cbind(res, lagged)

        fochar <- paste(c(fochar, paste0(colnames(lagged),":Season")), collapse = " + ")
    }

    fomodel <- as.formula(fochar)


    # fit <- lm(fomodel, data = res, contrasts = contrasts, na.action = na.exclude)
    fit <- lm(fomodel, data = res, na.action = na.exclude)
    fit.resid <- residuals(fit)

                                # 2015-07-29 (changed to deal with lengths non-multiple of d)
                                # was: apply(matrix(fit.resid, nrow = d), 1,
                                #                   function(x) mean(x^2, na.rm=TRUE))
    sigma2hat <- sapply(1:d, function(s){
                                 ind <- seq(from = s, by = d, to = length(fit.resid))
                                 mean(fit.resid[ind]^2, na.rm=TRUE)
                             }
                       )
    if(seasonof1st > 1)
        sigma2hat <- sigma2hat[order(seasyear)]  # TODO: check!


    co <- coef(fit)
    nams <- names(co)

    if(length(lags) > 0){
        ind <- t(sapply(lags, function(x) grep(paste("Lagged_", x, sep=""), nams) ))
        par <- apply(ind, 1, function(x) co[x])
    }else{
        par <- matrix(0, nrow = d, ncol = 0)
    }

    res <- list(par = par)

    if(sintercept){
        snames <- grep("^Season[0-9]+$", nams)  # TODO: is this reliable?
        res$sintercept <- co[snames]
    }

    if(sslope){
        snames <- grep("^Season[0-9]+:TimeIndex$", nams)  # TODO: is this reliable?
        ## In version R-3.6.x and maybe others the names of the variables were as below. From
        ##     R-devel and Rpatched circa start of Feb 2020 the names changed to the above. I
        ##     think that they must have been as above, since the above code was written
        ##     years ago and it surely didn't drop sslope back then.
        if(length(snames) == 0)
            snames <- grep("^TimeIndex:Season[0-9]+$", nams)  # TODO: is this reliable?
        res$sslope <- co[snames]
    }

    if(intercept){
        snames <- grep("^(Intercept)$", nams)  # TODO: is this reliable?
        res$intercept <- co[snames]
    }

    if(slope){
        snames <- grep("^TimeIndex$", nams)  # TODO: is this reliable?
        res$slope <- co[snames]
    }

    res$sigma2hat <- sigma2hat

    res$formula.char <- fochar

    if(!coefonly)
        res$fit <- fit

    res
}

pclspiar <- function(x, d, p, icoef = NULL, parcoef = NULL,
                     sintercept = FALSE,
                     seasonof1st = 1,
                     weights = TRUE,
                     itol = 1e-7, maxniter = 1000){

    if(length(p) != 1)
        stop("currently the length of 'p' must be equal to one.")
    if(p < 1)
        stop("p must be greater than or equal to one.")

    iflag <- is.null(icoef)
    pcflag <- is.null(parcoef)

    if(iflag && pcflag){          # icoef and parcoef - both missing
            # 2019-06-02 was: icoef = rep(1, d)
        r0sq <- sapply(1:d, function(i) mean(x[seq(i, length(x), by = d)]^2, na.rm = TRUE) )
        icoef <- r0sq / c(r0sq[d], r0sq[-d])

        ## print(r0sq)
        ## print(icoef)
    }

    if(is.matrix(icoef)){        # 2014-06-20 new conditional.
        if(ncol(icoef) == 1)
            icoef <- as.numeric(icoef)
        else                                                   # TODO: handle higher order PI?
            stop("Currently pclspiar handles only first order periodic integration.")
    }

    param.names <- paste("alpha", 1:d, sep="")
    names(icoef) <- param.names  # could use names(icoef), if available but there is a risk
                                 # for puzzling errors if icoef happens to have names that
                                 # are not appropriate (e.g. repeated or the empty string)

    param.last.expr <- paste(param.names[-d], collapse = " * ")
    param.last.expr <- paste("1/(", param.last.expr, ")", sep = "")


    n <- length(x)
    eps <- numeric(n)


                       # season.pplusone <- (p+1) %% d    # TODO: this assumes seasonof1st = 1
                       # if(season.pplusone == 0)
                       #    season.pplusone <- d
    seasyear <- c(1:d,1:d)[1:d + (seasonof1st - 1)]
    season.pplusone <- rep(seasyear, len = p+1)[p+1]

          # NOTE: pcsubmodel2 sintercept and similar are only in pcsubmodel, not pcsubmodel2 !
    pcsubmodel  <- list(phi   = parcoef, p = p-1, period = d)
    if(sintercept)
        pcsubmodel$intercept = sintercept

    pcsubmodel2 <- list(theta = parcoef, q = p, period = d) # note: order is p, not p-1 !

    niter <- 0
    threshold.store <- numeric(0)

    # for testing
    #
    # loglik.fit <- numeric(0)
    # alpha.all <- matrix(NA, nrow = 0, ncol = d)
    # colnames(alpha.all) <- param.names

    icoef.old <- Inf
    flag.break <- FALSE
    repeat{  # until convergence
        niter <- niter + 1

        icoef.old <- icoef

        imodel <- list(phi = matrix(icoef, ncol = 1), p = 1, period = d)

        ## filter x with icoef
        x.ifiltered <- pc.filter(x, eps, model = imodel, whiten = TRUE,
                                 seasonof1st = seasonof1st) # from = 2
        x.ifiltered[1] <- NA

        ## estimate parcoef                               # TODO: take care of the season !!!

        wrk <- pclsdf(x.ifiltered, d, lags = seq_len(p-1),
                     sintercept = sintercept, sslope = FALSE,
                     seasonof1st = seasonof1st, coefonly = TRUE)

        pcsubmodel$phi <- wrk$par
        if(sintercept)
            pcsubmodel$intercept <- wrk$sintercept

        pcsubmodel$sigma2hat <- wrk$sigma2hat

        if(flag.break)
            break

        ## prepare the response vector and the design data frame
        ylhs <- pc.filter(x, eps, model = pcsubmodel, whiten = TRUE,
                          from = p + 1, seasonof1st = seasonof1st )

        yrhs <- matrix(NA, ncol = d, nrow = length(x))
        for(i in 1:d){
            wrkphi <- pcsubmodel$phi

            if(p > 1){
                ind <- ( (1:(p-1)) %% d) != i - 1
                wrkphi[ , ind] <- 0
                wrkphi <- - wrkphi     # 2014-02-02
            }

            wrkphi <- cbind(if(i == 1) 1  else 0,
                            wrkphi)

            pcsubmodel2$theta <- wrkphi

            wrk2 <- pc.filter(numeric(n), x, model = pcsubmodel2, whiten = FALSE,
                              from = p + 1, seasonof1st = seasonof1st)

            wrk <- wrk2
            wrk[(p+1):n] <- wrk[(p+1):n] - x[(p+1):n]

            yrhs[ , i] <- wrk
        }

        ## rotate each row, so that the coef. are for alpha_d, ..., alpha_1 (in this order)
        y.rot <- yrhs

        ind <- 1:d
        seas.int <- 1:n + (seasonof1st - 1)
        for(ss in c(0, (d-1):1)){
            rows <- seas.int %% d == ss             # 08/02/2014   (1:n) %% d == ss
            y.rot[ rows, 1:d  ] <- yrhs[rows, ind]
            ind <- c(ind[d], ind[-d])
        }
        # y.rot  now contains the design matrix.

        data <- as.data.frame(y.rot)
        colnames(data) <- paste("Instr", d:1, sep="")

        mo.char <- paste("ylhs ~ 0 ", ###  "ylhs ~ nlsintercept ",
                           paste(rev(param.names)[-1], " * ", colnames(data)[-1],
                                 sep="", collapse=" + "),
                           paste(param.last.expr, " * ", colnames(data)[1], sep=""),
                           sep = " + " )

        mo.fo <- as.formula(mo.char)

          # pcweights <- rep((mean(pcsubmodel$sigma2hat)/pcsubmodel$sigma2hat) / length(ylhs),
          #                  len = length(ylhs))

        s2rotated <- pcsubmodel$sigma2hat[seasyear]
        pcweights <- rep(mean(s2rotated)/s2rotated / n, len = n)

        data <- data[-(1:p), ] # drop the first p rows
        ylhs <- ylhs[-(1:p)]
        pcweights <- pcweights[-(1:p)]

        fit <- if(weights)
            nls(mo.fo, data = data, start = c(icoef[1:(d-1)]),
                weights = pcweights, control=list(maxiter=1000) )
        else
            nls( mo.fo, data = data, start = c(icoef[1:(d-1)]),
                control=list(maxiter=1000) )

        icoef[1:(d-1)] <- coef(fit)
        icoef[d] <- 1/prod(icoef[1:(d-1)])   # not forgetting the last coefficient!
        # names(icoef)[d] <- param.names[d]

        # loglik.fit <- c(loglik.fit, logLik(fit))
        # alpha.all <- rbind(alpha.all, icoef)

        # print(icoef)
        # print(pcsubmodel$phi)
        # print(logLik(fit))
        # cat("\n=================\n\n")

                                              # TODO: do proper checks for convergence the
                                              # ones below are for the time being and check
                                              # only the parameters estimated by nls
        if(niter > 2 ){
            threshold <- sum(abs( (icoef[-d] - icoef.old[-d]) / icoef.old[-d] ))
            threshold.store <- c(threshold.store, threshold)
        }
        if(niter > 2  && threshold < itol)
            flag.break <- TRUE

        if(niter > maxniter)
            flag.break <- TRUE
    }

# browser()

    list(icoef = icoef, parcoef = pcsubmodel$phi
         , sintercept = pcsubmodel$intercept
         , sigma2hat = pcsubmodel$sigma2hat
         )

}

pi1ar2par <- function(picoef, parcoef){
    d <- length(picoef)
    p <- ncol(parcoef) + 1

    names(picoef) <- NULL

    pirev <- rep(rev(picoef), len = d+p)

    piwrk <- sapply(1:d, function(x) pirev[seq(d+1-x, length.out = p)])
    piwrk <- t(piwrk)

    cbind(parcoef, 0) - piwrk * cbind(-1, parcoef)
}

piar2par <- function(picoef, parcoef){                                        # 2014-06-20 new
    if(is.numeric(picoef))
        return(pi1ar2par(picoef, parcoef))
    ##else expect matrix picoef

    i <- ncol(picoef)     # TODO: test!
    while(i > 0){
        parcoef <- pi1ar2par(picoef[ , i], parcoef)
        i <- i - 1
    }
    parcoef
}

                                                            # 2014-04-08 renaming to test_piar
                                                            # was: piartest_franses96
test_piar <- function(x, d, p, sintercept = FALSE, sslope = FALSE, homoschedastic = FALSE ){
                                                                     # todo: seasonof1st, etc.
                                        # TODO: add argument sslope to pclspiar !!!
                   # fit0 <- pclspiar(x, d, p,  sintercept = sintercept, sslope = sslope)
    fit0 <- pclspiar(x, d, p,  sintercept = sintercept)
    fit1 <- pclsdf(x, d, lags = seq_len(p), sintercept = sintercept, sslope = sslope)

    n <- length(x)

    # if(homoschedastic){   # TODO: in principle, this should also do non-weighted regression.
    #     logss0 <- log( sum(fit0$sigma2hat) * n/d )  # todo: could be done more precisely.
    #     logss1 <- log( sum(fit1$sigma2hat) * n/d )
    # }else{
    #     logss0 <- sum( log(fit0$sigma2hat) ) * n/d
    #     logss1 <- sum( log(fit1$sigma2hat) ) * n/d
    # }
    # LR <- n * (logss0 - logss1)

    if(homoschedastic){   # TODO: in principle, this should also do non-weighted regression.
        LR <- ( log(sum(fit0$sigma2hat)/d) - log(sum(fit1$sigma2hat)/d) ) * n
    }else{
        LR <- ( sum(log(fit0$sigma2hat)) - sum(log(fit1$sigma2hat)) ) * n / d
    }



    if(p == 1){               # book Franses (1996), p. 130
        g.of.alpha.hat <- prod(fit1$par)
        g.of.alpha.hat.grad <- sapply(1:d, function(x) prod(fit1$par[-x, 1]))
    }else{
        g.of.alpha.hat <- NA  # TODO: vzh ideyata za Lina !!!
        g.of.alpha.hat.grad <- NULL
    }

    LRtau <- sign(g.of.alpha.hat - 1) * sqrt(LR)


    V <- vcov(fit1$fit)
    alpha.ind <- grepl("Lagged_1", rownames(V))
    V <- V[ alpha.ind, alpha.ind ]

                        # NOTE the division below, Franses (1976), p.130, eq. 8.23 has a typo
    se.g.of.alpha.hat <-
        if(!is.null(g.of.alpha.hat.grad))
            sqrt( as.vector( g.of.alpha.hat.grad %*% V %*% g.of.alpha.hat.grad ) )
        else
            NA

    tau <- (g.of.alpha.hat - 1) / se.g.of.alpha.hat

    perFuller <- (g.of.alpha.hat - 1) * n /d

    ## compute pvalue: fUnitRootTests::padf(perFuller)                # (asymptotic)
    ##         or      fUnitRootTests::padf(perFuller, N = floor(n/d) # finite number of years


    ## tau, LRtau
    ##
    ## Fuller (1976), Table 8.5.2; Franses and Paap (2004), Table A.1, p. 125
    ## Fuller (1996), Table 10.A.2 (minor differences from the above)
    ##     adfTable("nc")
    ##     adfTable("c")
    ##     adfTable("ct")

    ## rho
    ##
    ## Fuller (1976), Table 8.5.1(?)
    ## Fuller (1996), Table 10.A.1 (minor differences from the above)
    ##     adfTable("nc", statistic = "n")
    ##     adfTable("c", statistic = "n")
    ##     adfTable("ct", statistic = "n")

    ## pvalues; using asymptotic ones since currently there is a warning about deprecated
    ##          akima::interpp.old()
    tr <- if(sslope)
              "ct"
          else if(sintercept)
              "c"
          else
              "nc"

    pvalues <- c(LR = NA_real_,
                 LRtau = fUnitRoots::padf(LRtau,     trend = tr, statistic = "t"),
                 tau   = fUnitRoots::padf(tau,       trend = tr, statistic = "t"),
                     # 2019-06-02 was LRtau = ...
                 perFuller = fUnitRoots::padf(perFuller, trend = tr, statistic = "n") )
                     # 2019-06-02 commenting out, this patch not needed after the above change:
                     #    names(pvalues) <- c("LR", "LRtau", "tau", "perFilter")

                                 # 2014-04-08 add spec to the return value;
                                 #     also, combined the stats in one vector
    list(p = p,    # TODO: rename p => order, perFuller => rho ?
         spec = c(sintercept = sintercept, sslope = sslope, homoschedastic = homoschedastic),
         statistics = rbind(stats = c(LR = LR, LRtau = LRtau, tau = tau, perFuller = perFuller),
                            pvalues = pvalues)
         )
}
