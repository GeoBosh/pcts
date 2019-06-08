pc.filter.xarma <- function(x, eps, phi, theta, period, p, q, n, from, seasonof1st = 1,
                            intercept = NULL,  nintercept = NULL){

    if(length(p) == 1) p <- rep(p, period)
    if(length(q) == 1) q <- rep(q, period)

    ceps <- eps
    if(!is.null(intercept))
        ceps <- ceps + rep( shiftleft(intercept, seasonof1st - 1), length.out = n )
    if(!is.null(nintercept))
        ceps <- ceps + nintercept

    nextseason <- c(seq_len(period)[-1], 1L)   # like c(2:period, 1) but works for period=1

    ## if season(t0) is s, for which t season(t)=1?
    ## obviously(?),  season(t0 - (s-1))=1.
    ## Setting t0 = 1 and  s = seasonof1st gives t = 1 - (seasonof1st - 1)
    ##
    ## season of t = from-1
    k <- toSeason(from - 1, period, 1 - (seasonof1st - 1))

    for(t in from:n){
        k <- nextseason[k]
        wrk <- ceps[t]

        for(i in seq_len(p[k]) )
            wrk <- wrk + phi[k,i] * x[t-i]

        for(i in seq_len(q[k]) )
            wrk <- wrk + theta[k,i] * eps[t-i]

        x[t] <- wrk
    }
    x
}

           # set model to be the first argument (done), potentially make the function generic?
pc.filter <- function(model, x, eps, seasonof1st = 1, from = NA, whiten = FALSE, nmean = NULL,
                      nintercept = NULL){
                         # 2014-02-01 changing to "[[" to avoid partial matching for "p", etc.
                         #            which was a long standing cause of puzzling errors.
    phi       <- model[["phi",       exact = TRUE]] # model$phi
    theta     <- model[["theta",     exact = TRUE]] # model$theta
    p         <- model[["p",         exact = TRUE]] # model$p
    q         <- model[["q",         exact = TRUE]] # model$q
    period    <- model[["period",    exact = TRUE]] # model$period
    mean      <- model[["mean",      exact = TRUE]] # model$mean
    intercept <- model[["intercept", exact = TRUE]] # model$intercept

    if(is.null(p)) p <- 0
    if(is.null(q)) q <- 0

    if(is.na(from)){                            # choose a value for "from" if not supplied
        from <- 1 + max(p,q)
        if(length(p) > 1 || length(q) > 1){         # backtrack to see if it is possible
            ma <- pmax(p, q)                        # to choose "from" smaller than 1+max(p,q)
            for(i in from:1){
                ik <- toSeason(seasonof1st - 1 + i, period)  # season of i with seasonof1st
                                                             # is as that of seasonof1st-1 + i
                                                             #       but with seasonof1st=1.
                if( i > ma[ik] )
                    from <- i
                else                    # abandon search if i-p_i or i-q_i is smaller than 1.
                    break
            }
        }
    }

    n <- length(x)

    d <- is.null(mean) + 2 * is.null(nmean)
    if(d == 3){        # both NULL
        flag.mean <- FALSE
        y <- x
    }else{
        flag.mean <- TRUE
        mu <- if(d == 2)
                  rep(shiftleft(mean, seasonof1st - 1), length.out = n)#mean given, nmean=NULL
              else if(d==1)
                  nmean                                         # nmean given, mean=NULL
              else nmean + rep( shiftleft(mean, seasonof1st - 1), length.out = n )# both given
        y <- x - mu
    }

    if(whiten){ # compute residuals (whiten the series x if x is parma with these params)
        if(is.null(eps)){ # set initial values of eps to zero in this case.
            ## 2016-08-14 - this assumed that x is matrix:
            ##    eps <- matrix(0, nrow = nrow(x), ncol = ncol(x))
            eps <- if(is.matrix(x))
                       matrix(0, nrow = nrow(x), ncol = ncol(x))
                   else
                       numeric(n)
        }

        eps[] <- pc.filter.xarma(eps, y, -theta, -phi , period, q, p, n, from, seasonof1st
                                # 2014-02-06 (krapka), intercept, nintercept
                               , if(!is.null(intercept)) - intercept else intercept
                               , if(!is.null(nintercept)) - nintercept else nintercept
                                 )
        return(eps)
    }else{      # compute a series from given residuals ("colour"/unwhiten eps)
        y[]   <- pc.filter.xarma(y, eps, phi, theta,    period, p, q, n, from, seasonof1st
                               , intercept, nintercept)
        x[from:n] <- if(flag.mean) (y+mu)[from:n]
                     else               y[from:n]
        return(x)
    }
}
