sim_pwn <- function(n = 100, period = NA, seasonof1st = 1, scale = NULL, shift = NULL,
                    f = rnorm, ...){
    nsim <- n + seasonof1st - 1

    if(is.na(period))  # TODO: this is dubious; make this an error
      period <- if(class(f) == "list")
                    length(f)
                else
                    max(length(shift), length(scale))

    if(class(f) != "list"){  ## dangerous way to check!
        eps <- f(nsim, ...)
    }else{
        eps <- numeric(nsim)
        for(i in 1:period){
            ind <- seq(i, nsim, by = period)
            wrk <- f[[i]]
            eps[ind] <- do.call(wrk[[1]], c(length(ind), wrk[-1], ...))
        }
    }

    if(!is.null(scale))        # these rely on recycling
        eps <- scale * eps
    if(!is.null(shift))
        eps <- shift + eps

    eps[seasonof1st:nsim]
}

                                                                     # innov = rand.gen(,,...)
## 2016-08-13 -  did some bug fixing
##    'seasonof1st' was passed unchanged to pc.filter
##    but the vectors passed to pc.filter may have values prepended.
##    So, pc.filter might not work with correct seasons if xbefore was there.
##    Similarly for 'innovinit' and sim_pwn())
##
## 2019-05-10 return PeriodicTS object (now that the class has been settled.
##     TODO: add argument to control this?
##
## TODO: This is a very old function patched several times, maybe needs rewrite.
sim_pc <- function(model, n = NA, randgen = rnorm, seasonof1st = 1, nepochs = NA,
                   n.start = NA, x, eps, nmean = NULL, nintercept = NULL, ...  ){

    defaultnyears <- 100
    if(missing(eps)){
        innov       <- NA
        innovinit   <- NA
        innovbefore <- NA
    }else if( class(eps)=="list" ){
        innovbefore <- eps$before
        innovinit   <- eps$init
        innov       <- eps$main
    }else{
        innov       <- eps
        innovinit   <- NA
        innovbefore <- NA
    }

    if( missing(x) || is.na(x) ){
        xbefore <- NA
        xinit   <- NA
    }else if( class(x)=="list" ){
        xbefore <- x$before
        xinit   <- x$init
    }else{
        xbefore <- x    # dali tova e estestveno?
        xinit   <- NA
    }

    if(missing(nintercept)){
        ct.before <- NULL
        ct.init   <- NULL
        ct        <- NULL
    }else if( class(nintercept)=="list" ){
        ct.before <- nintercept$before
        ct.init   <- nintercept$init
        ct        <- nintercept$main
    }else{
        ct.before <- NULL
        ct.init   <- NULL
        ct        <- nintercept
    }

    if(is.na(n.start)) # the default is NA, so that in future automatic choice may be offered
        n.start <- 0

    period <- model$period
    if(is.na(n))
        if(!is.na(nepochs))
            n <- period*nepochs
        else
            n <- period*defaultnyears

    if(       is.na(innovbefore)  &&  !is.na(xbefore) )
        innovbefore <- numeric(length(xbefore))
    else if( !is.na(innovbefore)  &&   is.na(xbefore) )
        xbefore <- numeric(length(innovbefore))
    else if(  is.na(innovbefore)  &&   is.na(xbefore) )
        innovbefore <- xbefore <- numeric(0)
    else if( !is.na(innovbefore)  &&  !is.na(xbefore) )
        if(length(innovbefore) != length(xbefore) )
            stop("Lengths of xbefore and innovbefore must be equal if both are present.")

    if(       is.na(innovinit)  &&  !is.na(xinit) )
        innovinit <- numeric(length(xinit))
    else if( !is.na(innovinit)  &&   is.na(xinit) )
        xinit <- numeric(length(innovinit))
    else if(  is.na(innovinit)  &&   is.na(xinit) )
        innovinit <- xinit <- numeric(0)
    else if( !is.na(innovinit)  &&  !is.na(xinit) )
        if(length(innovinit) != length(xinit) )
            stop("Lengths of xinit and innovinit must be equal if both are present.")

    n.before <- length(xbefore)

    if(is.na(innov)){
        ## 2016-08-13 need to modify seasonof1st if length(innovinit) > 0;
        ##   doing it lazy - generate with seasonof1st,
        ##                   then drop length(innovinit) values
        ##       eps <- sim_pwn(n.start + n - length(innovinit),
        ##                      period, seasonof1st, f = randgen, ...)
        eps <- sim_pwn(n.start + n, period, seasonof1st, f = randgen, ...)
        if(length(innovinit) > 0)
            eps <- eps[-(1:length(innovinit))]
    }else if(length(innov) == n.start + n - length(innovinit))
        eps <- innov
    else
        stop("length(innov) is not compatible with n.start,n and length(innovinit).")

    if( is.null(ct.before) ) ct.before <- numeric(length(xbefore))
    if( is.null(ct.init) )   ct.init   <- numeric(length(xinit))

    eps <- c(innovbefore, innovinit, eps )
    x   <- c(xbefore    , xinit    , numeric(n.start + n - length(xinit)) )
    ctt <- if(is.null(ct))
               NULL
           else
               c(ct.before  , ct.init  , ct )

    stopifnot(length(x) == length(eps),
              is.null(ctt) || length(eps) == length(ctt)
              )

    from <- 1 + length(xbefore) + length(xinit) # todo: some protection needed ...

    ## 2016-08-13 need to modify seasonof1st if n.before > 0, was not done before!
    mod.seasonof1st <- toSeason(1 - n.before, period, seasonof1st)

    if(from==1)
        x <- pc.filter(model, x, eps, seasonof1st = mod.seasonof1st,
                       nmean = nmean, nintercept = ctt)
    else
        x <- pc.filter(model, x, eps, seasonof1st = mod.seasonof1st, from = from,
                       nmean = nmean, nintercept = ctt)

    if(n.start > 0 || n.before > 0)
        x <- x[-(1:(n.before + n.start))]

    pcts(x, nseasons = period)
}
