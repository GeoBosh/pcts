# the functions below are from 2013-04-02 and later
pcAr.ss <- function(x, model, eps = numeric(length(x))){
    wrk <- pc.filter(model = model, x = x, eps = eps, whiten=TRUE)
    sum(wrk^2)  # TODO: omit the first few values?
}

                                                                           # TODO: unfinished!
xx.ss <- function(period, type.eigval, n.root, eigabs, eigsign, co_r, co_arg, init = NULL
                  , len.block = NULL  # 2014-06-09 new; changes in the body to handle it
                  , mo.col
                  , generators = NULL # 2014-11-12 new argument and corresponding changes
                                      #            below (see optparam2mcparam)
                  ){

    if(is.null(len.block))
        len.block <- rep(1, length(type.eigval))

    n_col <- sum(len.block) # 2014-06-09 was: length(type.eigval) - changed for len.block!

    r_col <- which(type.eigval == "r")
    cp_col <- which(type.eigval == "cp")

    ind.r.eigval <- which(type.eigval == "r")
    r_ind <- chain_ind(ind.r.eigval, len.block)

    ind.cp.eigval <- which(type.eigval == "cp")
    cp_ind <- chain_ind(ind.cp.eigval, len.block)

                   # new 2013-17-10
                   # 2014-06-09 was: length(r_col) + 2*length(cp_col) - modified for len.block
    filter.order <- length(r_ind) + 2*length(cp_ind)

    ev_abs  <- if(missing(eigabs))  rep(NA_real_, ind.r.eigval) else eigabs
    ev_sign <- if(missing(eigsign)) rep(NA_real_, ind.r.eigval) else eigsign

    co_r    <- if(missing(co_r))    matrix(NA_real_, nrow = period, ncol = n_col) else co_r
    co_arg  <- if(missing(co_arg))  matrix(NA_real_, nrow = period, ncol = n_col) else co_arg

    ## TODO: check the dimensions of the above!


    flag.generators <- !is.null(generators)   # 2014-11-12 krapka to incorporate subspaces
    if(flag.generators){                      #            for unit roots
        par.ind <- NA
        auto.ind <- NA
        inf.ind <- NA
        universe <- NA
        ## fun.gener <- function(){
        ##     id.ev1 <- which(eigabs == 1) # todo: check the sign!
        ##                   # ev1 contains column indices of e. vectors corresponding to one.
           ##     ev1 <- sapply(1:length(id.ev1),
           ##                   function(x) chain_ind(id.ev1[x], len.block)[ , 1]
           ##            )
           ##
           ##     universe <<- generators[[1]]$universe
           ##
           ##     a <- 1 : ncol(generators[[1]][[ "tall" ]])
           ##     b <- ncol(generators[[1]][[ "tall" ]]) +
           ##                          1 : ncol(generators[[1]][[ "hang" ]])
           ##     if(generators$param == "tall"){
           ##         par.ind <<- a
           ##         auto.ind <<- b
           ##     }else{ # == "hang"
           ##         par.ind <<- b
           ##         auto.ind <<- a
           ##     }
           ##     inf.ind <<- which(co_r == Inf)
           ## }
           ##
           ## generate_auto_co <- function(){
           ##     locwrk <- null_complement( co[ , id.ev1[par.ind]], universe = universe )
           ##     co_r[inf.ind] <<- locwrk
           ## }
           ##
           ## fun.gener()  # 2014-11-12 krapka for generators

        col.minusinf.ind <- NA
        col.inf.ind <- NA

        fun.gener.simple <- function(){
            col.inf.ind <<- which(sapply(1:ncol(co_r),
                                         function(x)
                                             # all(co_r[ ,x] == Inf) && all(co_arg[ ,x] == 0)
                                             all(co_r[ ,x] == Inf) && all(co_arg[ ,x] == 1)
                                         ))
            col.minusinf.ind <<- which(sapply(1:ncol(co_r),
                                              function(x)
                                               #all(co_r[ ,x] == Inf) && all(co_arg[ ,x] != 0)
                                               all(co_r[ ,x] == Inf) && all(co_arg[ ,x] == -1)
                                              ))

            ## now set -Inf to NA since these params are estimated
            if(length(col.minusinf.ind) > 0){
                co_r[ , col.minusinf.ind] <<- NA_real_
                co_arg[ , col.minusinf.ind] <<- NA_real_
            }# if length(col.minusinf.ind) == 0 will not work,
             # but shouldn't happen with current (2014-11-12) mcSpec

        }

        generate_auto_co <- function(){
            locwrk <- null_complement(co[ , col.minusinf.ind, drop = FALSE]) # universe = universe
            ## 2014-11-28 krapka - locwrk    :TODO:  clean up this
            if(any(Im(locwrk)) != 0 )
                warning("Some component of supposedly real 'co's are complex")
            co_r[ , col.inf.ind] <<- Re(locwrk)

            co[ , col.inf.ind] <<- locwrk
        }


        fun.gener.simple()
    }



    ind1 <- which(is.na(ev_abs))

    # ind2 <- which(is.na(ev_sign)) # 2013-10-14; added additional condition
    ind2 <- which(is.na(ev_sign) & type.eigval != "r")
    ev_sign[is.na(ev_sign) & type.eigval == "r"] <- 1 # krapka :TODO:

    ind3 <- which(is.na(co_r))
    ind4 <- if(length(cp_ind) > 0)
                which(is.na(co_arg[ , cp_ind]))
            else
                integer(0)

    n1 <- length(ind1)
    n2 <- length(ind2)
    n3 <- length(ind3)
    n4 <- length(ind4)

    seqn1 <- seq(from = 1, length.out=n1)
    seqn2 <- seq(from = 1+n1, length.out=n2)
    seqn3 <- seq(from = 1+n1+n2, length.out=n3)
    seqn4 <- seq(from = 1+n1+n2+n3, length.out=n4)


    co <- if(length(cp_col) > 0)
              matrix(NA_complex_, nrow = period, ncol = n_col)
          else
              matrix(NA_real_, nrow = period, ncol = n_col)


    initmodel <- NA # initmodel is not used in the computations, it is set to the model
                    # created at the time of the xx.ss call. xx.ss sets wrkmodel initially to
                    # initmodel. Computations with the object returned by xx.ss may change
                    # wrkmodel but not initmodel. In particular, repeated runs of the
                    # optimisation functions modify the state (in particular wrkmodel), so
                    # that each subsequent optimisation starts where the previous ended.
                    #
                    # initmodel always remembers the very first set. Note that calling xx.ss
                    # with the same parameters may not create identical objects when argument
                    # init is NULL.


    wrkmodel <- NA
                    # 2013-06-20: was p=6; fixed!
    model <- list(period=period, p=n.root, q=0, phi = numeric(0)) # no model yet

    mcparam2optparam <- function(){
        c(wrkev[ind1], ev_sign[ind2], co_r[ind3],
          if(length(cp_ind) > 0)  co_arg[ , cp_ind][ind4]
          else                    numeric(0)
          )
    }


    tempnam <- c(rep("ev", length(ind1)),
                 rep("evSign", length(ind2)),
                 rep("co.r", length(ind3)),
                 rep("cp.arg", length(ind4))
                 )

    param_names <-
        paste0(tempnam, c(ind1, ind2, ind3, ind4))


    lo_bound <- c( rep(-1, n1), rep(-pi, n2), rep(-1, n3), rep(-pi, n4) )
    up_bound <- c( rep( 1, n1), rep( pi, n2), rep( 1, n3), rep( pi, n4) )

    optparam2mcparam <- function(param){

        wrkev[ind1] <<- param[seqn1]
        ev_abs <<- wrkev
        ev_abs[r_col] <<- abs(wrkev[r_col])

        ev_sign[r_col] <<- ifelse(wrkev[r_col] >= 0, 1, -1)
        ev_sign[ind2] <<- param[seqn2]

        co_r[ind3] <<- param[seqn3]
        if(length(cp_ind) > 0)
            co_arg[ , cp_ind][ind4] <<- param[seqn4]


        co[ , r_ind] <<- co_r[, r_ind]
        if(length(cp_ind) > 0)               # krapka 2013-06-20: case of no complex roots
            co[ , cp_ind] <<- co_r[ , cp_ind] * exp(1i * co_arg[ , cp_ind])

        if(flag.generators){          # 2014-11-12 krapka for generators
            generate_auto_co()
        }

        wrkmodel <<- sim_pcfilter(period = period
                                  , n.root = n.root
                                  , order = filter.order # new 2013-17-10
                                  , eigabs = ev_abs
                                  , eigsign = ev_sign
                                  , co = co
                                  , type.eigval = type.eigval

                                  , len.block = len.block # 2014-06-09 new
                                  , mo.col = mo.col   # 2014-06-10 new
                                  )

        # browser()

        model$phi <<- wrkmodel$pcfilter
        NULL
    }

    mcss.cnt <- 0

    mcss <- function(x, param, eps=numeric(length(x))){
        if(!missing(param))
            optparam2mcparam(param)

        epshat <- pc.filter(model = model, x = x, eps = eps, whiten = TRUE)

        mepshat <- matrix(epshat, nrow = period)   # 2013-12-03 was: nrow=4
        pervar <- apply(mepshat, 1, function(x) mean(x^2))
        persd <- sqrt(pervar)

                                 # ss <- sum( (mepshat/persd)^2) # this gives the length of x!
        ss <- sum( mepshat^2)          # TODO: omit the first few values?

        mcss.cnt <<- mcss.cnt + 1
        if(mcss.cnt >= 100){
            cat("ss is: ", ss)
            cat("\tpersd is: ", persd, "\n")
            mcss.cnt <<- 0
        }

        ss
    }

    mclik <- function(x, param, eps=numeric(length(x))){                      # new 2013-08-27
        if(!missing(param))
            optparam2mcparam(param)

        epshat <- pc.filter(model = model, x = x, eps = eps, whiten = TRUE)

        mepshat <- matrix(epshat, nrow = period)   # 2013-12-03 was: nrow=4
        perlog <-  apply(mepshat, 1, function(x) log(sum(x^2)) )

        mclik <- sum(perlog)      # TODO: should be sum( nk * perlog, where nk = c(n1,...,nd)

                         # 2013-11-26 was: mcss.cnt <<- mcss.cnt + 1 (changes below, as well)
        if(mcss.cnt >= 100 || mcss.cnt == 0){
            pervar <- apply(mepshat, 1, function(x) mean(x^2))
            persd <- sqrt(pervar)

            cat("condlik is: ", mclik)
            cat("\tpersd is: ", persd, "\n")
            mcss.cnt <<- 1
        }else
            mcss.cnt <<- mcss.cnt + 1

        mclik
    }

               # TODO: in pc.filter: if eps is NULL, x is assumed to be a matrix! check !!!!
    mcsigma2 <- function(x, param, eps = numeric(length(x))){
        if(!missing(param))  # note: this changes the parameters !!! :todo:
            optparam2mcparam(param)

        epshat <- pc.filter(model = model, x = x, eps = eps, whiten = TRUE)

        mepshat <- matrix(epshat, nrow = period)   # 2013-12-03 was: nrow=4
        mcsigma2 <-  apply(mepshat, 1, function(x) sum(x^2)/length(x) )
        mcsigma2
    }



    minim <- function(x, ..., CONDLIK = TRUE){        # 2013-08-27 new argument 'CONDLIK'
        par <- mcparam2optparam()
        optres <-
        if(CONDLIK)
            optim(par, function(param, x,...) mclik(x,param,...), x=x, ...)
        else
            optim(par, function(param, x,...) mcss(x,param,...), x=x, ...)

        optparam2mcparam(optres$par) # 2013-10-16 new (also for minimBB, etc. below)
                                        #  ensure that the environment contains the best
                                        #  params, not whatever params happened to be used in
                                        #  the last calculation of the likelihood requested
                                        #  by the optimisation function!

        names(optres$par) <- param_names
        optres
    }

    minimBB <- function(x, ..., CONDLIK = TRUE){      # 2013-08-27 new argument 'CONDLIK'
        par <- mcparam2optparam()
        optres <-
        if(CONDLIK)
            BBoptim(par = par, fn = function(param, x,...) mclik(x,param,...),
                    x=x, ...)
        else
            BBoptim(par = par, fn = function(param, x,...) mcss(x,param,...),
                    x=x, ...)

        optparam2mcparam(optres$par)

        names(optres$par) <- param_names
        optres
    }

    minimBBlu <- function(x, ..., CONDLIK = TRUE){    # 2013-08-27 new argument 'CONDLIK'
        par <- mcparam2optparam()
        optres <-
        if(CONDLIK)
            BBoptim(par = par, fn = function(param, x,...) mclik(x,param,...),
                    lower = lo_bound, upper = up_bound,
                    x=x, ...)
        else
            BBoptim(par = par, fn = function(param, x,...) mcss(x,param,...),
                    lower = lo_bound, upper = up_bound,
                    x=x, ...)

        optparam2mcparam(optres$par)

        names(optres$par) <- param_names
        optres
    }
                         # 2013-11-26 was: always generated random initial values
    if(is.null(init)){   # if init=NULL generate random initial values for the unknown params
        ev_abs[ind1] <- runif(n1, 0, 1)

        ev_sign[is.na(ind1)] <-  1   # TODO: krapka!
        ev_sign[ind2] <- runif(n2, -pi, pi)

        co_r[ind3] <- runif(n3, -1, 1)
        if(length(cp_ind) > 0)
            co_arg[ , cp_ind][ind4]  <- runif(n4, -pi, pi)

        wrkev <- ev_abs
        wrkev[r_col] <- ev_abs[r_col] * ev_sign[r_col]
    }else if(is.list(init)){                                 # 2014-06-09 new
        ev_abs[ind1] <- init$eigabs[ind1]

        ev_sign[is.na(ind1)] <-  1   # TODO: krapka!
        ev_sign[ind2] <- init$eigsign[ind2]

        co_r[ind3] <- init$co_r[ind3]
        if(length(cp_ind) > 0)
            co_arg[ , cp_ind][ind4] <- init$co_arg[ , cp_ind][ind4]

        wrkev <- ev_abs
        wrkev[r_col] <- ev_abs[r_col] * ev_sign[r_col]

    }else{                # assume 'init' is a vector of parameters, maybe from a previous run
        wrkev <- ev_abs  # TODO: krapka; inache wrkev ne sastestvuva!
        optparam2mcparam(init)
    }

    pa <- mcparam2optparam()
    optparam2mcparam(pa)

        # 2013-11-26 was: wrkmodel <- initmodel but optparam2mcparam(pa) above sets wrkmodel.
    initmodel <- wrkmodel



    env <-  sys.frame( sys.nframe() )    # for testing.

#browser()

    list(fmcss = mcss, fparamvec = mcparam2optparam, fmcparam = optparam2mcparam, env=env)
}

                                                            # 2013-10-11 new
mC.ss <- function(spec, ...){                               # 2014-06-09 updated for len.block
    type.eigval <- spec@ev.type
    n.root <- spec@n.root
    len.block <- spec@block.length    # 2014-06-09  new
    mo.col <- spec@mo.col

    eigabs <- spec@ev.abs
    eigsign <- 2 * pi * spec@ev.arg

    co_r <- spec@co.abs
    co_arg <- 2 * pi * spec@co.arg

        # 2014-06-09 was:
        #
        #     ind.r <- sum( spec@block.length[type.eigval == "r"] )
        #     if(ind.r > 0)
        #         co_arg[1:ind.r] <- ifelse(spec@co.arg[1:ind.r] < 1/4, 1, -1)

    ind.r.eigval <- which(type.eigval == "r")
    if(length(ind.r.eigval) > 0){
        eigsign[ind.r.eigval] <-                          # tryabva da e nula ili 1/2 tuk...
            ifelse(spec@ev.arg[ind.r.eigval] < 1/4, 1, -1)

        r_ind <- chain_ind(ind.r.eigval, len.block)
        co_arg[ , r_ind] <- ifelse(spec@co.arg[ , r_ind] < 1/4, 1, -1)
    }

    xxenv <- xx.ss(period = spec@mo, type.eigval = type.eigval, n.root = n.root,
                   eigabs = eigabs, eigsign = eigsign, co_r = co_r, co_arg = co_arg,
                   len.block = len.block,  # 2014-06-09  new
                   mo.col = mo.col,
                   ...)

    xxenv
}
