##  reimplementation of xx.ss using reference classes;  initially a literal translation
mcOptimCore <- setRefClass(
    "mcOptimCore",
    fields = list(
        flag.exploring = "logical"
        , private.counter.fn = "integer"

        # legacy from xx.ss
        , period = "integer"

        , len.block = "integer"
        , n_col = "integer"
        , r_col = "integer"
        , cp_col = "integer"

        , ind.r.eigval = "integer"
        , r_ind = "integer"

        , ind.cp.eigval = "integer"
        , cp_ind = "integer"

        , filter.order = "integer"

        , ev_abs = "matrix"
        , ev_sign = "matrix"
        , co_r = "matrix"
        , co_arg = "matrix"


        , flag.generators = "logical"
        , par.ind  = "ANY"
        , auto.ind = "ANY"
        , inf.ind  = "ANY"
        , universe = "ANY"


        , col.minusinf.ind = "ANY"
        , col.inf.ind      = "ANY"

        , ind1 = "integer"
        , ind2 = "integer"
        , ind3 = "integer"
        , ind4 = "integer"

        , n1 = "integer"
        , n2 = "integer"
        , n3 = "integer"
        , n4 = "integer"

        , seqn1 = "integer"
        , seqn2 = "integer"
        , seqn3 = "integer"
        , seqn4 = "integer"

        , co = "matrix"

        , initmodel = "ANY"
        , wrkmodel = "ANY"
        , model = "list"

        , tempnam = "ANY"
        , param_names = "character"
        , lo_bound = "ANY"
        , up_bound = "ANY"

        , mcss.cnt = "integer"
        , wrkev = "ANY"
        )
    )

mcOptimCore$methods(
    initialize = function(...
                           ## period, type.eigval, n.root, eigabs, eigsign, co_r, co_arg
                           ## , init = NULL
                           ## , len.block = NULL
                           ## , mo.col
                           ## , generators = NULL
                           ){
        dots <- list(...)

        period <<- dots$period

        len.block <<-
            if(is.null(dots$len.block))
                rep(1, length(dots$type.eigval))
            else
                dots$len.block

        n_col <<- sum(len.block)

        r_col <<- which(dots$type.eigval == "r")
        cp_col <<- which(dots$type.eigval == "cp")

        ind.r.eigval <<- which(dots$type.eigval == "r")
        r_ind <<- chain_ind(ind.r.eigval, len.block)

        ind.cp.eigval <<- which(dots$type.eigval == "cp")
        cp_ind <<- chain_ind(ind.cp.eigval, len.block)

        filter.order <<- length(r_ind) + 2*length(cp_ind)

        ev_abs  <<- if(dots$eigabs)  rep(NA_real_, ind.r.eigval) else dots$eigabs
        ev_sign <<- if(dots$eigsign) rep(NA_real_, ind.r.eigval) else dots$eigsign

        co_r  <<-
            if(dots$co_r)   matrix(NA_real_, nrow = period, ncol = n_col) else dots$co_r
        co_arg  <<-
            if(dots$co_arg) matrix(NA_real_, nrow = period, ncol = n_col) else dots$co_arg



        flag.generators <<- !is.null(generators)  # 2014-11-12 krapka to incorporate subspaces
        if(flag.generators){                      #            for unit roots
            par.ind <<- NA
            auto.ind <<- NA
            inf.ind <<- NA
            universe <<- NA

            col.minusinf.ind <<- NA
            col.inf.ind <<- NA

            fun.gener.simple()
        }

        ind1 <<- which(is.na(ev_abs))
        ind2 <<- which(is.na(ev_sign) & dots$type.eigval != "r")

        ev_sign[is.na(ev_sign) & dots$type.eigval == "r"] <<- 1 # krapka :TODO:

        ind3 <<- which(is.na(co_r))
        ind4 <<- if(length(cp_ind) > 0)
                     which(is.na(co_arg[ , cp_ind]))
                 else
                     integer(0)

        n1 <<- length(ind1)
        n2 <<- length(ind2)
        n3 <<- length(ind3)
        n4 <<- length(ind4)

        seqn1 <<- seq(from = 1, length.out=n1)
        seqn2 <<- seq(from = 1+n1, length.out=n2)
        seqn3 <<- seq(from = 1+n1+n2, length.out=n3)
        seqn4 <<- seq(from = 1+n1+n2+n3, length.out=n4)


        co <<- if(length(cp_col) > 0)
                   matrix(NA_complex_, nrow = period, ncol = n_col)
               else
                   matrix(NA_real_, nrow = period, ncol = n_col)


        initmodel <<- NA
                    # initmodel is not used in the computations, it is set to the model
                    # created at the time of the xx.ss call. xx.ss sets wrkmodel initially to
                    # initmodel. Computations with the object returned by xx.ss may change
                    # wrkmodel but not initmodel. In particular, repeated runs of the
                    # optimisation functions modify the state (in particular wrkmodel), so
                    # that each subsequent optimisation starts where the previous ended.
                    #
                    # initmodel always remembers the very first set. Note that calling xx.ss
                    # with the same parameters may not create identical objects when argument
                    # init is NULL.

        wrkmodel <<- NA

        model <<- list(period=period, p=dots$n.root, q=0, phi = numeric(0)) # no model yet


        tempnam <<- c(rep("ev", length(ind1)),
                      rep("evSign", length(ind2)),
                      rep("co.r", length(ind3)),
                      rep("cp.arg", length(ind4))
                      )

        param_names <<-
            paste0(tempnam, c(ind1, ind2, ind3, ind4))


        lo_bound <<- c( rep(-1, n1), rep(-pi, n2), rep(-1, n3), rep(-pi, n4) )
        up_bound <<- c( rep( 1, n1), rep( pi, n2), rep( 1, n3), rep( pi, n4) )


                          # if init=NULL generate random initial values for the unknown params
        if(is.null(dots$init)){
            ev_abs[ind1] <<- runif(n1, 0, 1)

            ev_sign[is.na(ind1)] <<-  1   # TODO: krapka!
            ev_sign[ind2] <<- runif(n2, -pi, pi)

            co_r[ind3] <<- runif(n3, -1, 1)
            if(length(cp_ind) > 0)
                co_arg[ , cp_ind][ind4] <<- runif(n4, -pi, pi)

            wrkev <<- ev_abs
            wrkev[r_col] <<- ev_abs[r_col] * ev_sign[r_col]
        }else if(is.list(dots$init)){
            ev_abs[ind1] <<- dots$init$eigabs[ind1]

            ev_sign[is.na(ind1)] <<-  1   # TODO: krapka!
            ev_sign[ind2] <<- dots$init$eigsign[ind2]

            co_r[ind3] <<- dots$init$co_r[ind3]
            if(length(cp_ind) > 0)
                co_arg[ , cp_ind][ind4] <<- dots$init$co_arg[ , cp_ind][ind4]

            wrkev <<- ev_abs
            wrkev[r_col] <<- ev_abs[r_col] * ev_sign[r_col]

        }else{           # assume 'init' is a vector of parameters, maybe from a previous run
            wrkev <<- ev_abs  # TODO: krapka; inache wrkev ne sastestvuva!
            optparam2mcparam(dots$init)
        }

        pa <- mcparam2optparam()
        optparam2mcparam(pa)               #  optparam2mcparam(pa) sets wrkmodel.
        initmodel <<- wrkmodel

        .self
    },

    mcparam2optparam = function(){
        c(wrkev[ind1], ev_sign[ind2],
          co_r[ind3],
          if(length(cp_ind) > 0)  co_arg[ , cp_ind][ind4]
          else                    numeric(0)
          )
    },

    optparam2mcparam = function(param){
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
    },

                             # exchanged the places of 'param' and 'x' from the original xx.ss
    mcss = function(param, x, eps=numeric(length(x))){
        mepshat <- matepshat(param = param, x = x, eps = eps)

            # pervar <- apply(mepshat, 1, function(x) mean(x^2))
            # persd <- sqrt(pervar) # use for weigthed leasst squares?
            #    Note: !!!  this gives the length of x:
            #          ss <- sum( (mepshat/persd)^2)
        ss <- sum(mepshat^2)                               # TODO: omit the first few values?

        if(flag.exploring)
            process.mcss.cnt(ss, mepshat)

        ss
    },

                             # exchanged the places of 'param' and 'x' from the original xx.ss
    mclik = function(param, x, eps=numeric(length(x))){
        mepshat <- matepshat(param = param, x = x, eps = eps)
        perlog <-  apply(mepshat, 1, function(x) log(sum(x^2)) )
        mclik <- sum(perlog)      # TODO: should be sum( nk * perlog, where nk = c(n1,...,nd)

        if(flag.exploring)
            process.mcss.cnt(mclik, mepshat)

        mclik
    },

    ## temp; compatibility

    mcsigma2 = function(param, x, eps = numeric(length(x))){
        sigma2hat(param, x, eps)
    },


    minim = function(x, ..., CONDLIK = TRUE){
        optimize(optim, if(CONDLIK) "mclik" else "mcss", MCBOUNDS = FALSE, x = x, ...)
    },

    minimBB = function(x, ..., CONDLIK = TRUE){
        optimize(BBoptim, if(CONDLIK) "mclik" else "mcss", MCBOUNDS = FALSE, x = x, ...)
    },

    minimBBlu = function(x, ..., CONDLIK = TRUE){
        optimize(BBoptim, if(CONDLIK) "mclik" else "mcss", MCBOUNDS = TRUE, x = x, ...)
    },


    ## new


    process.mcss.cnt = function(mclik, mepshat){
        if(mcss.cnt >= 100 ||private.counter.fn == 0){
            pervar <- apply(mepshat, 1, function(x) mean(x^2))
            persd <- sqrt(pervar)

            cat("fn is: ", mclik)
            cat("\tpersd is: ", persd, "\n")
            private.counter.fn <<- 1
        }else
            private.counter.fn <<- private.counter.fn + 1
    },

                 # TODO: in pc.filter: if eps is NULL, x is assumed to be a matrix! check !!!!
    matepshat = function(param, x, eps = numeric(length(x))){
        if(!missing(param))  # note: this changes the parameters !!!
            optparam2mcparam(param)

        epshat <- pc.filter(x, eps = eps, model = model, whiten=TRUE)
        matrix(epshat, nrow = period)
    },

    sigma2hat = function(param, x, eps = numeric(length(x))){
        mepshat <- matepshat(param = param, x = x, eps = eps)
        apply(mepshat, 1, function(x) sum(x^2)/length(x) )
    },


                                                                   # todo: option to maximise?
    optimize = function(optimfn, mcmethod, x, ..., MCBOUNDS = FALSE){
        fn <-
            if(is.function(mcmethod))
                mcmethod
            else
                switch(mcmethod,
                       "mcLikelihood" =      ,
                       "mclik"        = mclik,
                       "mcSS"         =     ,
                       "mcss"         = mcss,
                       # default (for now)
                                        mcss
                       )

        if(flag.exploring)
            private.counter.fn <<- 0   # reset fn calls counter

        par <- mcparam2optparam()
        optres <-
            if(MCBOUNDS)
                optimfn(par, fn, x=x, ...)
            else
                optimfn(par, fn, lower = lo_bound, upper = up_bound, x=x, ...)

                     #  Ensures that the environment contains the best params, not whatever
                     #  params happened to be used in the last calculation of the likelihood
                     #  requested by the optimisation function!
        optparam2mcparam(optres$par)

        names(optres$par) <- param_names
        optres
    }




    )
