## Anderson (1971), p. 112-113
Mtrig <- function(period, harmonics = NULL){
    flag <- period %% 2 == 0

    if(is.null(harmonics)){
        npairs <- period %/% 2 - flag  # or: if(flag) period / 2 - 1  else (period - 1) / 2
        
        mt <- .row(c(period, npairs)) # matrix(1:period, nrow = period, ncol = npairs)
        mk <- .col(c(period, npairs)) # matrix(1:npairs, nrow = period, ncol = npairs, byrow = TRUE)
        m <- mt * mk * 2/period    # pointwise multiplication

        wh_flags <- 1:ncol(m)
    }else{
        npairs_max <- period %/% 2 - flag  # or: if(flag) period / 2 - 1  else (period - 1) / 2
        
        pairs <- which((1:npairs_max) %in% harmonics)
        npairs <- length(pairs)
        if(npairs > 0){
            mt <- matrix(1:period, nrow = period, ncol = npairs)
            mk <- matrix(pairs, nrow = period, ncol = npairs, byrow = TRUE)
            m <- mt * mk * 2/period    # pointwise multiplication
        }else{
            m <- matrix(numeric(0), nrow = period, ncol = npairs) 
        }
    }

    wrk <- rbind(cospi(m), sinpi(m))
    ## reshape to make alternate cos, sin, cos, sin, ...
    res <- matrix(wrk, nrow = period)

    if(is.null(harmonics) || 0 %in% harmonics)
        res <- cbind(1/sqrt(2), res)
    
    if(flag && (is.null(harmonics) || (period/2) %in% harmonics)){
        res <- cbind(res, rep(c(-1, 1)/sqrt(2), length.out = period))
    }

    ## 2020-05-25 multiplying by sqrt(2/T),
    ##            TODO: did I initially leave this out on purpose?
    res <- sqrt(2/period) * res

    .harmonics(res) <- harmonics

    res
}

## for now just set or get attribute 'harmonics' 
## TODO: need more meaningful processing here
`.harmonics<-` <- function(x, ..., value){
    attr(x, "harmonics") <- value
    x
}

.harmonics <- function(x){
    attr(x, "harmonics")
}

.harmonics_names <- function(h, period){
    ## h <- attr(x, "harmonics")
    if(length(h) == 0)
        h <- 0:(period %/% 2)

    half <- period / 2
    flag <- period %% 2 == 0
    wrk <- lapply(h,
                  function(x){
                      if(x == 0 || x == half)
                          paste0("h", x)
                      else
                          paste0("h", x, c("cos", "sin"))
                  })
    unlist(wrk)
}

.harmonics_names2 <- function(h, period, order, sintercept){
    ## TODO: this needs more careful processing of 'h', especially with sintercept
    wrk <- .harmonics_names(h, period)
    res <- lapply(1:order, function(x) paste0("Lag", x, "_", wrk))
    if(length(sintercept) > 0 && sintercept)
        res <- list(paste0("C", "_", wrk), res)
    unlist(res)
}

.harmonics_names3 <- function(h, period, order, sintercept){
    ## TODO: this needs more careful processing of 'h', especially with sintercept
    wrk <- .harmonics_names(h, period * (order + sintercept))
    wrk
}

.harmonics_names4 <- function(h, period, order, sintercept){
    ## TODO: this needs more careful processing of 'h', especially with sintercept
    wrk <- .harmonics_names(h, period * order)
    if(sintercept)
       wrk <- c(paste0("C_", .harmonics_names(h, period)), wrk)
    wrk
}

Mtrig_Lund <- function(period, order, harmonics = NULL, sintercept = FALSE){
    if(is.null(harmonics) || is.numeric(harmonics)){
        m <- Mtrig(period, harmonics)
        wrk <- lapply(1:(sintercept + order), function(x) m) #todo: is there a simpler way to make this list?
        mat <- bdiag(wrk)
        .harmonics(mat) <- harmonics
    }else if(is.list(harmonics)){
        ## TODO: verify that other functions process this correctly
        ##       (until that is done, assume they do not))
        wrk <- lapply(1:(sintercept + order), function(ind) Mtrig(period, harmonics[[ind]]))
        mat <- bdiag(wrk)
        .harmonics(mat) <- lapply(wrk, .harmonics)
    }else{
        stop("argument harmonics is of unsupported class")
    }

    mat
}

## Mrig with intercept, this is for "vecbyrow"
sMtrig <- function(period, order, harmonics = NULL, sintercept = FALSE){
    ## TODO: the result should indicate somehow the different cases below
    if(sintercept){
        merge.flag <- isTRUE(attr(sintercept, "merge"))
        if(merge.flag){
             mat <- Mtrig((1 + order) * period, harmonics)
             mat
        }else{
            if(is.list(harmonics)  && length(harmonics) == 1)
                ## remove redundant 'list' wrapper
                harmonics <- harmonics[[1]]
            
            if(is.list(harmonics)){
                h.si <- harmonics[[1]]
                h.phi <- harmonics[-1]
                if(length(h.phi) == 1)
                    h.phi <- h.phi[[1]] 
            }else{
                h.si <- h.phi <- harmonics
            }
            msi <- Mtrig(period, h.si)
            mphi <- Mtrig(period * order, h.phi)
            mat <- bdiag(msi, mphi)
            .harmonics(mat) <- harmonics # or lapply(list(msi, mphi), .harmonics)  ?

            mat
        }
    }else
        Mtrig(order * period, harmonics)
}

## take the rows of M corresponding to season
HsM <- function(season, period, order, M, sintercept = FALSE){
    ind <- if(!sintercept)
               (season - 1) * order + 1:order
           else if(isTRUE(attr(sintercept, "merge")))
               (season - 1) * (order + sintercept) + 1:(order + sintercept)
           else # all intercepts are before the phi parameters
               c(season,  period + (season - 1) * order + 1:order)
                 
    M[ind, , drop = FALSE]
}

## take the rows of M corresponding to season
HsM_Lund <- function(season, period, order, M, sintercept = FALSE){
    ind <- (1:(order + sintercept) - 1) * period + season
    M[ind, , drop = FALSE]
}

data_trig <- function(x, order, period, sintercept = FALSE,
                      seasonof1st = 1, type = c("vecbyrow", "bylag"), harmonics){
    ## x - ts, order - PAR order
    stopifnot(length(order) == 1)

    type <- match.arg(type)

    ## TODO: more efficient implementation
    n <- length(x)
    y <- x[(order + 1):n]

    Xorig <- sapply(order:1, function(start) x[start : (n - (order - (start - 1)))])
    if(sintercept)
        Xorig <- cbind(1, Xorig)
    
    seasons <- 1:period
    if(seasonof1st > 1)
        seasons <- seasons[c(seasonof1st : period, 1 : (seasonof1st - 1))]
    seasons <- PeriodicVector(seasons)[order + 1:period]

    M <- if(type == "vecbyrow")
             ## Mtrig(order * period, harmonics)
             sMtrig(period, order, harmonics, sintercept = sintercept)
         else
             Mtrig_Lund(period, order, harmonics, sintercept = sintercept)
             
    nr <- nrow(Xorig)

    X <- matrix(NA_real_, nrow = nr, ncol = ncol(M))

    if(type == "vecbyrow"){
        for(i in 1:period){
            s <- seasons[i]
            hsm <- HsM(s, period, order, M, sintercept = sintercept)
            
            ind <- seq(i, nr, by = period)
            X[ind, ] <- as.matrix(Xorig[ind, ] %*% hsm)
        }
    }else{ 
        for(i in 1:period){
            s <- seasons[i]
            hsm <- HsM_Lund(s, period, order, M, sintercept = sintercept)
            
            ind <- seq(i, nr, by = period)
            ## the class of rhs is from 'Matrix',
            ## todo: without as.matrix the assignment gives error.
            ##       Report as a possible bug? Probably not, '[<-' is internal generic
            ##       and the method for ordinary matrices is probably triggered.
            X[ind, ] <- as.matrix(Xorig[ind, ] %*% hsm)
        }
    }

    list(y = y, X = X, period = period, seasonof_y_1 = seasons[1], type = type, M = M,
         harmonics = harmonics, sintercept = sintercept)
}

setClass("SubsetPM",
         slots = c(
             theTS = "ANY", # dali da e part of the model?
             period = "integer",
             order = "integer",
             findex = "function", # see `ind' in HsM and HsM_Lund
             harmonics = "integer", # integer(0) if not applicable (but what that means?)
             call = "call",
             other = "namedList"
             )
         )

## setGeneric("@<-")
## setMethod("@<-", "SubsetPM",
##           function(object, name, value){
##               object@name <- value
##               object@other <- "Ah"
##               object
##           }
##           )

`@<-.SubsetPM` <- 
          function(object, name, value){
              ## this of course leads to infinite recursion:
              ##     object@name <- value
              ##     object@other <- "Ah"
              slot(object, name) <- value
              slot(object, "other") <- "Ah" # TODO: this is for testing only!
              object
          }

## TODO: sintercept etc.?
fit_trigPAR <- function(x, order, nseasons, seasonof1st = 1, maxiter = 200, tol = 1e-7, 
                        weights, verbose = TRUE){
    CALL <- match.call()
    data <- data_trig(x, order, nseasons, seasonof1st = seasonof1st, harmonics = NULL)

    nr <- nrow(data$X)
    if(missing(weights))
        weights <- rep(nseasons, nr)
    else
        weights <- rep(weights, length.out = nr)
    niter <- 0
    sumsqold <- Inf
    sigma2 <- Inf
    ## for now simple iteration
    repeat {
        niter <- niter + 1
        fit <- lm(y ~ 0 + X, data = data[c("y", "X")], weights = weights)
        ## values of the objective function
        eps <- weighted.residuals(fit)
        sumsq <- sum(eps^2) / nr
        
        ## new estimate for sigma2
        sigma2 <- sapply(1:nseasons,
                         function(i){
                             ind <- seq(i, nr, by = nseasons)
                             ## !!! Note: non-weighted
                             sum(residuals(fit)[ind]^2) / length(ind)
                         })

        if(verbose){
            cat("sumsq: ", sumsq, "\n",
                "abs(sumsqold - sumsq)", abs(sumsqold - sumsq), "\n",
                "sigma2: ", sigma2, "\n",
                "sigma2/sum(sigma2)", sigma2/sum(sigma2), "\n")
        }
        
        weights <- rep(sum(sigma2) / sigma2, length.out = nr)
        if(abs(sumsqold - sumsq) < tol)
            ## TODO: do a final lm() here? - answer: no?
            break

        if(niter >= maxiter)
            break
        sumsqold <- sumsq
    }
    ## TODO: need to relabel the coefficients, so that they correspond to actual seasons
    ##       when seasonof1st > 1
    if(verbose)    
        cat("niter = ", niter, "\n")
#browser()

    other <- new("namedList", list(fit = fit, data = data, sigma2 = sigma2, sumsq = sumsq))

    res <- new("SubsetPM",
               theTS = x,
               period = as.integer(nseasons),
               order = as.integer(order),
               ##findex = ,
               ##harmonics = harmonics, # TODO: currently there is no argument 'harmonics' for this f.
               call = CALL,
               other = other
               )

    res
}

fit_trigPAR_optim <- function(x, order, nseasons, seasonof1st = 1, maxiter = 200,
                              harmonics = NULL, sintercept = FALSE,
                              tol = 1e-7, type = c("vecbyrow", "bylag"), verbose = TRUE){
    type <- match.arg(type)

    CALL <- match.call()

    data <- data_trig(x, order, nseasons, seasonof1st = seasonof1st, type = type, 
                      harmonics = harmonics, sintercept = sintercept) 

    nr <- nrow(data$X)

    f <- function(x){ # is log sigma2
        sigma2 <- exp(x)
        sigma2 <- sigma2 / sum(sigma2)
        weights <- rep(1/sigma2, length.out = nr)
#print(weights)
        fit <- lm(y ~ 0 + X, data = data[c("y", "X")], weights = weights)
            # eps <- residuals(fit)
	    # 
            # sigma2 <- sapply(1:nseasons,
            #                  function(i){
            #                      ind <- seq(i, nr, by = nseasons)
            #                      ## !!! Note: non-weighted
            #                      sum(eps[ind]^2) / length(ind)
            #                  })
            # weights <- rep(1/sigma2, length.out = nr)
            # sumsq <- sum(weights * eps^2) / nr
        sumsq <- sum(weighted.residuals(fit)^2) / nr
if(verbose){
    print(sigma2)
    print(sumsq)
}
        sumsq
    }

    wrk <- optim(rep(0, nseasons), f, control = list(reltol = tol))

    ## lm() prepends "X" to the names of the coefficients in fit$coef, see below
    colnames(data$X) <- .trig_vec_coef_names(data$harmonics, data$period, order, 
                                             data$sintercept, data$type) 

    sigma2 <- exp(wrk$par)
    sigma2 <- sigma2 / sum(sigma2)
    weights <- rep(1/sigma2, length.out = nr)
    fit <- lm(y ~ 0 + X, data = data[c("y", "X")], weights = weights)

    ## see the comment above; TODO: vcov still prepends 'X' to the names
    names(fit$coef) <- colnames(data$X)

    e <- residuals(fit)
    sigma2ls <- .permean(e^2, nseasons)
    
        # res <- list(fit = fit, data = data,
        #             optim = wrk, type = type, sigma2ls = sigma2ls, sigma2optim = sigma2)
    
    other <- new("namedList",
                 list(fit = fit, data = data, ##sigma2 = sigma2, sumsq = sumsq
                      optim = wrk, type = type, sigma2ls = sigma2ls, sigma2optim = sigma2
                      ))

    res <- new("SubsetPM",
               theTS = x,
               period = as.integer(nseasons),
               order = as.integer(order),
               ##findex = ,
               ##harmonics = harmonics, # TODO: currently there is no argument 'harmonics' for this f.
               call = CALL,
               other = other
               )

    res
}

.permean <- function(x, nseasons){
    n <- length(x)
    sapply(1:nseasons,
           function(i){
               ind <- seq(i, n, by = nseasons)
               sum(x[ind]) / length(ind)
           })
}

## TODO: add argument to convert to "phi"
setMethod("coef", "SubsetPM",
          function(object, type = NULL, ..., matrix = FALSE){
              ## passing "..." but (:TODO:) for "standardize" it is not clear
              ## if the weights need to be updated with the final estimate of sigma2.
              ##
              ## TODO: this could be sorted out by doing final lm() in the fitting functions,
              ##       but this is like chicken and egg. It is better to assume that the
              ##       fitting function has reached a fixed point. 
              co <- coef(object@other$fit, ...)  # object@other$fit$coef
              objtype <- object@other$type
              period <- object@period
              order <- object@order
              sintercept <- object@other$data$sintercept

              if(!is.null(type) && type == "PAR"){
                  ## TODO:
                  ## crude patch 2022-01-24 - I have forgotten what was going on here
                  beta  <- as.vector(object@other$data$M %*% co)

                  merge.flag <- isTRUE(attr(sintercept, "merge"))
                  if(sintercept && !merge.flag){
                      beta0 <- beta[1:period]
                      beta <- beta[-(1:period)]
		  }
                  
                  phi <- if(objtype == "vecbyrow")
                             matrix(beta, nrow = object@period, byrow = TRUE)
                         else # "bylag"
                             matrix(beta, nrow = object@period)
                  
                  if(sintercept && !merge.flag){
                      phi <- cbind(beta0, phi)
                  }
                  
                  colnames(phi) <- c(if(sintercept) "Sintercept" else character(0), 
                                     paste0("Lag_", 1:order))
                  ## TODO: this should use names of seasons from the object)
                  rownames(phi) <- paste0("Season_", 1:nrow(phi))
                  
                  return(phi)  # Attention: !!! Early return !!!
              }
              
              if(!is.null(type) &&  objtype != type){
                  ## compute the PAR coef's and convert them to the desired type
                  ##  (by row or by lag)
                  beta  <- as.vector(object@other$data$M %*% co)
                  ## phi2C expects matrix
                  phi <- if(objtype == "vecbyrow")
                             matrix(beta, nrow = object@period, byrow = TRUE)
                         else # "bylag"
                             matrix(beta, nrow = object@period)
                  co <- phi2C(phi, type = type)
              }
              
              if(matrix){
                  ## convert to matrix
                  if(objtype == "vecbyrow"){
                      merge.flag <- isTRUE(attr(sintercept, "merge"))
                      if(sintercept && !merge.flag){
                          ## co <- cbind(co[1:period], matrix(co[-(1:period)],
                          ##                                  nrow = object@period,
                          ##                                  byrow = TRUE ))
                          co <- matrix(co, ncol = 1)
                          ## colnames(co) <- .harmonics_names(object@harmonics, period * order)
                          rownames(co) <- .harmonics_names4(object@harmonics, period, order, sintercept)
                      }else{
                          nams <- .harmonics_names3(object@harmonics, period, order, sintercept)
                          nr <- length(nams)
                          co <- matrix(co, ncol = 1)
                          rownames(co) <- nams
                          ## rnames <- .harmonics_names(object@harmonics, period)
                          ## nr <- length(rnames)
                          ## co <- matrix(co, nrow = nr, byrow = TRUE)
                          ## ## colnames(co) <- .harmonics_names(object@harmonics, period * (order + 1))
                          ## colnames(co) <- c(if(sintercept) "Sintercept" else character(0), 
                          ##               paste0("Lag_", 1:order))
                      	  ## 
                          ## rownames(co) <- .harmonics_names(object@harmonics, period)
                          
                      }
                  }else{
                      rnames <- .harmonics_names(object@harmonics, period)
                      nr <- length(rnames)
                      co <- matrix(co, nrow = nr)
                      ## colnames(co) <- .harmonics_names2(object@harmonics, period, order, 
                      ##                                   sintercept)
                      ## colnames(co) <- .harmonics_names2(object@harmonics, period, order, 
                      ##                                   sintercept)
                      colnames(co) <- c(if(sintercept) "Sintercept" else character(0), 
                                        paste0("Lag_", 1:order))
                      
                      rownames(co) <- .harmonics_names(object@harmonics, period)

                  }
                  ## rownames(co) <- paste0("S", 1:period)
              }else{
                     # if(objtype == "vecbyrow"){
                     #    merge.flag <- isTRUE(attr(sintercept, "merge"))
                     #    if(sintercept && !merge.flag){
                     #        names(co) <- .harmonics_names4(object@harmonics, period, order, sintercept)
                     #    }else{
                     #        nams <- .harmonics_names3(object@harmonics, period, order, sintercept)
                     #        names(co) <- nams
                     #    }
                     # }else{
                     #     names(co) <- .harmonics_names2(object@harmonics, period, order, 
                     #                                    sintercept)
                     # }
                  names(co) <- .trig_vec_coef_names(object@harmonics, period, order, 
                                                    sintercept, objtype)
              }
              co
          }
          )

.trig_vec_coef_names <- function(harmonics, period, order, sintercept, objtype){
    if(objtype == "vecbyrow"){
        merge.flag <- isTRUE(attr(sintercept, "merge"))
        if(sintercept && !merge.flag){
            nams <- .harmonics_names4(harmonics, period, order, sintercept)
        }else{
            nams <- .harmonics_names3(harmonics, period, order, sintercept)
        }
    }else{
        nams <- .harmonics_names2(harmonics, period, order, sintercept)
    }

    nams
}


setMethod("residuals", "SubsetPM",
          function(object, ...){
              ## passing "..." but (:TODO:) for "standardize" it is not clear
              ## if the weights need to be updated with the final estimate of sigma2.
              ##
              ## TODO: this could be sorted out by doin final lm() in the fitting functions,
              ##       but this is like chiken and egg. It is better to assume that the
              ##       fitting function has reached a fixed point. 
              residuals(object@other$fit, ...)
          }
          )

setMethod("fitted", "SubsetPM",
          function(object, ...){
              fitted(object@other$fit, ...)
          }
          )

setMethod("vcov", "SubsetPM",
          function(object, ...){
              vcov(object@other$fit, ...)
          }
          )

## setMethod("confint", "SubsetPM",
##           function(object, ...){
##               vcov(object@other$fit, ...)
##           }
##           )

## see also sarima:::predict.Sarima
predict.SubsetPM <- function (object, n.ahead = 1, se.fit = FALSE, ...){
    if(se.fit){
        se <- NULL
        stop("'se.fit = TRUE' not implemented yet")
    }
    
    stopifnot(length(n.ahead) == 1, n.ahead >= 1)
    
    type <- object@other$type
    stopifnot(!is.null(type))

    seasonof1st <- object@other$data$seasonof_y_1
    stopifnot(!is.null(seasonof1st))

    period <- object@period
    order  <- object@order

    n <- nTicks(theTS(object))
    seasons <- 1:period
    if(seasonof1st > 1)
        seasons <- seasons[c(seasonof1st : period, 1 : (seasonof1st - 1))]
    seasons <- PeriodicVector(seasons)[n + 1:n.ahead]

    ## TODO: can be made more efficient
    co <- coef(object)

    pred <- numeric(n.ahead)

    sintercept <- object@other$data$sintercept

    xt <- object@theTS[n:(n - order + 1)]
    if(sintercept)
        xt <- c(1, xt)
    
    stopifnot(length(xt) == order + sintercept)

    M <- if(type == "vecbyrow")
             ## Mtrig(order * period, harmonics = object@other$data$harmonics)
             sMtrig(period, order, harmonics = object@other$data$harmonics, sintercept = sintercept)
         else
             Mtrig_Lund(period, order, harmonics = object@other$data$harmonics,
                        sintercept = sintercept)

    xt.len <- length(xt) # == sintercept + order
    if(type == "vecbyrow"){
        for(i in 1:n.ahead){
            s <- seasons[i]
            hsm <- HsM(s, period, order, M, sintercept = sintercept)
            
            ## ind <- seq(i, nr, by = period)
            ## X[ind, ] <- Xorig[ind, ] %*% hsm
            design.row <- xt %*% hsm
            pred[i] <- as.vector(design.row %*% co)
            xt <- if(sintercept)
                      c(xt[1], pred[i], xt[-c(1,xt.len)])
                  else
                      c(pred[i], xt[-xt.len])
        }
    }else{ 
        for(i in 1:n.ahead){
            s <- seasons[i]
            hsm <- HsM_Lund(s, period, order, M, sintercept = sintercept)
            
                # ind <- seq(i, nr, by = period)
                # ## the class of rhs is from 'Matrix',
                # ## todo: without as.matrix the assignment gives error.
                # ##       Report as a possible bug? Probably not, '[<-' is internal generic
                # ##       and the method for ordinary matrices is probably triggered.
                # X[ind, ] <- as.matrix(Xorig[ind, ] %*% hsm)
            design.row <- xt %*% hsm
            pred[i] <- as.vector(design.row %*% co)
            xt <- if(sintercept)
                      c(xt[1], pred[i], xt[-c(1, xt.len)])
                  else
                      c(pred[i], xt[-xt.len])
        }
    }
    

    if(se.fit)
        list(pred = pred, se = se)
    else
        pred
 }

## object here is "SubsetPM"
## get the ordinary phi_tk coefficients of PAR
fromSubsetPM <- function(object){
    ## TODO: remove this function!!! See the comments and examples above

    ## actually, the coef() method for SubsetPM should have argument to ask for these.
    type <- object@other$type
    stopifnot(!is.null(type))

    seasonof1st <- object@other$data$seasonof_y_1
    stopifnot(!is.null(seasonof1st))

    period <- object@period
    order  <- object@order

    seasons <- 1:period
    co <- coef(object)

    sintercept <- object@other$data$sintercept

    M <- if(type == "vecbyrow")
             # Mtrig(order * period, harmonics = object@other$data$harmonics)
             sMtrig(period, order, harmonics = object@other$data$harmonics, sintercept = sintercept)
         else
             Mtrig_Lund(period, order, harmonics = object@other$data$harmonics,
                        sintercept = sintercept)
                        
    res <- matrix(NA_real_, nrow = period, ncol = order)
    ## put coef for each season in a row
    if(type == "vecbyrow"){
        for(i in 1:period){
            ## s <- seasons[i]
            s <- i
            hsm <- HsM(s, period, order, M, sintercept = sintercept)
            res[s, ] <- hsm %*% co
        }
    }else{
        for(i in 1:period){
            ## s <- seasons[i]
            s <- i
            hsm <- HsM_Lund(s, period, order, M, sintercept = sintercept)
            res[s, ] <- as.vector(hsm %*% co) # without as.vector - error,
                                              # but why not also when "vecbyrow"?
                                              # ((maybe because it is a row vector there!)
        }
    }

    res
}

## get the ordinary phi_tk coefficients of PAR
## object here is matrix (nseasons, order)

phi2C <- function(object, type =  c("vecbyrow", "bylag"), sintercept = FALSE){
    if(sintercept)
        stop("sintercept = TRUE not implemented yet for phi2C")

    type <- match.arg(type)

    period <- nrow(object)
    order  <- ncol(object)

    seasons <- 1:period
    ## co <- coef(object)

    M <- if(type == "vecbyrow")
             # Mtrig(order * period) # no 'harmonics', at least for now
             sMtrig(period, order, sintercept = sintercept)
         else
             Mtrig_Lund(period, order, sintercept = sintercept)

    beta <- if(type == "vecbyrow")
               as.vector(t(object))
            else
               as.vector(object)

    co <- t(M) %*% beta  # since beta = M %*% co and M is orthogonal
#browser()
    as.vector(co)
}

setMethod("show", "SubsetPM",
          function(object){
              .reportClassName(object, "SubsetPM")
              call <- paste(capture.output(print(object@call)), collapse = "\n")
              cat("call = ", call, "\n",
                  "period = ", object@period, "\n",
                  "order = ", object@order, "\n",
                  ## "findex = "
                  "harmonics = ", 
                       if(is.null(object@harmonics)) "ALL" else object@harmonics, "\n",
                  ## other
                  "type = ", object@other$type, "\n",
                  "include sintercept = ", object@other$data$sintercept, 

                  if(object@other$type == "vecbyrow" && 
                    isTRUE(attr(object@other$data$sintercept, "merge")))
                      "(merged with other coefficients)" else "", 
                      "\n",
                  
                  "coef = ", coef(object), "\n",
                  "sigma2ls = ", object@other$sigma2ls, "\n",
                  "sigma2optim = ", object@other$sigma2optim, "\n",
                  "\n", sep = " ")
              invisible(NULL)
          }
          )
