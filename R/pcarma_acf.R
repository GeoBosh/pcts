## moved here from pc28arma.r after moving the latter to package pctsArma.
##
## TODO: this file contains relies on indexing of 'slMatrix' objects
##           with type = "tl", "tl+-"
##
##       The uses of "tl" seem all to be on locally created slMatrix objects (surely with the
##       express purpose of doing exactly that), but there is one use of "tl+-" as an object
##       received as argument. This will need handling if the default becomes "Lagged2d or a
##       subclass of that.
##
pcarma_acvf_lazy <- function(phi, theta, sigma2, p, q, period, maxlag = 100){
                                                     # to do: dobavi argument h
                                                     # to do: vklyuhi sluchaya p i q - vektori
    ## krapki; probably changing below p =>p[i] and q => q[i] will do.
    ##         (plus taken care that p and q are vectors!)
    p <- max(p) # 2016-01-31 new line; TODO: is this meant to work with vector p?
    q <- max(q) # 2016-08-07 new line; TODO: is this meant to work with vector q?

    h <- pcarma_h_lazy(phi, theta, p, q, period, maxlag)
    wrk <- pcarma_acvf_system(phi, theta, sigma2, p, q, period)
    acf <- solve(wrk$A, wrk$b)

    mtmp <- matrix(NA, nrow = period, ncol = maxlag+1)
    mtmp[1:length(acf)] <- acf

    r <- slMatrix(init = mtmp, period = period, maxlag = maxlag)

    f <- function(t, k){
        if(length(t) > 1 || length(k)>1){                   # compute a matrix of values of h
            res <- matrix(NA, nrow = length(t), ncol = length(k))
            for(i in 1:length(t))
                for(j in 1:length(k))
                    res[i, j] <- Recall(t[i], k[j])
            return(res)
        }else if(k < 0){
            return(Recall(t - k, -k))    # ponezhe Ex(t)x(t-k)=Ex(t-k)x(t)=Ex(t-k)x(t-k-(-k))
        }else if( k <= maxlag && !is.na(r[t, k, type = "tl"]) )
             return( r[t, k, type = "tl"] )
         else{
             res <- if(k == 0) sigma2[toSeason(t-k, period)]  else 0

             if(p>0)
                 for(i in 1:p)
                     res <- res + phi[t, i, type = "co"] * Recall(t - i, k - i)

             if(q>0)
                 for(i in 1:q)
                     res <- res + theta[t, i, type = "co"] *
                                      sigma2[toSeason(t - i, period)] * h(t - i, i - k)

             if( k <= maxlag ){
                 r[t, k, type = "tl"] <<- res
             }
             return(res)
         }
    }
}

pcarma_h_lazy <- function(phi, theta, p, q, period, maxlag = 200){
    env <- new("environment")   # TODO: 2016-01-01 this is very old, don't need envir.here
    env$h <- slMatrix(init = NA, period = period, maxlag = maxlag)
    f <- function(t, k){
        if(length(t)>1 || length(k)>1){                      # compute a matrix of values of h
            res <- matrix(NA, nrow = length(t), ncol = length(k))
            for(i in 1:length(t))
                for(j in 1:length(k))
                    res[i, j] <- Recall(t[i], k[j])
            return(res)
        }else if(k<0)
             return(0)
         else if(k==0)
             return(1)
         else if( k <= maxlag && !is.na(env$h[t, k, type = "tl"]) )
             return( env$h[t, k, type = "tl"] )
         else{
             if(q > 0)
                 res <- theta[t, k, type = "co"]
             else
                 res <- 0
             if(p>0)
                 for(i in 1:p)
                     res <- res + phi[t, i, type = "co"] * Recall(t - i, k - i)
             if( k <= maxlag ){
                 env$h[t, k, type = "tl"] <- res
             }
             return(res)
         }
    }
}
                                                              # 2016-01-02 renamed 'x' to 'h'
pcarma_h <- function(h, na = NA){ # for convenience,  implementation is similar to lazyh
    env <- new("environment") # TODO:
    maxlag <- ncol(h) - 1        # assumes ncol() defined for h
    period <- nrow(h)            # assumes nrow() defined for h
    env$h <- slMatrix(init = h)
    f <- function(t, k){
        if(length(t) > 1 || length(k) > 1){                 # compute a matrix of values of h
            res <- matrix(NA, nrow = length(t), ncol = length(k))
            for(i in 1:length(t))
                for(j in 1:length(k))
                    res[i, j] <- Recall(t[i], k[j])
            return(res)
        }else if(k < 0){
            return(0)
        }else if(k == 0){
            return(1)
        }else if( k <= maxlag && !is.na(env$h[t, k, type = "tl"]) ){
            return( env$h[t, k, type = "tl"] )
        }else{         # mozhe da se napravi da izpalnyava funktsiya tuk ako na=f
            return(NA)
        }
    }
}

pcarma_acvf_system <- function(phi, theta, sigma2, p, q, period){
    p <- max(p) # 2016-01-31 new line; TODO: is this meant to work with vector p?
    n <- (p + 1) * period
    b <- numeric(n)
    m <- matrix(0, nrow = n, ncol = n)
    h <- pcarma_h_lazy(phi, theta, p, q, period, maxlag = p)   # if(q>0) ???
    for(t in 1:period){
        for(k in 0:p){
            r <- t + k * period
            for(i in 0:p){                                 # row r of the matrix of the system
                wrk <- toSeasonPair(t - i, (t - i) -(k - i), period) # see eq.(4.3), bosh paper
                c <- wrk$season + wrk$lag * period
                m[r, c] <- m[r, c] + if(i == 0) 1  else  - phi[t, i, type = "co"]
            }
            b[r] <- if(k == 0) sigma2[toSeason(t - k, period)]  else 0
            if(q > 0)
                for(i in 1:q)                                     # r-h el. of right-hand side
                    b[r] <- b[r] + theta[t, i, type = "co"] *
                                       sigma2[toSeason(t - i, period)] * h(t - k, i - k)
        }
    }
    list(A = m, b = b)
}
                                            # 20/03/2007 promenyam da raboti s vektorni p i q.
                                         # to do: reflect these  changes in the documentation!
pcarma_param_system <- function(acf, h, sigma2, p, q, period){
    if(length(p) == 1)
        p <- rep(p, period)
    if(length(q) == 1)
        q <- rep(q, period)

    n <- sum(p + q + 1)  # was: n <- (p+q+1)*period for scalar p, q
    b <- numeric(n)
    m <- matrix(0, nrow = n, ncol = n)
    r <- 0                            # equation counter

    offset <- c(0, cumsum(p + q + 1))[-(period + 1)] # offset for params of season t
                                       # was: offset <- (p+q+1)*((1:period)-1) for scalar p, q
    sigmaoffset <- 0
    phioffset   <- 1
    # thetaoffset <- p+1
    for(t in 1:period){
        thetaoffset <- p[t]+1
        for(k in 0:p[t]){                                               # equation (4.3)
            r <- r + 1
            b[r] <- acf[t, k]
            if(k == 0)
                m[r, offset[t] + sigmaoffset + 1] <- 1
            if(p[t] > 0)
                for(i in 1:p[t])
                    m[r, offset[t] + phioffset + i] <- acf[t - i, k - i, type = "tl+-"]
            if(q[t] > 0)
                for(i in 1:q[t])
                    m[r, offset[t] + thetaoffset + i] <- sigma2[t - i] * h(t - k, i - k)
        }
        if(q[t] > 0)                                                      # equation (4.4)
            for(l in 1:q[t]){
                r <- r + 1
                b[r] <- h(t, l)
                if(p[t]>0)
                    for(i in 1:p[t])
                        m[r, offset[t] + phioffset + i] <- h(t - i, l - i)
                m[r, offset[t] + thetaoffset + l] <- 1 # inside brackets +l is +el, not +one.
            }
    }
    list(A = m, b = b, p = p, q = q)  # dobavyam p, q na 20/03/2007
}

pcarma_acvf2model <- function(acf, model, maxlag){
    model <- pcarma_prepare(model, type = 1)

    p <- model$p
    q <- model$q
    period <- model$period
     # si2 <- sVector(model$si2)  # TODO: vavedi standartisirano ime na si2 za tseliya paket!
    si2 <- model$si2     # vavedi standartisirano ime na si2 za tseliya paket!
    h <- model$h

    ## slozhi tuk dopalnitelna obrabotka!
    wrk <- pcarma_param_system(acf, h, si2, p, q, period)
    wrk$param <- solve(wrk$A, wrk$b)

    pcarma_unvec(wrk)
}
