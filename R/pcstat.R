pcacfMat <- function(parmodel){
    p <- filterOrder(parmodel@ar) # was: arOrder(parmodel) # parmodel@order
    nseas <- nSeasons(parmodel) # parmodel@nseasons

    maxlag <- nseas + max( p - (1:nseas) )

    wrk <- pc.acf.parModel(parmodel, maxlag = maxlag)

    indx <- nseas : (nseas - maxlag)
    res <- wrk[indx, indx, type = "tt" ]   # lazy
    res
}

parcovmatlist <- function(parmodel, n, cor = FALSE, result = "list"){
    m <- pcacfMat(parmodel)

    p <- filterOrder(parmodel@ar) # arOrder(parmodel) # parmodel@order
    nseas <- nSeasons(parmodel)   # parmodel@nseasons
    sigma2 <- sigmaSq(parmodel)   #  parmodel@scalesq
    
    if(length(n) == 1)
        n <- rep(n / nseas, nseas)   # crude, is this necessary? should I subtract one
                                     # for estimation of mean? Doesn't make much difference.

    ## 2015-02-11 TODO: There is corrected version of McLeod's paper where
    ##                  gamma_{i-j,m} is replaced by gamma_{i-j,m-j} CHECK!
    matlist <- lapply(1:nseas,
                      function(x){
                          if(p[x] == 0){
                              matrix(NA, nrow = 0, ncol = 0)
                          }else{
                              start <- nseas + 1 - x
                              ind <- start + 0:(p[x] - 1)

                              mloc <- solve(  m[ind , ind] / sigma2[x] ) / n[x]
                              if(cor){
                                  d <- diag( (1 / sqrt(diag(mloc))) )
                                  mloc <- d %*% mloc %*% d
                              }
                              mloc
                          }
                      }
                      )
    if(identical(result, "list"))
        matlist
    else if(identical(result, "Matrix"))
        .bdiag(matlist)
    else
        as.matrix(.bdiag(matlist))
}

meancovmat <- function(parmodel, n, cor = FALSE, result = "var"){
    p <- filterOrder(parmodel@ar) # arOrder(parmodel) # parmodel@order
    nseas <- nSeasons(parmodel)   # parmodel@nseasons
    sigma2 <- sigmaSq(parmodel)   #  parmodel@scalesq

    fltco <- filterCoef(parmodel@ar) # was: arCoef(parmodel) # parmodel@coef

    fltco[is.na(fltco)] <- 0
    flt <- new("MultiFilter", coef = fltco)
    wrk <- mf_VSform(flt, form = "I", perm = 1:nseas)  # 2015-12-31 was: perm = 1:4

    if(length(n) == 1)
        n <- rep(n/nseas, nseas)   # TODO: crude, is this necessary? should I subtract one
                                   # for estimation of mean? Doesn't make much difference.

    varnoise <- wrk$Phi0inv %*% diag(sigma2) %*% t(wrk$Phi0inv)

    phi <- wrk$Phi
    ## phi <- array(phi, c(nrow(phi), nrow(phi), ncol(phi)/nrow(phi)))

    fac <- diag(rep(1, nrow(phi)))
    ifac <- 1:nseas
    for(i in seq(1, ncol(phi), by = nseas)){
        indx <- i - 1 + ifac

        if(indx[nseas] > ncol(phi)){ # last step, may be shorter if ncol(phi) i snot a
                                     # multiple of nseas
            indx <- i:ncol(phi)
            ifac <- 1:length(indx)
        }

        fac[ , ifac] <- fac[ , ifac] - phi[ , indx]
    }

    ifac <- solve(fac)
    covmat <- ifac %*% varnoise %*% t(ifac)  # Luetkepohl p. 84-5

    ## TODO: it doesn't make sense to have cor = TRUE  and result = "var"

    if(cor){ # 2019-12-15 new; TODO: check!
        d <- diag( (1 / sqrt(diag(covmat))) )
        d %*% covmat %*% d
    }else if(identical(result, "var"))
        diag(covmat) / n
    else
        covmat / n
}

## interesting to check with the above, this should be (almost?) equal to the diagonal of the
## above.
meanvarcheck <- function(parmodel, n){
    ## p <- parmodel@order
    nseas <- nSeasons(parmodel) # parmodel@nseasons

    if(length(n) == 1)
        n <- rep(n/nseas, nseas)   # crude, is this necessary? should I subtract one

    maxlag <- nseas * ceiling(max(n)) + 20 * nseas # arbitrary

    acv <- pc.acf.parModel(parmodel, maxlag = maxlag)

    res <- numeric(nseas)
    for(s in 1:nseas){
        alag <- (0:(n[s]-1)) * nseas   # alag <- seq(from=0, to = nseas*n[s], by=nseas)
        k <- 1 : (n[s]-1)

        res[s] <- sum(c(1, 2 * (1 - k / n[s])) * acv[s, alag]) / n[s]
    }
    res
}
