num2pcpar <- function(x, order, result = NULL, ...){
    sampleacvf <- calc_peracf(x, maxlag = max(order), what = "cov", ...)
    wrkmean <- attr(sampleacvf, "mean")

    comat <- calc_predictionCoefficients(sampleacvf, order)
    co <- slMatrix(comat)
    fv <- alg1(sampleacvf, order)$fv

    scalesq <- fv[ , ncol(fv)]

    if(is.null(result))
        list(mean = wrkmean, coef = co, scale = sqrt(scalesq))
    else# 2015-02-11: return PAR coefficients if 'result' is not NULL
        co
}

## 2018-10-20 see also pc.mean(), 2020-06-10: pc_mean()
## calc_permean <- function(x, period, na.rm = TRUE, ...){
##     stop("Not ready")
## }

calc_peracf <- function(x, maxlag, period, mean = TRUE, seasonof1st = 1, what = "cor"){
    if(is.matrix(x)){ # TODO: more general test to allow other type of matrix?
        r <- 0
    }else{
        if(seasonof1st > 1)
            x <- c(rep(NA_real_, seasonof1st - 1), x)

        r <- length(x) %% period
        if(r > 0)
            x <- c(x, rep(NA_real_, period - r)) # x <- c(x, numeric(period - r))

        x <- matrix(x, nrow = period)
    }

    if(isTRUE(mean)){

        ## NOTE: pc.mean() assumed 'matrix' for 'x' and had na.rm = TRUE since the above code
        ##     potentially inserts NA's, it is not really possible to have option na.rm =
        ##     FALSE without changing the code.

        wrkmean <- pc_mean(x, period, na.rm = TRUE) # 2020-06-10 was: pc.mean(x)
        x <- x - wrkmean
        if(r > 0) # restore the padded zeros
            x[(r+1):period, ncol(x)] <- NA ## TODO: or zero?
    }else if(is.numeric(mean)){
        wrkmean <- mean
        x <- x - mean
        if(r > 0)
            x[(r+1):period, ncol(x)] <- NA ## TODO: or zero?
    }else if(identical(mean, FALSE)){
        wrkmean <- 0
    }else{# 2015-02-11 was: wrkmean <- NULL; demean <- TRUE
        stop("Invalid argument 'mean'")
    }

    res <- if(what == "cor")
               pc.acrf(x, maxlag = maxlag, demean = FALSE)
           else
               pc.acf(x, maxlag = maxlag, demean = FALSE)
    attr(res, "mean") <- wrkmean
    res
}

pc_matrix <- function(x, period) {
    r <- length(x) %% period
    if(r != 0)
        x <- c(x, numeric(period - r))
    matrix(x, nrow = period)
}

pc_sum <- function(x, period, na.rm = FALSE) {
    ## was just this line: rowSums(pc_matrix(x, period))
    r <- length(x) %% period
    if(r != 0)
        x <- c(x, numeric(period - r))
    .rowSums(x, period, length(x) / period, na.rm)
}

pc_mean <- function(x, period, na.rm = FALSE) {
    r <- length(x) %% period
    if(r == 0)
        return( .rowMeans(x, period, length(x) / period, na.rm) )

    ## r != 0
    ## TODO: not sure if splitting into two cases below is worth it.
    ##       Maybe use unconditionally the 'else' part.
    if(na.rm || all(is.finite(x))){
        ## the case after '||' can also be handled with na.rm = TRUE.
        x <- c(x, rep_len(NA_real_, period - r))  # numeric(period - r)
        .rowMeans(x, period, length(x) / period, na.rm = TRUE)
    }else{ # here na.rm = FALSE, default for 'mean', so not passed to mean() below
        n <- length(x)
        sapply(1:period,
               function(i)
                   base::mean.default(x[seq(from = i, to = n, by = period)]) )
    }
}

pc.acsum <- function(x, maxlag = ncol(x)/4){
    nm1 <- length(x) - 1
    nr <- nrow(x)
    nc <- ncol(x)
    
    xlagged <- x

    res <- matrix(NA_real_, nrow = nr, ncol = maxlag + 1)
                   # 2002-06-10 was: apply( x * xlagged, 1, function(x) sum(x, na.rm = TRUE) )
    res[ , 1] <- .rowSums(x * xlagged, nr, nc, na.rm = TRUE)
    for(k in seq_len(maxlag) ){
        xlagged[] <- c(0, xlagged[1:nm1])   # prepend a 0 and drop the last element of xlagged
                                            # xlagged[] is used on the left-hand side
                                            #           to keep xlagged a matrix.
                                            # x1 x2 x3 ... xnm1 xn
                                            #  0 x1 x2 ... xnm2 xnm1
                                            #  0  0 x1 ... xnm3 xnm2

                                                          # multiply elwise and find row sums.
                    # 2002-06-10 was: apply(x * xlagged, 1, function(x) sum(x, na.rm = TRUE) )
        res[ , k + 1] <- .rowSums(x * xlagged, nr, nc, na.rm = TRUE)
    }

    res
}

pc.cconesidedsum <- function(x, y, maxlag = ncol(x)/4){
    ## assume x and y of equal dim, positive lags
    nm1 <- length(x) - 1
    nr <- nrow(x)
    nc <- ncol(x)

    stopifnot(all(dim(x) == dim(y)))

    ylagged <- y
    
    res <- matrix(NA, nrow = nr, ncol = maxlag + 1)
                   # 2002-06-10 was: apply( x*ylagged, 1,  function(x) sum(x, na.rm = TRUE) )
    res[ , 1] <- .rowSums(x * ylagged, nr, nc, na.rm = TRUE)
    for(k in seq_len(maxlag) ){
        ylagged[] <- c(0, ylagged[1:nm1])                           # see comments in pc.acsum

                 # 2002-06-10 was: apply( x * ylagged,  1,  function(x) sum(x, na.rm = TRUE) )
        res[ , k + 1] <- .rowSums(x * ylagged, nr, nc, na.rm = TRUE)
    }
    res
}

pc_sdfactor <- function(sd, maxlag){
    d <- length(sd)
    revsd <- rev(sd)
    wrk <- matrix(NA, nrow = d, ncol = d)                  # prepare a d x d block of factors,
    for(k in d:1){
        wrk[k, ] <- revsd[1]*revsd
        revsd <- shiftleft(revsd)
    }
             # now we create [wrk wrk ...] to create n x d  matrix
             # matrix()  may give a warning that the size of wrk is not a sub-multiple of that
             # of the result. This is OK since maxlag+1 is  not necessarilly multiple of d.
             # To avoid the message, we create a larger marix if necessary and subset it.
    n <- pc.arith.ceiling(maxlag + 1, d) # guarantee that n is a multiple of d and >= maxlag+1

    res <- matrix(wrk, nrow = d, ncol = n)  # recycle wrk as many times as necessary
        # 2020-06-11: use drop = FALSE to get a matrix also when maxlag = 0.
        #     This has the desirable effect that m / pc_sdfactor( ..., maxlag = 0)
        #     if m has more than one column, which almost certainly indicates programming 
        #     error. Before this change the vector returned by pc_sdfactor would be recycled.
    res[ , 1:(maxlag + 1), drop = FALSE]
}

pc.sdfactor <- function(...){
    .Deprecated(msg = "pc.sdfactor will be removed in pcts v1.0.0, please use 'pc_sdfactor'")
    pc_sdfactor(...)
}


## computes pcacf from autocorrelation matrix and standard deviations of the seasons,
## ie sqrt(R_1(0),..., R_d(0)))
pc.acrftoacf <- function(acrf, sd){
    maxlag <- ncol(acrf) - 1
    res <- acrf * pc_sdfactor(sd, maxlag)
    res
}

pc.acf <- function(x, maxlag, nyear = ncol(x), demean = TRUE){
    if(missing(nyear)){ # 2018-10-25 new; default for nyear was: nyear = ncol(x)
        if(anyNA(x)){
            xna <- is.na(x)   
            a <- match(FALSE, xna)      # first nonNA in x
            if(is.na(a))
                stop("No non-missing values in x!")
            b <- match(FALSE, rev(xna)) # last  nonNA in x (counted from the end)
            m <-  a + b - 2L # == (a-1) + (b-1)
            nyear <- ncol(x) - (m %/% nrow(x))
            if(nyear < 2) # TODO: maybe require more than 2?
                stop("Too few non-missing values in x")
        }else
            nyear <- ncol(x)
    }
    ## 2020-06-10 was: pc.acsum(if(demean) pc.center(x) else x, maxlag) / nyear
    pc.acsum(if(demean) x - pc_mean(x, nrow(x), na.rm = TRUE) else x, maxlag) / nyear
}

                     # didn't center, change all current calls of pc.var to have demean=FALSE!
pc.var <- function(x, ...){
    wrk <- pc.acf(x, maxlag = 0, ...)
    as.vector(wrk)
}

pc.acrf <- function(x, maxlag, ...){
    res <- pc.acf(x, maxlag, ...)
    sd  <- sqrt(res[, 1])                           # compute standard deviations from R_k(0)
    wrk <- pc_sdfactor(sd, maxlag)                    # prepare the normalising factors
    res <- res / wrk
    res
}

pc.hat.h <- function(x, eps, maxlag, si2hat){
    period <- nrow(x)
    n <- ncol(x)
    if(missing(si2hat))
        si2hat <- pc.var(eps)

    res <- pc.cconesidedsum(x, eps, maxlag)/n
    for(k in 0:(period-1)){
        ind <- seq(k + 1, maxlag + 1, by = period)
        ind2 <- seq(k + 1, maxlag + 1, by = period)
        stopifnot(all(ind == ind2))
        res[ , ind] <- res[ , ind] / shiftright(si2hat, k)
    }
    res
}

pc_apply <- function(x, period, FUN, ...) {
    r <- length(x) %% period
    if(r == 0){
        m <- matrix(x, nrow = period)
        apply(m, 1, FUN, ...)
    }else{
        n <- length(x)
        sapply(1:period,
               function(i)
                   apply(x[seq(from = i, to = n, by = period)]), 1, FUN, ... )
    }
}
