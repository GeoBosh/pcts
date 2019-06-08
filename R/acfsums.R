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

## 2018-10-20 see also pc.mean()
calc_permean <- function(x, period, na.rm = TRUE, ...){
    stop("Not ready")
}

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
        wrkmean <- pc.mean(x)
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

pc_sum <- function(x, period) {
    rowSums(pc_matrix(x, period))
}

pc.mean <-
function(x, na.rm = TRUE, ...){
  apply(x, 1, mean, na.rm = na.rm, ...)
}

pc.center <-
  function(x, xbar = NULL, ...){
    if(is.null(xbar))
      xbar <- pc.mean(x, ...)
    x - xbar
  }

pc.acsum <-
function(x, maxlag = ncol(x)/4){
  nm1 <- length(x)-1
  res <- matrix(NA, nrow = nrow(x), ncol = maxlag+1)
  xlagged <- x

  res[, 1] <- apply( x*xlagged, 1, function(x) sum(x, na.rm = TRUE) )
  for(k in seq_len(maxlag) ){    # if(maxlag>0) for(k in 1:maxlag ){
    xlagged[] <- c(0, xlagged[1:nm1])        # prepend a 0 and drop the last element of xlagged
                                            # xlagged[] is used on the left-hand side
                                            #           to keep xlagged a matrix.
                                            # x1 x2 x3 ... xnm1 xn
                                            #  0 x1 x2 ... xnm2 xnm1
                                            #  0  0 x1 ... xnm3 xnm2

                                                          # multiply elwise and find row sums.
    res[, k+1] <- apply( x*xlagged, 1, function(x) sum(x, na.rm = TRUE) )
  }
  res
}

pc.cconesidedsum <-
function(x, y, maxlag = ncol(x)/4){                   # assume x and y of equal dim, positive lags
  nm1 <- length(x)-1
  res <- matrix(NA, nrow = nrow(x), ncol = maxlag+1)
  ylagged <- y

  res[, 1] <- apply( x*ylagged, 1,  function(x) sum(x, na.rm = TRUE) )
  for(k in seq_len(maxlag) ){
      ylagged[] <- c(0, ylagged[1:nm1])                              # see comments in pc.acsum
      res[, k+1] <- apply( x*ylagged,  1,  function(x) sum(x, na.rm = TRUE) )
    }
  res
}

pc.ccsum <-
function(maxlag, xlist){
  nx <- length(x)
  period <- nrow(xlist[[1]])
  res <- array(NA, dim = c(nx, nx, period, maxlag+1))

  for(i in 1:nx){
    x <- xlist[[i]]
    for(j in 1:nx){
      y <- xlist[[j]]
      res[i, j, , ] <- pc.cconesidedsum(x, y, maxlag)    # res[i,j,,] <- laggedcrosssum(x,y,maxlag)
    }
  }
  res
}

pc.sdfactor <-
  function(sd, maxlag){
    d <- length(sd)
    revsd <- rev(sd)
    wrk <- matrix(NA, nrow = d, ncol = d)                        # prepare a d x d block of factors,
    for(k in d:1){
      wrk[k, ] <- revsd[1]*revsd
      revsd <- shiftleft(revsd)
    }
             # now we create [wrk wrk ...] to create n x d  matrix
             # matrix()  may give a warning that the size of wrk is not a sub-multiple of that
             # of the result. This is OK since maxlag+1 is  not necessarilly multiple of d.
             # To avoid the message, we create a larger marix if necessary and subset it.
    n <- pc.arith.ceiling(maxlag+1, d)    # guarantee that n is a multiple of d and >= maxlag+1

    res <- matrix(wrk, nrow = d, ncol = n) # wrk is recycled as many times as necessary by matrix()
    res[ , 1:(maxlag+1)]
}

pc.acrftoacf <-            # computes pcacf from autocorrelation matrix and
  function(acrf, sd){       # standard deviations of the seasons (ie, sqrt(R_1(0),..., R_d(0)))
    maxlag <- ncol(acrf)-1
    res <- acrf * pc.sdfactor(sd, maxlag)
    res
  }

pc.acf <- function(x, maxlag, nyear = ncol(x), demean = TRUE){
    ## TODO: need tests targeted for the different possibilities here.
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
    pc.acsum(if(demean) pc.center(x) else x, maxlag) / nyear
}

pc.var <-           # didn't center, change all current calls of pc.var to have demean=FALSE!
function(x, ...){
  wrk <- pc.acf(x, maxlag = 0, ...)
  as.vector(wrk)
}


pc.acrf <-
function(x, maxlag, ...){
  res <- pc.acf(x, maxlag, ...)
  sd  <- sqrt(res[, 1])                           # compute standard deviations from R_k(0)
  wrk <- pc.sdfactor(sd, maxlag)                    # prepare the normalising factors
  res <- res / wrk
  res
}

pc.ccf <-                                       # arguments of pc.ccf do not seem natural.
function(maxlagx, nyear, demean = TRUE, ...){                   # allow demean to be a vector?
  xlist <- list(...)
  if(demean)
    for( i in 1:length(xlist) )
      xlist[[i]] <- pc.center(xlist[[i]])
  res <- pc.ccsum(maxlagx,  xlist) / nyear
  res
}

pc.hat.h <-
function(x, eps, maxlag, si2hat){
  period <- nrow(x)
  n <- ncol(x)
  if(missing(si2hat))
    si2hat <- pc.var(eps)

  res <- pc.cconesidedsum(x, eps, maxlag)/n
  for(k in 0:(period-1)){
    res[, seq(k+1, maxlag+1, by = period)] <-
      res[, seq(k+1, maxlag+1, by = period)] / shiftright(si2hat, k)
  }
  res
}

pc.argmin <-
  function(x){
    apply(x, 1, function(x) rank(x, ties.method = "first") )
  }

pc.par.argmin <-
  function(aic){    # aic; may be bic, etc?
    pc.argmin(aic) - 1    # subtract 1 because the the first el. correspond to p_k=0
  }
