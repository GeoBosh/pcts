## todo: period = "integer"?
setClass("PeriodicVector", contains = "numeric",
         slots = c(period = "numeric"))

## TODO: method for initialize()?
## TODO: check `period' is ok (when present)?
PeriodicVector <- function(x, period = length(x)){
    if(missing(x))
        x <- rep(NA_real_, period)
    else if(length(x) != period)
        stop("length(x) must be equal to 'period'")
    new("PeriodicVector", x, period = period)
}

setMethod("[", signature(x = "PeriodicVector", i = "missing", j = "missing"),
    function (x, i, j, ..., drop = TRUE){
        ind <- 1 + ( (1:x@period - 1) %% x@period )
        x[ind]
    })

setMethod("[", signature(x = "PeriodicVector", i = "ANY", j = "missing"),
    function (x, i, j, ..., drop = TRUE){
        ind <- 1 + ( (i - 1) %% x@period )
        x@.Data[ind]
    })

## TODO: implement in C/C++
setMethod("[", signature(x = "PeriodicVector", i = "missing", j = "ANY"),      # x[i-j]
    function (x, i, j, ..., drop = TRUE){
        ## ## resOld is as in sVector, remove after testing
        ## i <- 1:x@period
        ## resOld <- matrix(NA, nrow = x@period, ncol = length(j))
        ## for(k in 1:length(j)){
        ##     ind <- 1 + ( ((i - j[k]) - 1) %% x@period )
        ##     resOld[ , k] <- x[ind]
        ## }
        ##
        ## ## alternatively (TODO: verify!)
        all.i <- rep(1:x@period, times = length(j))
        all.j <- rep(j, each = x@period)
        ##
        ## stopifnot(length(all.i) == length(all.j)) # for testing

        ind <- 1 + (all.i - all.j - 1) %% x@period
        #### another alternative for ind (but hardly more efficient)
        #### ind <- outer(ind, j, function(x, y, period) 1 + (x - y -1) %% period )

        res <- matrix(x[ind], ncol = length(j))

        ## stopifnot(identical(res, resOld))

        res
    })

           # todo: does it make sense to return a vector if drop = TRUE?
           #       (it makes but would need to change the default to FALSE for convenient use)
           # Note: in the old sVector class the format of the result depends on whether or
           #       not i is missing! This is almost certainly an error but check how it was
           #       used (on the right-hand side of expression matrix or vector usually
           #       doesn't matter).
setMethod("[", signature(x = "PeriodicVector", i = "ANY", j = "ANY"),      # x[i-j]
    function (x, i, j, ..., drop = TRUE){
        ## ## resOld is as in sVector, remove after testing
        ## i <- 1:x@period
        ## resOld <- matrix(NA, nrow = x@period, ncol = length(j))
        ## for(k in 1:length(j)){
        ##     ind <- 1 + ( ((i - j[k]) - 1) %% x@period )
        ##     resOld[ , k] <- x[ind]
        ## }
        ##
        ## ## alternatively (TODO: verify!)
        all.i <- rep(i, times = length(j))
        all.j <- rep(j, each = length(i))

        ## stopifnot(length(all.i) == length(all.j)) # for testing

        ind <- 1 + (all.i - all.j - 1) %% x@period
        res <- matrix(x[ind], ncol = length(j))

        ## stopifnot(identical(res, resOld))

        res
    })

setReplaceMethod("[", signature(x = "PeriodicVector", i = "missing"),
    function(x, i, j, ..., value){ # todo: drop "j" from the signature to avoid confusion?
        x@.Data[] <- value   # equivalent to: x[1:x@period] <- value  (todo: check)
        x
    })

setReplaceMethod("[", signature(x = "PeriodicVector", i = "ANY"),
    function(x, i, j, ..., value){
        i <- 1 + ( (i - 1) %% x@period )
        x@.Data[i] <- value
        x
    })
