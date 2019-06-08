## 2013-10-21 removed some functions for inclusion in a separate package, gbutils
## 2016-01-30 was the last time stamp on the file before putting it under git

pc.arith.floor   <- function(x, period) x -  x %% period
pc.arith.ceiling <- function(x, period) x -  x %% period + if(x %% period != 0) period  else 0

toSeason <- function(t, period, t1 = 1, from = 1){
                                  # t - time (integer, may be vector) whose season is required
                                  # t1 - a time (integer) whose season is the first season
                                  # from - 1 if the numbering of seasons is 1,2,...,period
                                  #        0                                0,1,...,period-1
                                  #        other values - not admissible (but not checked)
    from + ((t-t1) %% period) # expects %% to give result in 0,1,...,period-1
}

toSeasonPair <- function(t, s, period, ...){              # for the moment t,s must be scalar!
    if(t >= s)
        list(season = toSeason(t, period, ...), lag = t - s)
    else
        list(season = toSeason(s, period, ...), lag = s - t)
}

ttmatToslPairs <- function(i, j, period){
    wrk <- matrix(NA, nrow = 0, ncol = 4)  # each row contains  four numbers representing
    for(k1 in 1:length(i)){                # a tt-pair and the corresponding sl-pair
       for(k2 in 1:length(j)){
          sp <- toSeasonPair(i[k1], j[k2], period)
          tmp <- c(k1, k2, sp$season, sp$lag)
          wrk <- rbind(wrk, tmp)
       }
    }
    wrk
}

ttTosl <- function(r, period){
    wrk <- matrix(NA, nrow = 0, ncol = 3)
    for(i in 1:nrow(r)){
        for(j in 1:ncol(r)){
            sp <- toSeasonPair(i, j, period)
            wrk <- rbind(wrk, c(sp$season, sp$lag, r[i,j]))
        }
    }
    colmax <- max(wrk[,2])
    res <- matrix(NA, nrow = period, ncol = colmax + 1) # +1 because lags start at zero
                                        #    flag <- 0
    for(i in 1:nrow(wrk)){
        val <- res[wrk[i,1], wrk[i,2] + 1]
        if(is.na(val)){  #+1 because lags start at zero
            res[wrk[i,1], wrk[i,2] + 1] <- wrk[i,3]
       }else{
         if(val != wrk[i,3])
           warning("ttTosl:  encountered different values of acf for a season-lag pair.")
       }
    }
    res
}
