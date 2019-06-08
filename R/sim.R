                                        # 13/05/2007 moved from mcompanion
                                        # 2018-10-18 renamed from mCpar to sim_parCoef
sim_parCoef <- function(period, n.root, sigma2 = rep(1,period), ...){
  wrk <- sim_pcfilter(period, n.root, ...)
  res <- list(ar = cbind(rep(1,period), wrk$pcfilter), sigma2 = sigma2)
  res
}
                                        # 2016-01-27 renamed  rAracf to sim_arAcvf and debugged
                                        # 2018-10-18 renamed  sim_arAcvf to sim_parAcvf
sim_parAcvf <- function(period, order, sigma2){
  mo <- if(missing(sigma2))
           sim_parCoef(period, order)
        else
           sim_parCoef(period, order, sigma2)

  wrk <- pcarma_acvf_system(slMatrix(mo$ar), NULL, mo$sigma2, order, 0, period)
  sol <- solve(wrk$A, wrk$b)

  acv <- matrix(sol, nrow = period)
  ## 2016-08-03 was: res <- new("PeriodicAutocovariance", acv = acv)
  ## 2018-10-19 was: res <- new("PeriodicAutocovarianceSimple", pcmatrix = acv)
  res <- acv

  ## 2018-10-18: temporary patch
  mo$order <- order
  attr(res, "model") <- mo

  res
}
