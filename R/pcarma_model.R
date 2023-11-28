pcarma_prepare <- function(model,type){                    # currently harmonises only p,q,period
  p <- model$p                         # type is for future extentions
  q <- model$q
  period <- model$period

  if(is.null(p))
    p <- 0
  if(is.null(q))
    q <- 0

  if(is.null(period))
    period <- max(length(p),length(q))

  if(period <= 1)
    warning("period is equal to ",period,call.=TRUE)

  if(length(p)==1)
    p <- rep(p,period)
  if(length(q)==1)
    q <- rep(q,period)

  model$p <- p
  model$q <- q
  model$period <- period

  model
}

pcarma_unvec <- function(model){
  model <- pcarma_prepare(model,type=1)

  p <- model$p
  q <- model$q
  period <- model$period
  x <- model$param  # need unified name?

  res <- list(p=p,q=q)
  res$si2   <- numeric(period)
  if(max(p)>0)
    res$phi <- matrix(0,nrow=period,ncol=max(p))
  if(max(q)>0)
    res$theta <- matrix(0,nrow=period,ncol=max(q))

  icur <- 1
  for(t in 1:period){
    res$si2[t]        <- x[icur]
    if(p[t]>0)
      res$phi[t,1:p[t]] <- x[icur +1:p[t]]
    if(q[t]>0)
      res$theta[t,1:q[t]] <- x[icur+p[t] +1:q[t]]
    icur <- icur + 1+p[t]+q[t]
  }

  res
}

pcarma_tovec <- function(model){
  model <- pcarma_prepare(model,type=1)

  p <- model$p
  q <- model$q
  period <- model$period
  x <- model$param  # need unified name?

  # 2013-04-01 adding assignments for si2, phi, theta.
  #     todo: the function seems unfinished and probably never used!
  phi <- model$phi
  theta <- model$theta
  si2 <- model$si2  # todo: check - model$sigma2?

  res <- list(p=p,q=q)

  x <- numeric(0)
  for(t in 1:period){
    x <- c(x, si2[t], phi[t,seq_len(p[t])], theta[t,seq_len(q[t])])
  }
  res$param <- x

  res
}
