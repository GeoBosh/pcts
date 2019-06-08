                                                    ### test functions from before ppackaging
ex1f <-
function(t,s){
  if(t==s)
    return(1)

  if(t>s){
    tmp <- t
    t <- s
    s <- tmp
  }

  while(t<=0){
    t <- t+2
    s <- s+2
  }

  while(t>2){
    t <- t-2
    s <- s-2
  }

  season <- t %% 2
  q <- s %% 2
  i <- s %/% 2


  if( t == 1  && s %% 2 == 1 )
    res <- 0.9^(i+2)
  else if( t == 2  && s %% 2 == 1 )
    res <- 0.9^{i+1}
  else if( t == 2  && s %% 2 == 0 )
    res <- 0.9^(i-1)
  else if( t == 1  && s %% 2 == 0 )
    res <- 0.9^i

  res
}

## 2016-01-02 commenting out
##
## fibo <-
##   function(k){
##     if(k<2)
##       return(1)
##     else
##       return( fibo(k-1)+fibo(k-2) )
##   }
##
## m1 <- rbind( c(1,0.81,0),c(1, 0.4972376, 0.4972376) )
## m2 <- rbind( c(1,0,0),c(1,0,0) )
## testvar <- c(1,1)
