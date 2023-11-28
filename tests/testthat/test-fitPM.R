context("fitPM")

test_that("test fitPM()",
{
    set.seed(1234)

    x <- arima.sim(list(ar = 0.9), n = 1000)
    mx <- matrix(x, nrow = 4)
    x_pcts <- pcts(as.numeric(x), nseasons = 4)

    expect_error(fitPM(c(1.5, 2, 3, 1), x), "The PAR orders must be non-negative integer numbers")
    expect_error(fitPM("dummy", x), "unable to find an inherited method for function 'fitPM'")
    expect_error(fitPM(c(3,2,2,2), mx), "multivariate PAR fitting not implemented yet")

    proba1 <- fitPM(c(3, 2, 2, 2), as.numeric(mx))
    expect_equal_to_reference(proba1, "proba1.RDS")
    expect_output(show(proba1))
    expect_output(summary(proba1))

    expect_error(fitPM(2, mx),
                 ## "unable to find an inherited method for function [.]nSeasons[.] for signature [.]\"matrix\"[.]"
                                  "unable to find an inherited method for function"
                 )
    expect_identical(fitPM(2, x_pcts), fitPM(c(2, 2, 2, 2), as.numeric(mx)))

    eps.proba1 <- residuals(proba1)
    expect_identical(eps.proba1, pcts:::.whiten(proba1))

    eps.proba1[1:2] <- NA
    dim(eps.proba1) <- dim(mx)
    pc.cconesidedsum(mx, eps.proba1, maxlag = 4)
    ## estimate h_{t,i}, see Boshnakov (1996)
    pc.hat.h(mx, eps.proba1, maxlag = 4)
   
    ## replacing with equivalent code using the new Fraser2017
    ##     data(Fraser, package = "pear")
    Fraser <- window(Fraser2017, start = c(1912, 3), end = c(1990, 12))
    
    logFraser <- log(Fraser)
    ## TODO: for now I need whole years;
    ##    !!! However note the typo 'logfraser', the following use 'logFraser'!
    logfraser <- ts(logFraser[1:936], frequency = 12)

    #### co1_pear <- pear::pear(logFraser, 1)[["phi"]]
        # fitPM(as.numeric(logFraser), order = rep(1, 12), period = 12, seasonof1st = 3)
    az1 <- fitPM(model = rep(1, 12), as.numeric(logFraser), seasonof1st = 3)
    az2 <- fitPM(model = rep(1, 12), as.numeric(logFraser))
    #### expect_true(all.equal(as.vector(az1@ar@coef[ , 1]), as.vector(co1_pear[ , 1])))
    #### expect_true(all.equal(as.vector(az2@ar@coef[ , 1]), as.vector(co1_pear[ , 1])[c(3:12, 1:2)]))

    ## pcfr2  <- pcts(dataFranses1996[ , 2  ])
    pcfr23 <- pcts(dataFranses1996[ , 2:3])
    expect_error(fitPM(model = rep(1, 4), pcfr23), "Multivariate case not implemented yet")

    ## fitPM(model = rep(1, 4), pcfr23[1]) # tests the method for PeriodicMTS ([] keep MTS class)
    ## fitPM(model = rep(1, 4), pcfr23[[1]]) # tests the method for PeriodicTS ('[[' drops the 'M')
    expect_identical(fitPM(model = rep(1, 4), pcfr23[1]),
                     fitPM(model = rep(1, 4), pcfr23[[1]]))


    x <- arima.sim(list(ar = 0.9), n = 960)
    pcx <- pcts(x, nseasons = 4)
    mx <- matrix(x, nrow = 4)

    pfm1 <- PeriodicArModel(matrix(1:12, nrow = 4), order = rep(3,4), sigma2 = 1)
    sipfm1 <- new("SiPeriodicArModel", iorder = 1, siorder = 1, pcmodel = pfm1)
    fitPM(sipfm1, mx)

    expect_output(show(sipfm1))
    

    d4piar2  <- rbind(c(1,0.5,-0.06), c(1, 0.6, -0.08), c(1, 0.7, -0.1), c(1, 0.2, 0.15))
    picoef1  <- c(0.8, 1.25, 2, 0.5)
    parcoef1 <- d4piar2[, 2:3]
    coef1    <- pi1ar2par(picoef1, parcoef1)
    tmpval <- PeriodicArModel(parcoef1)
    ##pipfm    <- PiParModel(piorder = 1, picoef = picoef1, par = tmpval)
    pipfm    <-  new("PiPeriodicArModel", piorder = 1,
                     picoef = matrix(picoef1, ncol = 1), pcmodel = tmpval)

    expect_output(show(pipfm))

    perunit  <- sim_pc(list(phi = coef1, p = 3, q = 0, period = 4),500)
    fitPM(pipfm, perunit)

    ## temporary
    proba1x <- new("FittedPeriodicArmaModel", as(proba1, "PeriodicArmaModel"),
                   theTS = proba1@theTS, ns = proba1@ns, asyCov = proba1@asyCov)    
    expect_identical(residuals(proba1x), residuals(proba1))
    
    expect_error(as_pcarma_list(1:10),
                 "unable to find an inherited method for function 'as_pcarma_list'")

    expect_output(show(proba1x))
    fitted(proba1x)
    predict(proba1x, 1)
    predict(proba1x, 8)

    n <- 100
    x <- arima.sim(list(ar=0.9), n = n)
    proba1 <- fitPM(c(3,2,2,2), x)

    meancovmat(proba1, n/10)
    meancovmat(proba1, n/10, cor = TRUE)
    meancovmat(proba1, n/10, result = "")
    meancovmat(proba1, n/10, cor = TRUE, result = "")

    meanvarcheck(proba1, 100)
})


test_that("test mC.ss() works",
{
    pcts_exdata()
    ## examples from mC.ss.Rd
# test0 roots
spec.coz2 <- mcompanion::mcSpec(dim = 5, mo = 4, root1 = c(1,1), order = rep(2,4))
spec.coz2
xxcoz2a <- mC.ss(spec.coz2)

## test0 roots
spec.coz4 <- mcompanion::mcSpec(dim = 5, mo = 4, root1 = c(1,1), order = rep(3,4))
xxcoz4a <- mC.ss(spec.coz4)

    ## excerpt from 
    ## ~/Documents/Rwork/pctsExperiments/Rsessions/combined upto 2013-12-31 17h36m.Rhistory
spec.co2 <- mcompanion::mcSpec(dim = 5, mo = 4, siorder = 1)
tmp2 <- mC.ss(spec.co2)
## only two iters for testthat
expect_output(mc.res1ssenv2b <- tmp2$env$minimBB(nsaauto, control=list(maxit=2)))

expect_output(tmp2$env$minimBB(nsaauto, control=list(maxit=2)))
expect_output(tmp2$env$minimBBlu(nsaauto, control=list(maxit=2)))
expect_output(tmp2$env$minimBB(nsaauto, control=list(maxit=2), CONDLIK = FALSE))
tmp2$env$minim(nsaauto, control=list(maxit=2))
tmp2$env$minim(nsaauto, control=list(maxit=2), CONDLIK = FALSE)
expect_output(tmp2$env$minimBB(nsaauto, control=list(maxit=2), CONDLIK = FALSE))
mC.ss(spec.co2, generators = TRUE)
    
    tmp2$env$mcparam2optparam()
    tmp2$env$mcsigma2(nsaauto)
    tmp2$env$mcsigma2(nsaauto, tmp2$env$mcparam2optparam())
    
mC.ss(spec.co2, init = tmp2$env$mcparam2optparam())    

    ## this chunk was commented out in mC.ss.Rd, old testing with it.
##  > xxco.1 <- mC.ss(m1.new, generators = TRUE)
## 
##  > datansa <- read.csv("nsadata.csv")
##  > nsaauto <- ts(datansa$AUTOMOTIVEPRODNSA[113:328], start=c(1947, 1), frequency=4)
## 
##  > res.xxco.1 <- xxco.1$env$minimBB(nsaauto, control=list(maxit=1000))
## 
##  condlik is:  32.85753	persd is:  16.96771 10.40725 3.567698 7.426556
##  iter:  0  f-value:  32.85753  pgrad:  14.83674
##  iter:  10  f-value:  30.21297  pgrad:  0.0007615952
##    Successful convergence.
## 
##  > res.xxco.1$value
##  [1] 30.21297
##  > res.xxco.1$par
##       co.r1      co.r2      co.r3      co.r4
##  -0.4069477 -0.5093360 -0.6026860 -0.5174826
##  > res.xxco.1
##  $par
##       co.r1      co.r2      co.r3      co.r4
##  -0.4069477 -0.5093360 -0.6026860 -0.5174826
## 
##  $value
##  [1] 30.21297
## 
##  $gradient
##  [1] 9.023893e-06
## 
##  $fn.reduction
##  [1] 2.644559
## 
##  $iter
##  [1] 14
## 
##  $feval
##  [1] 16
## 
##  $convergence
##  [1] 0
## 
##  $message
##  [1] "Successful convergence"
## 
##  $cpar
##  method      M
##       2     50
## 
##  > with(xxco.1$env, model)
##  $period
##  [1] 4
## 
##  $p
##  [1] 5
## 
##  $q
##  [1] 0
## 
##  $phi
##            [,1]          [,2]          [,3] [,4]       [,5]
##  [1,] 1.1646497 -1.165471e-16 -4.254923e-17    1 -1.1646497
##  [2,] 0.8451102 -2.220446e-16 -5.456035e-17    1 -0.8451102
##  [3,] 0.7989768  0.000000e+00  2.220446e-16    1 -0.7989768
##  [4,] 1.2716195 -1.110223e-16 -6.058867e-17    1 -1.2716195
## 
##  > with(xxco.1$env, zapsmall(model$phi))
##            [,1] [,2] [,3] [,4]       [,5]
##  [1,] 1.1646497    0    0    1 -1.1646497
##  [2,] 0.8451102    0    0    1 -0.8451102
##  [3,] 0.7989768    0    0    1 -0.7989768
##  [4,] 1.2716195    0    0    1 -1.2716195

    set.seed(1234)
    
    ## This prints something like:
    ##     condlik is:  18.09375	persd is:  1.651785 1.714789 3.041003 1.577415 
    fitPM(spec.coz4, rnorm(100), control = list(maxit = 1))    
    
})
