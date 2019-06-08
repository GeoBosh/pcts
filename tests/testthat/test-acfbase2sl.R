context("season-lag")

test_that("the conversions between `season-lag' and acf from `base' are ok",
{
    ##
    ## ?acf2ARMA
    ## ?ARMAacf
    ## ARMAacf(c(1.0, -0.25), 1.0, lag.max = 10)
    ## devtools::test()
    ## tacvfARMA(phi = 1.2, sigmasq = 1, maxLag = 5)
    ## ltsa::tacvfARMA(phi = 1.2, sigmasq = 1, maxLag = 5)
    ## ltsa::tacvfARMA(phi = 0.8, sigmasq = 1, maxLag = 5)
    ## (acf(lh))

    ############ autocovariances
    ##
    ## from examples for pcacfMat
    set.seed(1234)
    x <- arima.sim(list(ar=0.9), n=1000)
    mx <- matrix(x, nrow=4)

new("PeriodicBJFilter", coef = matrix(1:12,4), order = c(3,2,2,2))
new("PeriodicBJFilter", coef = matrix(1:12,4))
arfi <- new("PeriodicBJFilter",  order = c(3,2,2,2))


tmp.par <- new("PeriodicArmaModel", ar = arfi)
new("PeriodicArmaModel", ma = arfi)
new("PeriodicArmaModel", ar = arfi, sigma2 = 1:4)
new("PeriodicArmaModel", ma = arfi, sigma2 = 1:4)
new("PeriodicArmaModel", ar = arfi, ma = arfi, sigma2 = 1:4)


        ## TODO: make this work?
    ##     new("PeriodicArModel", order = c(3,2,2,2))
PeriodicArModel(c(3,2,2,2))

## 2017-06-04 was: expect_error(new("PeriodicArModel", tmp.par))
##            ARMA to AR  with MA order 0 is allowed;
##            (don't remember why it was an error before)
new("PeriodicArModel", tmp.par)


PeriodicArModel(tmp.par)

filterOrder(tmp.par@ma) # maOrder(tmp.par)

proba1 <- fitPM(c(3,2,2,2), as.numeric(mx))



proba1
acfb <- pc.acf.parModel(proba1, maxlag=8)
acfb[4:(-2), 4:(-2), type="tt"]

pcacfMat(proba1)

## autocovariances()
## for "parModel"

expect_equal_to_reference(autocovariances(proba1),  "acv_proba1a.RDS")
expect_equal_to_reference(autocorrelations(proba1), "acf_proba1a.RDS")
expect_equal_to_reference(autocovariances(proba1, 6), "acv_proba1a6.RDS")
expect_equal_to_reference(autocorrelations(proba1, 6),"acf_proba1a6.RDS")
})
