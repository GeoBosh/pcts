test_that("class PeriodicInterceptSpec behaves",
{
    new("PeriodicInterceptSpec")

    new("PeriodicInterceptSpec", center = 3)
    new("PeriodicInterceptSpec", intercept = 2)
    new("PeriodicInterceptSpec", sigma2 = 1)

    new("PeriodicInterceptSpec", center = c(3.1, 3.2))
    new("PeriodicInterceptSpec", intercept = c(2.1, 2.2))
    new("PeriodicInterceptSpec", sigma2 = c(1,2))

    new("PeriodicInterceptSpec", center = 3, intercept = 2)
    new("PeriodicInterceptSpec", center = 3, sigma2 = c(1,2))
    new("PeriodicInterceptSpec", center = 3, intercept = 2, sigma2 = c(1,2))
    new("PeriodicInterceptSpec", center = c(3.1, 3.2), intercept = 2, sigma2 = c(1,2))
    new("PeriodicInterceptSpec", center = c(3.1, 3.2), intercept = 2)

    ## scalars are promoted to vectors, but othewise lengths must match
    expect_error(new("PeriodicInterceptSpec", center    = c(3.1, 3.2),
                                              intercept = c(2.1, 2.2, 2.3)))

    expect_error(new("PeriodicInterceptSpec", center    = c(3.1, 3.2),
                                              sigma2    = c(2.1, 2.2, 2.3)))

    ## argument 'nseasons': scalar arguments are promoted:
    new("PeriodicInterceptSpec", center = 3, intercept = 2, nseasons = 4)
    new("PeriodicInterceptSpec", center = 3, sigma2 = 2, nseasons = 4)

    ## ... but otherwise must match:
    new("PeriodicInterceptSpec", center = c(3.1, 3.2), nseasons = 2)
    expect_error(new("PeriodicInterceptSpec", center = c(3.1, 3.2), nseasons = 4))

    nSeasons(new("PeriodicInterceptSpec", center = c(3.1, 3.2), nseasons = 2))

})


test_that("constructors of periodic filter models are ok",
{
    m <- rbind(c(0.81, 0), c(0.4972376, 0.4972376))
    si2 <- PeriodicVector(c(0.3439000, 0.1049724))

    ar_filt3 <- new("PeriodicBJFilter",  coef =  m, order = c(1,2))
    ma_filt0 <- new("PeriodicSPFilter",   order = rep(0, 2))

    ## 2016-10-18 no such model now:
    ## wn0 <- new("PeriodicWhiteNoiseModel", scalesq = c(0.3439000, 0.1049724))

    armaA <- new("PeriodicArmaModel", ar = ar_filt3, ma = ma_filt0, sigma2 = c(0.3439000,0.1049724))

    nSeasons(armaA)
    class(armaA)

    autocovariances(armaA)
    autocovariances(armaA, maxlag = 5)


    armap0.spec <- new("PeriodicArmaModel", ar = ar_filt3, sigma2 = c(0.3439000, 0.1049724))
    armap0.spec[1,3]
    ## armaA[1, 3] # no subsetting for xxxModel

    autocovariances(armap0.spec)
    autocorrelations(armap0.spec)

    pfar <- new("PeriodicArModel", ar = ar_filt3, sigma2 = c(0.3439000, 0.1049724))

    pfar
    expect_equal_to_reference(autocovariances(pfar), "acv_pfar.RDS")
    expect_equal_to_reference(autocorrelations(pfar), "acf_pfar.RDS")

    ## autocorrelations(ar.spec)

})


