test_that("constructors of periodic AR filters are ok",
{
    ar_filt0 <- new("PeriodicBJFilter") # functions in package "methods" sometimes use this.
    expect_identical(nSeasons(ar_filt0), 0L)
    expect_identical(ar_filt0@coef, matrix(NA_real_, 0, 0))
    expect_equal(ar_filt0@order, numeric(0))

    ar_filt1 <- new("PeriodicBJFilter", order = rep(0L, 4))
    expect_identical(nSeasons(ar_filt1), 4L)
    expect_identical(ar_filt1@coef, matrix(NA_real_, 4, 0))
    expect_equal(ar_filt1@order, rep(0L, 4))

    ar_filt2 <- new("PeriodicBJFilter", order = rep(2L, 4))
    expect_identical(nSeasons(ar_filt2), 4L)
    expect_true(all(is.na(ar_filt2@coef)))
    expect_equal(ncol(ar_filt2@coef), 2)
    expect_equal(ar_filt2@order, rep(2L, 4))

    m <- rbind(c(0.81, 0), c(0.4972376, 0.4972376))
    ar_filt3 <- new("PeriodicBJFilter",  coef =  m, order = c(1,2))
    expect_identical(nSeasons(ar_filt3), 2L)
    expect_equal(ar_filt3@coef, m)
    expect_identical(ar_filt3@order, c(1,2))

    expect_identical(filterCoef(ar_filt3), ar_filt3@coef)
    expect_identical(filterOrder(ar_filt3), ar_filt3@order)
    expect_identical(filterPolyCoef(ar_filt3              ), cbind(1, - ar_filt3@coef))
    expect_identical(filterPolyCoef(ar_filt3, lag_0 = TRUE), cbind(1, - ar_filt3@coef))
    expect_identical(filterPolyCoef(ar_filt3, lag_0 = FALSE), - ar_filt3@coef)

    expect_output(show(ar_filt3))

    arma_filt3 <- new("PeriodicArmaFilter", ar = ar_filt3)
    expect_output(show(arma_filt3))
    maxLag(arma_filt3)
    ## for now, just run to ensure they do not throw error:
    filterPoly(ar_filt3)
    filterPoly(as(ar_filt3, "PeriodicSPFilter"))

    dummy <- new("PeriodicArFilter")
    dummy <- new("PeriodicMaFilter")


})

test_that("constructors of periodic MA filters are ok",
{
    ma_filt0 <- new("PeriodicSPFilter") # functions in package "methods" sometimes use this.
    expect_identical(nSeasons(ma_filt0), 0L)
    expect_identical(ma_filt0@coef, matrix(NA_real_, 0, 0))
    expect_equal(ma_filt0@order, numeric(0))

    ma_filt1 <- new("PeriodicSPFilter", order = rep(0L, 4))
    expect_identical(nSeasons(ma_filt1), 4L)
    expect_identical(ma_filt1@coef, matrix(NA_real_, 4, 0))
    expect_equal(ma_filt1@order, rep(0L, 4))

    ma_filt2 <- new("PeriodicSPFilter", order = rep(2L, 4))
    expect_identical(nSeasons(ma_filt2), 4L)
    expect_true(all(is.na(ma_filt2@coef)))
    expect_equal(ncol(ma_filt2@coef), 2)
    expect_equal(ma_filt2@order, rep(2L, 4))

    m <- rbind(c(0.81, 0), c(0.4972376, 0.4972376))
    ma_filt3 <- new("PeriodicSPFilter",  coef =  m, order = c(1,2))
    expect_identical(nSeasons(ma_filt3), 2L)
    expect_equal(ma_filt3@coef, m)
    expect_identical(ma_filt3@order, c(1,2))

    expect_identical(filterCoef(ma_filt3), ma_filt3@coef)
    expect_identical(filterOrder(ma_filt3), ma_filt3@order)
    expect_identical(filterPolyCoef(ma_filt3              ), cbind(1, ma_filt3@coef))
    expect_identical(filterPolyCoef(ma_filt3, lag_0 = TRUE), cbind(1, ma_filt3@coef))
    expect_identical(filterPolyCoef(ma_filt3, lag_0 = FALSE), ma_filt3@coef)


})

## test_that("constructors of periodic ARMA filters are ok",
## {
##
##    TODO:
##
## })
