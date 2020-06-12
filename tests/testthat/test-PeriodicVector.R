context("PeriodicVector")

test_that("PeriodicVector class is ok", {
    x <- PeriodicVector(1:4)

    expect_equal(PeriodicVector(1:4, period = 4), x)
    expect_identical(PeriodicVector(1:4, period = 4L), x)

    expect_equal(x[], 1:4)
    expect_identical(PeriodicVector(period = 4)[], rep(NA_real_, 4))

    expect_error(PeriodicVector(1:3, period = 4)) ## since length(x) != period:

    expect_identical(x[1:4], x[5:8])

    ## "[<-" works on the underling vector
    y <- x
    y[1] <- 11
    expect_equal(y[1], 11)
    expect_equal(y[5], 11)

    ## modulo indexing works also in assignments:
    y[5] <- 21
    expect_equal(y[1], 21)
    expect_equal(y[5], 21)

    ## empty index returns the underlying vector
    identical(x[], x@.Data)
    expect_equal(x[2,1], matrix(x[2 - 1], nrow = 1)) # roughly, x[i - j]
    expect_identical(x[1:4, 2], x[ , 2])

    ## the recycling rule applies on assignment
    y[] <- 9
    expect_equal(y[], rep(9, y@period))

    y[] <- 1:2
    expect_equal(y[], rep(1:2, 2))

    expect_warning(y[] <- 8:1) ## this gives warning, as for numeric vectors
                               ## compare:   x <- 1:4; x[] <- 8:1

    ## arithmetic works as usual:
    2 * x
    x + 1:4
    expect_warning(x + 1:3) # warning - '... a multiple ...'

})

test_that("pdSafeParOrder is ok", {
    expect_equal(pdSafeParOrder(c(0,2)), c(1,2))
    expect_equal(pdSafeParOrder(c(2,3)), c(2,3))
})

test_that("permean2intercept and intercept2permean are ok", {
    ## d = 2
    mu2a <- c(1, 2)
    pm2a <- PeriodicArModel(matrix(c(0.5, 0.5), nrow = 2), order = rep(1, 2), sigma2 = 1, mean = mu2a)
    c2a <- permean2intercept(mu2a, pm2a@ar@coef, c(1,1))
    expect_equal(intercept2permean(c2a, pm2a@ar@coef, c(1,1)), mu2a)

    ## just to see if this works with length(intercept) = 1
    intercept2permean(1.5, pm2a@ar@coef, c(1,1))
    
    d <- 4
    mu4a <- 1:d
    co4a <- rep(0.5, d)
    pm4a <- PeriodicArModel(matrix(co4a, nrow = d), order = rep(1, d), sigma2 = 1, mean = mu4a)

    c4a <- permean2intercept(mu4a, pm4a@ar@coef, order = rep(1, d))
    expect_equal(intercept2permean(c4a, pm4a@ar@coef, order = rep(1, d)), mu4a)

})
