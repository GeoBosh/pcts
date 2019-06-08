context("fitPM")

test_that("test fitPM()",
{
    set.seed(1234)

    x <- arima.sim(list(ar = 0.9), n = 1000)
    mx <- matrix(x, nrow = 4)
    x_pcts <- pcts(as.numeric(x), nseasons = 4)

    expect_error(fitPM(c(1.5, 2, 3, 1), x), "The PAR orders must be non-negative integer numbers")
    expect_error(fitPM("dummy", x), "doesn't have a method for 'model' of class character")
    expect_error(fitPM(c(3,2,2,2), mx), "multivariate PAR fitting not implemented yet")

    proba1 <- fitPM(c(3, 2, 2, 2), as.numeric(mx))
    expect_equal_to_reference(proba1, "proba1.RDS")

    expect_error(fitPM(2, mx),
          "unable to find an inherited method for function [.]nSeasons[.] for signature [.]\"matrix\"[.]")
    expect_identical(fitPM(2, x_pcts), fitPM(c(2, 2, 2, 2), as.numeric(mx)))


    data(Fraser, package = "pear")
    logFraser <- log(Fraser)
    ## TODO: for now I need whole years
    logfraser <- ts(logFraser[1:936], frequency = 12)

    co1_pear <- pear::pear(logFraser, 1)[["phi"]]
        # fitPM(as.numeric(logFraser), order = rep(1, 12), period = 12, seasonof1st = 3)
    az1 <- fitPM(model = rep(1, 12), as.numeric(logFraser), seasonof1st = 3)
    az2 <- fitPM(model = rep(1, 12), as.numeric(logFraser))
    expect_true(all.equal(as.vector(az1@ar@coef[ , 1]), as.vector(co1_pear[ , 1])))
    expect_true(all.equal(as.vector(az2@ar@coef[ , 1]), as.vector(co1_pear[ , 1])[c(3:12, 1:2)]))
})
