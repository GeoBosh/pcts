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
    expect_output(show(proba1))
    expect_output(summary(proba1))

    expect_error(fitPM(2, mx),
                 ## "unable to find an inherited method for function [.]nSeasons[.] for signature [.]\"matrix\"[.]"
                                  "unable to find an inherited method for function"
                 )
    expect_identical(fitPM(2, x_pcts), fitPM(c(2, 2, 2, 2), as.numeric(mx)))


    data(Fraser, package = "pear")
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


    d4piar2  <- rbind(c(1,0.5,-0.06), c(1, 0.6, -0.08), c(1, 0.7, -0.1), c(1, 0.2, 0.15))
    picoef1  <- c(0.8, 1.25, 2, 0.5)
    parcoef1 <- d4piar2[, 2:3]
    coef1    <- pi1ar2par(picoef1, parcoef1)
    tmpval <- PeriodicArModel(parcoef1)
    ##pipfm    <- PiParModel(piorder = 1, picoef = picoef1, par = tmpval)
    pipfm    <-  new("PiPeriodicArModel", piorder = 1,
                     picoef = matrix(picoef1, ncol = 1), pcmodel = tmpval)

    perunit  <- sim_pc(list(phi = coef1, p = 3, q = 0, period = 4),500)
    fitPM(pipfm, perunit)

})
