context("autocovariances and autocorrelations")

test_that("pc.acrf is ok", {
    ## library("pear")

    data(Fraser, package = "pear")
    logFraser <- log(Fraser)
    ## TODO: for now I need whole years
    logfraser <- ts(logFraser[1:936], frequency = 12)

    #### commenting out comparisons with 'pear' since the following gave error on CRAN
    ####
    #### pe1 <- pear::peacf(logfraser, lag.max = 16)
    #### pe1acf <- pe1$acf

    pcacf1 <- pc.acrf(matrix(as.numeric(logfraser), nrow = 12), maxlag = 16)

    ## expect_equal(pcacf1[ , -1], pe1acf[ , 1:16], tol = 1e-10,
    ##              check.names = FALSE, check.attributes = FALSE)
    #### expect_equal(pcacf1[ , -1], pe1acf[ , 1:16],
    ####              check.names = FALSE, check.attributes = FALSE)
    #### 
    #### pe1a <- pear::peacf(logFraser, lag.max = 16)
    #### pe1aacf <- pe1a$acf
    pcacf1a <- calc_peracf(as.numeric(logFraser), maxlag = 16, period = 12, seasonof1st = 3)
    #### expect_equal(pcacf1a[ , -1], pe1aacf[ , 1:16],
    ####              check.names = FALSE, check.attributes = FALSE)

    pcacf1acov <- calc_peracf(as.numeric(logFraser), maxlag = 16, period = 12, seasonof1st = 3, what = "cov")

    sdfactor <- sqrt(pcacf1acov[ , 1])

    expect_equal(pc.acrftoacf(pcacf1a, sdfactor), pcacf1acov,
                 check.names = FALSE, check.attributes = FALSE)

    num2pcpar(as.numeric(logFraser), order = 2, period = 12, seasonof1st = 3)

    co1_az <- num2pcpar(as.numeric(logFraser), order = rep(1, 12), period = 12, seasonof1st = 3)
        # co1_az <- co1_az$coef@m[ , -1, drop = FALSE]
    co1_az <- as.matrix(co1_az$coef)[ , -1, drop = FALSE]
    #### names(pear::pear(logFraser, 1))
    #### pear::pear(logFraser, 1)[["model.orders"]]
    #### co1_pear <- pear::pear(logFraser, 1)[["phi"]]
    #### expect_equal(as.vector(co1_az), - as.vector(co1_pear))

    ## TODO: higher orders PAR

    autocovariances(1:60, maxlag = 10)
    autocovariances(1:60, nseasons = 4, maxlag = 10)

    autocorrelations(1:60, maxlag = 10)
    autocorrelations(1:60, nseasons = 4, maxlag = 10)

    expect_error(autocovariances(matrix(1:60, nrow =3), nseasons = 4, maxlag = 10))
    autocovariances(matrix(1:60, ncol = 2), maxlag = 10)
    autocovariances(matrix(1:64, ncol = 2), nseasons = 4, maxlag = 10)

})



