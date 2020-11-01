context("autocovariances and autocorrelations")

test_that("pc.acrf is ok", {

    ## 'presidents' is quarterly, full years (len = 120)
    ##     but it contains NA's and the first value is NA.
    pres <- pcts(presidents)
    
    expect_equal(pc_sum(pres, 4),               c(NA, 1693, NA,  NA))
    expect_equal(pc_sum(pres, 4, na.rm = TRUE), c(1695, 1693, 1545, 1486))

    expect_equal(pc_mean(pres, 4), rowMeans(matrix(pres, nrow = 4)))              
    expect_equal(pc_mean(pres, 4, na.rm = TRUE),
                 rowMeans(matrix(pres, nrow = 4), na.rm = TRUE))

    matpres <- matrix(presidents, nrow = 4)
    expect_equal(pc.acsum(matpres, 6), pc.cconesidedsum(matpres, matpres, 6))
    ## TODO: need example for pc.cconesidedsum with y not identical to x
    
    ## presidents[1] is NA, so now not all years are complete and the 1st value is for 2nd
    ##                      quarter (hence the shiftleft() below)
    pres.m1 <- presidents[-1]
    ## with na.rm = TRUE the values just get rotated
    expect_equal(pc_sum(pres.m1, 4, na.rm = TRUE), shiftleft(c(1695, 1693, 1545, 1486)))
    expect_equal(pc_mean(pres[-1], 4, na.rm = TRUE),
                 shiftleft(rowMeans(matrix(pres, nrow = 4), na.rm = TRUE)))

    ## drop [4] from the comparison the dropped value is the only NA in the 1st quarter, so
    ## sum and mean for it are not NA.
    expect_true( is.na(pc_sum(pres,     4)[4]) )
    expect_true( !is.na(pc_sum(pres.m1, 4)[4]) )
    expect_equal(pc_sum(pres.m1, 4)[-4], shiftleft(c(NA, 1693, NA,  NA))[-4])
    expect_equal(pc_mean(pres.m1, 4)[-4], shiftleft(rowMeans(matrix(pres, nrow = 4)))[-4])
    
    ## replacing with equivalent code using the new Fraser2017
    ##     data(Fraser, package = "pear")
    Fraser <- window(Fraser2017, start = c(1912, 3), end = c(1990, 12))
    
    logFraser <- log(Fraser)
    ## TODO: for now I need whole years
    logfraser936 <- ts(logFraser[1:936], frequency = 12)

    #### commenting out comparisons with 'pear' since the following gave error on CRAN
    ####
    #### pe1 <- pear::peacf(logfraser936, lag.max = 16)
    #### pe1acf <- pe1$acf

    pcacrf1 <- pc.acrf(matrix(as.numeric(logfraser936), nrow = 12), maxlag = 5)
    pcacvf1 <- pc.acf(matrix(as.numeric(logfraser936), nrow = 12), maxlag = 5)
    fac <- pc_sdfactor(sqrt(pcacvf1[ , 1]), 5)
    expect_equal(pcacvf1, pcacrf1 * fac)

    ## expect_equal(pcacrf1[ , -1], pe1acf[ , 1:16], tol = 1e-10,
    ##              check.names = FALSE, check.attributes = FALSE)
    #### expect_equal(pcacrf1[ , -1], pe1acf[ , 1:16],
    ####              check.names = FALSE, check.attributes = FALSE)
    ####
    #### pe1a <- pear::peacf(logfraser936, lag.max = 16)
    #### pe1aacf <- pe1a$acf
    calc_peracf(as.numeric(logfraser936), maxlag = 16, period = 12, seasonof1st = 3,
                           mean = 7) # 7 is arbitrary here
    pcacrf1a <- calc_peracf(as.numeric(logfraser936), maxlag = 16, period = 12, seasonof1st = 3)
    #### expect_equal(pcacrf1a[ , -1], pe1aacf[ , 1:16],
    ####              check.names = FALSE, check.attributes = FALSE)

    pcacrf1acov <- calc_peracf(as.numeric(logfraser936), maxlag = 16, period = 12, seasonof1st = 3, what = "cov")

    sdfactor <- sqrt(pcacrf1acov[ , 1])

    expect_equal(pc.acrftoacf(pcacrf1a, sdfactor), pcacrf1acov,
                 check.names = FALSE, check.attributes = FALSE)

    num2pcpar(as.numeric(logfraser936), order = 2, period = 12, seasonof1st = 3)
    num2pcpar(as.numeric(logfraser936), order = 2, period = 12, seasonof1st = 3, result = "coef")

    co1_az <- num2pcpar(as.numeric(logfraser936), order = rep(1, 12), period = 12, seasonof1st = 3)
        # co1_az <- co1_az$coef@m[ , -1, drop = FALSE]
    co1_az <- as.matrix(co1_az$coef)[ , -1, drop = FALSE]
    #### names(pear::pear(logfraser936, 1))
    #### pear::pear(logfraser936, 1)[["model.orders"]]
    #### co1_pear <- pear::pear(logfraser936, 1)[["phi"]]
    #### expect_equal(as.vector(co1_az), - as.vector(co1_pear))

    ## TODO: higher orders PAR

    autocovariances(1:60, maxlag = 10)
    autocovariances(1:60, nseasons = 4, maxlag = 10)
    
    expect_output(show(autocovariances(1:60, nseasons = 4, maxlag = 10)))

    autocorrelations(1:60, maxlag = 10)
    autocorrelations(1:60, nseasons = 4, maxlag = 10)

    expect_error(autocovariances(matrix(1:60, nrow =3), nseasons = 4, maxlag = 10))
    autocovariances(matrix(1:60, ncol = 2), maxlag = 10)
    autocovariances(matrix(1:64, ncol = 2), nseasons = 4, maxlag = 10)

})



