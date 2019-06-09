
test_that("pclspiar() is ok", {

    ts1 <- window(dataFranses1996[ , "CanadaUnemployment"],
                  start = c(1960, 1), end = c(1987, 4))
    test_piar(ts1, 4, 1, sintercept = TRUE)
    pcTest(ts1, "piar", 4, 1, sintercept = TRUE) # same

    test_piar(ts1, 4, 1, sintercept = TRUE, sslope = TRUE)
    test_piar(ts1, 4, 1)
    test_piar(ts1, 4, 1, homoschedastic = TRUE)


    parcoef    <- rbind(c(0.5, -0.06), c(0.6, -0.08),
                        c(0.7, -0.1),  c(0.2, 0.15) )
    picoef1    <- c(0.8, 1.25, 2, 0.5)

    parcoef2   <- pi1ar2par(picoef1, parcoef)
    picoef2    <- c(4, 0.25, 5, 0.2)

    coefper2I2 <- pi1ar2par(picoef2, parcoef2)

    expect_identical(coefper2I2, piar2par(picoef2, parcoef2))
    expect_identical(coefper2I2, piar2par(matrix(picoef2, ncol = 1), parcoef2))

})
