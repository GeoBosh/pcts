
test_that("pclspiar() is ok", {

    ts1 <- window(dataFranses1996[ , "CanadaUnemployment"],
                  start = c(1960, 1), end = c(1987, 4))
    test_piar(ts1, 4, 1, sintercept = TRUE)
    pcTest(ts1, "piar", 4, 1, sintercept = TRUE) # same

    test_piar(ts1, 4, 1, sintercept = TRUE, sslope = TRUE)
    test_piar(ts1, 4, 1)
    test_piar(ts1, 4, 1, homoschedastic = TRUE)


})
