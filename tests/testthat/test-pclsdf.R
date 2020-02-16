#    expect_that( , is_identical_to(  ))
#    make_expectation( , "is_identical_to")

test_that("pclsdf works correctly",
{
    ## sintercept = TRUE, sslope = FALSE, intercept = FALSE, slope = FALSE
    expect_equal_to_reference({
         res <- pclsdf(austres[-89], 4, lags = 1:3,
                       sintercept = FALSE, sslope = FALSE, intercept = FALSE, slope = FALSE,
                       contrasts = NULL)
         res$fit <- NULL # for now, res$fit contains environments, trouble for checking...
         res
    }, "pclsdf_FFFF.rds")


    contr <- ""
    for(i in c(FALSE,TRUE))
        for(si in c(FALSE,TRUE))
            for(s in c(FALSE,TRUE))
                for(ss in c(FALSE,TRUE)){
                    nam <- paste0(ifelse(c(si,ss,i,s),"T","F"), collapse = "")
                    res <- pclsdf(austres[-89], 4, lags = 1:3,
                                  sintercept = si, sslope = ss, intercept = i, slope = s,
                                  contrasts = NULL)
                    res$fit <- NULL # for now, res$fit contains environments, 
                    ## 2020-02-15 - 
                    ##   don't check names, since names of coefficients of 'sslope'
                    ##   vary between R versions. They also vary depending on what other terms
                    ##   are in the formula.
                    ##
                    ## This change related to the following error which started to appear on
                    ## CRAN for pcts v0.14-3 in Feb 2020, see also the changes in the code of
                    ## pclsdf(). Note that the error is only for half of the tests for which
                    ## sslope is TRUE (those with no intercept).
                    ## 
                    ## Version: 0.14-3
                    ## Check: tests
                    ## Result: ERROR
                    ##      Running 'testthat.R' [27s/30s]
                    ##     Running the tests in 'tests/testthat.R' failed.
                    ##     Complete output:
                    ##      ...
                    ##      > test_check("pcts")
                    ##      -- 1. Failure: pclsdf works correctly (@test-pclsdf.R#27) --------
                    ##      `res` has changed from known value recorded in 'pclsdf_FTFF.rds'.
                    ##      Component "sslope": names for target but not for current
                    ##      Component "sslope": Numeric: lengths (4, 0) differ
                    ##     
                    ##      -- 2. Failure: pclsdf works correctly (@test-pclsdf.R#27) --------
                    ##      `res` has changed from known value recorded in 'pclsdf_FTFT.rds'.
                    ##      Component "sslope": names for target but not for current
                    ##      Component "sslope": Numeric: lengths (4, 0) differ
                    ##     
                    ##      -- 3. Failure: pclsdf works correctly (@test-pclsdf.R#27) --------
                    ##      `res` has changed from known value recorded in 'pclsdf_FTTF.rds'.
                    ##      Component "sslope": names for target but not for current
                    ##      Component "sslope": Numeric: lengths (4, 0) differ
                    ##     
                    ##      -- 4. Failure: pclsdf works correctly (@test-pclsdf.R#27) --------
                    ##      `res` has changed from known value recorded in 'pclsdf_FTTT.rds'.
                    ##      Component "sslope": names for target but not for current
                    ##      Component "sslope": Numeric: lengths (4, 0) differ
                    ##     
                    ##      == testthat results ==============================================
                    ##      [ OK: 155 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 4 ]
                    ##      1. Failure: pclsdf works correctly (@test-pclsdf.R#27)
                    ##      2. Failure: pclsdf works correctly (@test-pclsdf.R#27)
                    ##      3. Failure: pclsdf works correctly (@test-pclsdf.R#27)
                    ##      4. Failure: pclsdf works correctly (@test-pclsdf.R#27)

                    ## This is the test from devtools just before setting check.names = FALSE:
                    ##
                    ## > devtools::test()
                    ## Loading pcts
                    ## Testing pcts
                    ## ✔ |  OK F W S | Context
                    ## ✔ |   2       | autocovariances and autocorrelations
                    ## ✔ |   4       | season-lag [0.3 s]
                    ## ✔ |  15       | cycle
                    ## ✔ |  10       | fitPM [6.7 s]
                    ## ✔ |  28       | pc00smallutil
                    ## ✖ |  13 4     | pclsdf
                    ## ─────────────────────────────────────────────────────────────────
                    ## test-pclsdf.R:27: failure: pclsdf works correctly
                    ## `res` has changed from known value recorded in 'pclsdf_FTFF.rds'.
                    ## Component 2: Names: 4 string mismatches
                    ## 
                    ## test-pclsdf.R:27: failure: pclsdf works correctly
                    ## `res` has changed from known value recorded in 'pclsdf_FTFT.rds'.
                    ## Component 2: Names: 4 string mismatches
                    ## 
                    ## test-pclsdf.R:27: failure: pclsdf works correctly
                    ## `res` has changed from known value recorded in 'pclsdf_FTTF.rds'.
                    ## Component 2: Names: 4 string mismatches
                    ## 
                    ## test-pclsdf.R:27: failure: pclsdf works correctly
                    ## `res` has changed from known value recorded in 'pclsdf_FTTT.rds'.
                    ## Component 2: Names: 4 string mismatches
                    
                    expect_equal_to_reference(res, paste0("pclsdf_", nam, ".rds"),
                                              check.names = FALSE)
                }
    
})





# pclsdf(austres, 4, lags = 1:3)
# pclsdf(austres, 4, lags = 1:3, sintercept = TRUE)
# pclsdf(austres, 4, lags = 1:3, sintercept = TRUE, sslope = TRUE)
# 
# x <- rep(1:4,10)
# pclsdf(x, 4, lags = 1:3, sintercept = TRUE, sslope = TRUE)
# 
# # this is for the version when contrasts arg. was passed on directly to lm.
# # tmp1 <- pclsdf(austres, 4, lags = 1, sintercept = FALSE, sslope = TRUE, contrasts = list(Season = "contr.sum" ))
# 
# contr <- ""
# for(i in c(FALSE,TRUE))
#     for(si in c(FALSE,TRUE))
#         for(s in c(FALSE,TRUE))
#             for(ss in c(FALSE,TRUE)){
#                 nam <- paste0("mo",  paste0(ifelse(c(i,si,s,ss), "T", "F"), collapse = ""),
#                               contr) 
#                 print(nam)
#                 fit <- pclsdf(austres[-89], 4, lags = 1:3, intercept = i, sintercept = si,
#                               slope = s, sslope = ss, contrasts = NULL)
#                 print(fit)
#                 assign(nam,fit)
#             }
# 
# contr <- "contr.sum"
# for(i in c(FALSE,TRUE))
#     for(si in c(FALSE,TRUE))
#         for(s in c(FALSE,TRUE))
#             for(ss in c(FALSE,TRUE)){
#                 nam <- paste0("mo",  paste0(ifelse(c(i,si,s,ss), "T", "F"), collapse = ""),
#                                      contr)
#                 print(nam)
#                 fit <- pclsdf(austres[-89], 4, lags = 1:3, intercept = i, sintercept = si,
#                               slope = s, sslope = ss, contrasts = "contr.sum")
#                 print(fit)
#                 assign(nam,fit)
#             }
# 
# contr <- "iden"
# for(i in c(FALSE,TRUE))
#     for(si in c(FALSE,TRUE))
#         for(s in c(FALSE,TRUE))
#             for(ss in c(FALSE,TRUE)){
#                 nam <- paste0("mo",  paste0(ifelse(c(i,si,s,ss), "T", "F"), collapse = ""),
#                               contr) 
#                 print(nam)
#                 fit <- pclsdf(austres[-89], 4, lags = 1:3, intercept = i, sintercept = si,
#                               slope = s, sslope = ss, contrasts = identity(4))
#                 print(fit)
#                 assign(nam,fit)
#             }
