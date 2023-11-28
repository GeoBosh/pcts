test_that("trig.R is ok", {
    pcts_exdata()
    xtmp <- as.numeric(window(pcfr[[4]], start = availStart(pcfr[[4]]), end = availEnd(pcfr[[4]])))
    Mtrig(4)
    Mtrig(7)
    Mtrig(10)
    expect_equal(dim(Mtrig(10, harmonics = c(0:2))), c(10,5))
    expect_equal(dim(Mtrig(10, harmonics = c(0:2,5))), c(10,6))
    Mtrig_Lund(10, 1, harmonics = c(0:2,5))
    Mtrig_Lund(10, 2, harmonics = c(0,5)) # block-diag

    expect_true(is.null(.harmonics(Mtrig(4))))
    expect_equal(.harmonics(Mtrig(10, harmonics = c(0:2))), 0:2)

    expect_equal(.harmonics(Mtrig_Lund(10, 2, harmonics = list(c(0:1), c(0:2)))),
                 list(c(0:1), c(0:2)) )
    
    HsM(1, 7, 2, Mtrig(14))
    HsM(2, 7, 2, Mtrig(14))

    expect_true(all(Mtrig(14)[1:2,] == HsM(1, 7, 2, Mtrig(14))))

    ## TODO: there seems to be a bug here,
    ##       notice the big sigma for season 1
    num2pcpar(xtmp, order = 1, period = 4)
    
    tmpfit <- fit_trigPAR(AirPassengers, 1, 12, verbose = FALSE)
    tmpfit <- fit_trigPAR(AirPassengers, 2, 12, verbose = FALSE)

    tmpfit  <- fit_trigPAR_optim(xtmp, 2, 4, tol = 1e-14, verbose = FALSE)
    tmpfitL <- fit_trigPAR_optim(xtmp, 2, 4, tol = 1e-14, type = "bylag", verbose = FALSE)
    tmpfitP <- pclsdf(xtmp, 4, 1:2, sintercept = FALSE)

    beta  <- as.vector(tmpfit@other$data$M %*% tmpfit@other$fit$coef   )
    betaL <- as.vector(tmpfitL@other$data$M %*% tmpfitL@other$fit$coef )
    betaP <- as.vector(coef(tmpfitP$fit)                               )

    ## these should be numerically equal
    phi  <- matrix(beta, nrow = 4, byrow = TRUE)
    phiL <- matrix(betaL, nrow = 4)
    phiP <- matrix(betaP, nrow = 4)
    expect_equivalent(phi, phiL)
    expect_equivalent(phi, phiP)

    ## sintercept
    tmpfitc  <- fit_trigPAR_optim(xtmp, 2, 4, tol = 1e-14, verbose = FALSE, sintercept = TRUE)
    fit_trigPAR_optim(xtmp, 2, 4, tol = 1e-14, verbose = FALSE, sintercept = structure(TRUE, merge = FALSE))
    tmpfitcn  <- fit_trigPAR_optim(xtmp, 2, 4, tol = 1e-14, verbose = FALSE, sintercept = structure(TRUE, merge = TRUE))
    tmpfitLc <- fit_trigPAR_optim(xtmp, 2, 4, tol = 1e-14, type = "bylag", verbose = FALSE, sintercept = TRUE)

    coef(tmpfitc, matrix = TRUE)
    coef(tmpfitcn, matrix = TRUE)
    coef(tmpfitLc, matrix = TRUE)

    coef(tmpfitc)
    coef(tmpfitcn)
    coef(tmpfitLc)

    parc  <- coef(tmpfitc,  type = "PAR", matrix = TRUE)
    parcn <- coef(tmpfitcn, type = "PAR", matrix = TRUE)
    parLc <- coef(tmpfitLc, type = "PAR", matrix = TRUE)
    expect_equal(parc, parcn)
    expect_equal(parc, parLc)
    
    coef(tmpfit)
    coef(tmpfitL)

    parL <- coef(tmpfitL, type = "PAR", matrix = TRUE)
    par  <- coef(tmpfit, type = "PAR", matrix = TRUE)
    expect_equal(parL, par)

    predict(tmpfitc, n.ahead = 4)
    predict(tmpfitcn, n.ahead = 4)

    expect_output(show(tmpfitc))
    expect_output(show(tmpfitcn))
    expect_output(show(tmpfitLc))

    residuals(tmpfitc)
    fitted(tmpfitc)
    vcov(tmpfitc)
    p.air1 <- predict(fit_trigPAR_optim(AirPassengers[1:132], 4, 12, tol = 1e-14,
                                        type = "bylag", verbose = FALSE), n.ahead = 12)
    p.air2 <- predict(fit_trigPAR_optim(AirPassengers[1:132], 4, 12, tol = 1e-14,
                                        type = "vecbyrow", verbose = FALSE), n.ahead = 12)
    expect_equivalent(p.air1, p.air2)
    ## phi2C converts phi's to "vecbyrow" or "bylag"
    ##
    ## fromSubsetPM() returns coef's in phi format, fromSubsetPM() is unnecessarilly long,
    ## see the above computations of beta and betaL (I wrote fromSubsetPM() a lot later than
    ## the other trig functions and had forgotten the details.)
    ##
    ## all.equal(phi2C(fromSubsetPM(tmpfitL), type = "bylag"), as.vector(coef(tmpfitL)))
    expect_equivalent(phi2C(fromSubsetPM(tmpfitL), type = "bylag"), coef(tmpfitL))
    expect_equivalent(phi2C(fromSubsetPM(tmpfit), type = "vecbyrow"), coef(tmpfit))

    ## coef
    ## these do not do computations (TODO: so could be compared more strictly)
    expect_equivalent(coef(tmpfitL, type = "vecbyrow"), coef(tmpfit))
    expect_equivalent(coef(tmpfit, type = "bylag"), coef(tmpfitL))
    ## ... but these do compute the transformation
    expect_equivalent(coef(tmpfitL, type = "vecbyrow"), coef(tmpfit))
    expect_equivalent(coef(tmpfit, type = "bylag"), coef(tmpfitL))
    
    pclsdf(xtmp, 4, 1)

    fitPM(rep(1,4), xtmp)
    fitPM(rep(2,4), xtmp)
    fitPM(rep(1,4), pcts(xtmp, nseasons = 4))
    fitPM(rep(2,4), pcts(xtmp, nseasons = 4))
})
