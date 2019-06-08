context("sim_XXX")

test_that("sim_pc is ok", {
    m1 <- rbind( c(1, 0.81, 0), c(1, 0.4972376, 0.4972376) )
    testphi <- slMatrix( init = m1 )

    m2 <- rbind( c(1, 0, 0), c(1, 0, 0) )
    testtheta <- slMatrix( init = m2 )

    ## phi and theta are slMatrix here.
    mo1 <- list(phi = testphi, theta = testtheta, p = 2, q = 2, period = 2)
    set.seed(1234)
    a1 <- sim_pc(mo1, 100)

    ## phi and theta are ordinary matrices here.
    mo2 <- list(phi = m1[ , 2:ncol(m1)], theta = m2[ , 2:ncol(m2)], p = 2, q = 2, period = 2)
    set.seed(1234)
    a2 <- sim_pc(mo2, 100)

    expect_identical(a1, a2)
})


test_that("sim_parAcvf is ok", {
    set.seed(1234)
    tmp1 <- sim_parAcvf(2, 5)
    expect_equal_to_reference(tmp1, "sim_parAcvf_a.RDS")
    tmp2 <- sim_parAcvf(2, 5, sigma2 = 2)
    expect_equal_to_reference(tmp2, "sim_parAcvf_b.RDS")
})


test_that("sim_parCoef is ok", {
    ## TODO: do not put code involving simulations in the expect_XXX functions!
    ##       no reproducibility is guaranteed!
    set.seed(1234)
    tmp <- sim_parCoef(2, 4)
    expect_equal_to_reference(tmp, "sim_parCoef_1a.RDS") # 2 seasons
    tmp2 <- sim_parCoef(2, 4, sigma2 = c(2, 4))
    expect_equal_to_reference(tmp2, "sim_parCoef_1b.RDS")

    tmp3 <- sim_parCoef(4, 2)
    expect_equal_to_reference(tmp3, "sim_parCoef_2.RDS") # 4 seasons

    tmp4 <- sim_parCoef(period = 4, n.root = 6,
                        eigabs = c(1, 1, 1, 0.036568887, 0.001968887),
                        type.eigval = c("cp", "r", "r", "r",  "r"),
                        eigsign     = c(pi/2,   1,  -1,   1,   -1))
    expect_equal_to_reference(tmp4, "sim_parCoef_3.RDS")
})


test_that("sim_pwn is ok", {
    ## TODO: do not put calculations that involve sampling in functions with non-standard evaluation,
    ##       like the expectations. This is a cause of trouble!
    set.seed(1234)
    pwn1 <- sim_pwn(100, f = rnorm, scale = c(0.5, 2))
    expect_equal_to_reference(pwn1, "sim_pwn_1.RDS")

    pwn2 <- sim_pwn(n = 100, scale = c(0.5, 2))  # rnorm is the default generator
    expect_equal_to_reference(pwn2, "sim_pwn_2.RDS")

    pwn3 <- sim_pwn(100, f = list(c(rnorm, 0, 0.5), c(rnorm, 0, 2)))
    expect_equal_to_reference(pwn3, "sim_pwn_3.RDS")

})
