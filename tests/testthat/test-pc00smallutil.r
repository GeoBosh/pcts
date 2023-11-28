test_that("pc.arith.XXX() are ok",
{
    expect_equal(pc.arith.floor(9,4), 8)
    expect_equal(pc.arith.ceiling(9,4), 12)

    expect_equal(pc.arith.floor(12,4), 12)
    expect_equal(pc.arith.ceiling(12,4), 12)

    expect_equal(pc.arith.floor(-9,4), -12)
    expect_equal(pc.arith.ceiling(-9,4), -8)

    expect_equal(pc.arith.floor(-12,4), -12)
    expect_equal(pc.arith.ceiling(-12,4), -12)
})

test_that("toSeasonXXX() are ok",
{
    expect_equal(toSeason(1:4, 4), 1:4)
    expect_equal(toSeason(5:8, 4), 1:4)

    expect_equal(toSeason(2, 4, t1 = 2), 1)
    expect_equal(toSeason(1, 4, t1 = 2), 4)
    expect_equal(toSeason(5:8, 4, t1 = 2), c(4, 1:3))

    expect_equal(toSeason(4, 4, t1 = 4), 1)
    expect_equal(toSeason(1, 4, t1 = 4), 2)
    expect_equal(toSeason(1:8, 4, t1 = 4), c(2:4,1, 2:4,1))

    ## non-positive t
    expect_equal(toSeason(-4, 4), 4)
    expect_equal(toSeason(-3, 4), 1)
    expect_equal(toSeason(-2, 4), 2)
    expect_equal(toSeason(-1, 4), 3)
    expect_equal(toSeason( 0, 4), 4)

    expect_equal(toSeasonPair(2,3, period = 4), list(season = 3, lag = 1))
    ## swapping times doesn't change the result:
    expect_equal(toSeasonPair(2,3, period = 4), toSeasonPair(3,2, period = 4))

    expect_equal(as.vector(ttmatToslPairs(2,3, period = 4)), c(1, 1, 3, 1))
    expect_equal_to_reference(ttmatToslPairs(1:5,1:5, period = 4), "ttmatToslPairs.RDS")

    m <- matrix(c(10, 21, 32, 43,
                  21, 20, 31, 42,
                  32, 31, 30, 41,
                  43, 42, 41, 40
                  ), nrow = 4,  byrow = TRUE)

    m_sl <- ttTosl(m, period = 4)
    m_sl_manual <- matrix(c(10, NA, NA, NA,
                            20, 21, NA, NA,
                            30, 31, 32, NA,
                            40, 41, 42, 43
                            ), nrow = 4,  byrow = TRUE)
    expect_equal(m_sl, m_sl_manual)

    m2 <- matrix(c(10, 21, 32, 43, 14, 25, 36,
                   21, 20, 31, 42, 13, 24, 35,
                   32, 31, 30, 41, 12, 23, 34,
                   43, 42, 41, 40, 11, 22, 33,
                   14, 13, 12, 11, 10, 21, 32,
                   25, 24, 23, 22, 21, 20, 31,
                   36, 35, 34, 33, 32, 31, 30
                   ), nrow = 7,  byrow = TRUE)

    m2_sl <- ttTosl(m2, period = 4)
    m2_sl_manual <- matrix(c(10, 11, 12, 13, 14, NA, NA,
                             20, 21, 22, 23, 24, 25, NA,
                             30, 31, 32, 33, 34, 35, 36,
                             40, 41, 42, 43, NA, NA, NA
                            ), nrow = 4,  byrow = TRUE)
    expect_equal(m2_sl, m2_sl_manual)

    m3 <- m2
    m3[5, 5] <- 99 # expected equal to m[1,1], since period is 4.
    expect_warning(ttTosl(m3, period = 4), "encountered different values of acf for a season-lag pair")

})

