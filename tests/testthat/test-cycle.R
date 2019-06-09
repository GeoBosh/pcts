test_that("the cycle classes are ok",
{
    ##
    ## BareCycle
    expect_equal_to_reference(b  <- new("BareCycle")              , "BareCycle_0.rds")
    expect_equal_to_reference(b1 <- new("BareCycle", nseasons = 1), "BareCycle_1.rds")
    expect_equal_to_reference(b4 <- new("BareCycle", nseasons = 4), "BareCycle_4.rds")
    expect_equal_to_reference(b12 <- new("BareCycle", nseasons = 12), "BareCycle_12.rds")

    expect_identical({ new("BareCycle", nseasons = 1L) }, b1)
    expect_identical({ new("BareCycle", nseasons = 4L) }, b4)

    expect_identical({ new("BareCycle", 1) }, b1)
    expect_identical({ new("BareCycle", 4) }, b4)

    ## SimpleCycle
    new("SimpleCycle")
    expect_equal_to_reference(a1 <- new("SimpleCycle", 1), "SimpleCycle_1.rds")
    expect_equal_to_reference(a4 <- new("SimpleCycle", 4), "SimpleCycle_4.rds")

    expect_identical({ new("SimpleCycle", nseasons = 1) }, a1)
    expect_identical({ new("SimpleCycle", nseasons = 4) }, a4)

    expect_error({ new("SimpleCycle", c(4, 12)) },
                   "currently 'nseasons' should have length one")

    ## Builtin cycles - only check that the constructors don't give errors for now.
    new("QuarterYearCycle")
    new("MonthYearCycle")
    new("DayWeekCycle")
    new("FiveDayWeekCycle")
    new("OpenCloseCycle")


    unitCycle(a4)
    unitSeason(a4)
    seqSeasons(a4)
    allSeasons(a4)

    a4a <- a4
    unitCycle(a4a) <- "Godina"
    unitSeason(a4a) <- "Trimesechie"
    allSeasons(a4a) <- c("Parvo", "Vtoro", "Treto", "Chetvart")
    expect_output(show(a4a))


    new("QuarterYearCycle", first = 2)



    BuiltinCycle(2)
    BuiltinCycle(4)
    BuiltinCycle(5)
    BuiltinCycle(7)
    yc <- BuiltinCycle(12)
    BuiltinCycle(48)
    expect_error(BuiltinCycle(999))

    as(yc, "SimpleCycle")

    pcCycle(4)

})

