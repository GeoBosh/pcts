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

    unitCycle()
    unitSeason()

    unitCycle(a4)
    unitSeason(a4)
    seqSeasons(a4)
    allSeasons(a4)

    a4a <- a4
    unitCycle(a4a) <- "Godina"
    unitSeason(a4a) <- "Trimesechie"
    allSeasons(a4a) <- c("Parvo", "Vtoro", "Treto", "Chetvarto")
    allSeasons(a4a, abb = TRUE) <- c("I", "II", "III", "IV")
    expect_output(show(a4a))

    a4a[1:2, abb = 2]
    a4a[1:2, abb = -2]

    a4a[abb = 2]
    a4a[abb = -2]

    a4a[1:2] <- letters[1:2]
    a4a[] <- LETTERS[1:4]
    

    new("QuarterYearCycle", first = 2)

    qy <- new("QuarterYearCycle")
    unitSeason(qy)
    unitCycle(qy)
    allSeasons(qy)
    allSeasons(qy, abb = TRUE)
    allSeasons(qy, abb = FALSE)

    dw <- new("DayWeekCycle")
    unitSeason(dw)
    unitCycle(dw)
    allSeasons(dw)
    allSeasons(dw, abb = TRUE)
    allSeasons(dw, abb = FALSE)

    fdw <- new("FiveDayWeekCycle")
    unitSeason(fdw)
    unitCycle(fdw)
    allSeasons(fdw)
    allSeasons(fdw, abb = TRUE)
    allSeasons(fdw, abb = FALSE)

    oc <- new("OpenCloseCycle")
    unitSeason(oc)
    unitCycle(oc)
    allSeasons(oc)
    allSeasons(oc, abb = TRUE)
    allSeasons(oc, abb = FALSE)


    BuiltinCycle(2)
    BuiltinCycle(4)
    BuiltinCycle(5)
    BuiltinCycle(7)
    yc <- BuiltinCycle(12)
    BuiltinCycle(48)
    expect_error(BuiltinCycle(999))

    as(yc, "SimpleCycle")

    pcCycle(4)
    cyc <- pcCycle(4, seasons = c("Spring", "Summer", "Autumn", "Winter"))
    pcCycle(cyc, "BareCycle")
    pcCycle(cyc,
            unitCycle = "Year", unitSeason = "Season",
            allSeasons = c("Spring", "Summer", "Autumn", "Winter"),
            abb = c("Sp", "Su", "Au", "Wi"))
    
    pcCycle(cyc, type = "SimpleCycle",
            unitCycle = "Year", unitSeason = "Season",
            allSeasons = c("Spring", "Summer", "Autumn", "Winter"),
            abb = c("Sp", "Su", "Au", "Wi"))
    

pcCycle("QuarterYearCycle")
pcCycle("QuarterYearCycle", type = "BareCycle")
pcCycle("QuarterYearCycle", type = "SimpleCycle")


    ## 'Every30MinutesCycle'
    e30 <- BuiltinCycle(48)
    unitSeason(e30)
    unitCycle(e30)
    allSeasons(e30)
    allSeasons(e30, abb = TRUE)


    ## Cyclic !!
ap <- pcts(AirPassengers)
nSeasons(ap) # 12
    unitCycle(ap)
    unitSeason(ap)

    ## can't change built-in class 'MonthYearCycle'
    expect_error(unitCycle(ap) <- "Godina")
    expect_error(unitSeason(ap) <- "Sezon")

    ap2 <- ap
    ap2@cycle <- as(ap2@cycle, "SimpleCycle")
    unitCycle(ap2) <- "Godina"
    unitSeason(ap2) <- "Sezon"
    allSeasons(ap2) <- paste0("S_", 1:12)
    
## pcfr <- pcts(dataFranses1996)
## nSeasons(pcfr) # 4


tipi <- dataFranses1996[ , "USTotalIPI"]
## plot(tipi)
## convert to PeriodicTS and remove NA's at the start and end
pctipi <- pcts(tipi)
pctipi <- window(pctipi, start = availStart(pctipi), end = availEnd(pctipi))
## plot(pctipi)
    plot(autocorrelations(pctipi, maxlag = 10))
    
})

