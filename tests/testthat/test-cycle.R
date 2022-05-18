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

    expect_identical(new("BareCycle", 5), pcCycle(5))
    expect_identical(BuiltinCycle(7), new("DayWeekCycle"))
    expect_identical(BuiltinCycle(7, first = 7), new("DayWeekCycle", first = 7)) 
    expect_identical(BuiltinCycle(12), new("MonthYearCycle") )
    expect_identical(new("BareCycle", 5), pcCycle(5))
    
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

    dwc <- new("DayWeekCycle")
    expect_equal(.get_period_length(dwc), 1)
    expect_equal(.get_period_units(dwc), "days")

    expect_error(new("FiveDayWeekCycle"),
                 "New objects from class 'FiveDayWeekCycle' can no longer be created")
    ## TODO: when class "FiveDayWeekCycle" is removed, modify "FiveDayWeekCycle_objects.R" to
    ##       use Builtin(5) and regenerate "FiveDayWeekCycle_objects.rds".
    fdc_all <- readRDS("FiveDayWeekCycle_objects.rds")
    
    fdc <- fdc_all[[1]] # new("FiveDayWeekCycle")
    expect_equal(.get_period_length(fdc), 1)
    expect_equal(.get_period_units(fdc), "days")

    .cycle_and_time2pair(fdc, as.Date(4)) # [1] 2 1
    .cycle_and_time2pair(fdc, as.Date(6)) # [1] 2 3
    .cycle_and_time2pair(fdc, as.Date(8)) # [1] 2 5
    .cycle_and_time2pair(fdc, as.Date(9)) # [1] 2 NA   (Saturday)

    .cycle_offsets(fdc, 10, 1) # [1]  0  1  2  3  4  7  8  9 10 11
    ## two five-day weeks from Monday 1969-12-29
    expect_equal(as.Date(.get_origin(fdc))  + 1 + .cycle_offsets(fdc, 10, 1),
                 c(seq(as.Date("1969-12-29"), as.Date("1970-01-02"), length.out = 5),
                   seq(as.Date("1970-01-05"), as.Date("1970-01-09"), length.out = 5)) )
    
    unitSeason(fdc)
    unitCycle(fdc)
    allSeasons(fdc)
    allSeasons(fdc, abb = TRUE)
    allSeasons(fdc, abb = FALSE)
    fdc[1:3]
    fdc[]

    .cycle_offsets(BuiltinCycle(7), 10, 1) # 0:9
    expect_equal(
        as.Date(.get_origin(BuiltinCycle(7)))  + 1 + .cycle_offsets(BuiltinCycle(7), 10, 1),
        seq(as.Date("1969-12-29"), by = "1 day", length.out = 10) )
    
    ## pdc5b <- new("FiveDayWeekCycle")
    pdc5bP1 <- as.Date(Cyclic(cycle = fdc, start = c(1,1))) #  "1969-12-29"
    format(pdc5bP1)
    pdc5bP2 <- as.Date(Cyclic(cycle = fdc, start = c(1,2))) # "1969-12-30"
    format(pdc5bP2)
    
    # pcpdc5bP1 <- as.pcdate(Cyclic(cycle = fdc, start = c(1,1))) # "W1 Monday"
    # format(pcpdc5bP1)
    # pcpdc5bP2 <- as.pcdate(Cyclic(cycle = fdc, start = c(1,2))) # "W1 Tuesday"
    # format(pcpdc5bP2)
    
    
    occ <- new("OpenCloseCycle")
    expect_equal(.get_period_length(occ), 12)
    expect_equal(.get_period_units(occ), "hours")

    ## 'Every30MinutesCycle'
    e30 <- BuiltinCycle(48)
    expect_equal(.get_period_length(e30), 30)
    expect_equal(.get_period_units(e30), "minutes")
    .get_origin(e30)

    ## TODO: this checks current behaviour, not clear it should stay so. 
    expect_equal(.get_period_length(BareCycle(3)), 1)
    expect_equal(.get_period_units(BareCycle(3)), "seconds")
    

    unitSeason(e30)
    unitCycle(e30)
    allSeasons(e30)
    allSeasons(e30, abb = TRUE)
    
    
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
    expect_error(BuiltinCycle(19))
    BuiltinCycle(19, stop = FALSE)
    BuiltinCycle(19, coerce = TRUE, stop = FALSE)

    BuiltinCycle(4, coerce = TRUE)

    pdc5a <- new("PartialCycle", orig = new("DayWeekCycle"), subindex = 1:5)
    expect_output(show(pdc5a))
    unitSeason(pdc5a)
    unitCycle(pdc5a)
    allSeasons(pdc5a)
    allSeasons(pdc5a, abb = TRUE)
    pdc5a[1:3]
    pdc5a[]

    expect_equal(.get_origin(pdc5a), as.POSIXct("1969-12-28", "UTC"))
    expect_equal(.get_offset(pdc5a), 0)

    
    
    ## pdcWeekend <- new("PartialDayWeekCycle", subindex = 6:7)
    ## unitSeason(pdcWeekend)
    ## unitCycle(pdcWeekend)
    ## allSeasons(pdcWeekend)
    ## allSeasons(pdcWeekend, abb = TRUE)
    ## pdcWeekend[1:3] # [1] "Saturday" "Sunday"   NA   ==> TODO: is this by design?
    ## pdcWeekend[]

    ## equivalent to above
    pdcWeekend2 <- new("PartialCycle", orig = new("DayWeekCycle"), subindex = 6:7)
    unitSeason(pdcWeekend2)
    unitCycle(pdcWeekend2)
    allSeasons(pdcWeekend2)
    allSeasons(pdcWeekend2, abb = TRUE)
    pdcWeekend2[1:3] # [1] "Saturday" "Sunday"   NA   ==> TODO: is this by design?
    pdcWeekend2[]

    
    as(yc, "SimpleCycle")

    pcCycle(4)
    pcCycle(4, "SimpleCycle")
    pcCycle(4, "BareCycle")
    
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




    ## Cyclic !!
ap <- pcts(AirPassengers)
nSeasons(ap) # 12
    unitCycle(ap)
    unitSeason(ap)
    expect_equal(frequency(ap), 12)
    expect_equal(deltat(ap), 1/12)
    
    expect_equal(cycle(as(ap, "Cyclic")), 1) # only one value
    expect_equal(frequency(as(ap, "Cyclic")), 12)
    expect_equal(deltat(as(ap, "Cyclic")), 1/12)


    ## can't change built-in class 'MonthYearCycle'
    expect_error(unitCycle(ap) <- "Godina")
    expect_error(unitSeason(ap) <- "Sezon")

    ap2 <- ap
    ap2@cycle <- as(ap2@cycle, "SimpleCycle")
    unitCycle(ap2) <- "Godina"
    unitSeason(ap2) <- "Sezon"
    allSeasons(ap2) <- paste0("S_", 1:12)
    
    pcfr <- pcts(dataFranses1996)
    ## nSeasons(pcfr) # 4
    expect_error(pcfr[[c(1,2)]],
                 "for \\[\\[ the length of argument i must be equal to one")

    pctipiM <- pcfr["USTotalIPI"] # PeriodicMTS
    plot(pctipiM)
tipi <- dataFranses1996[ , "USTotalIPI"]
## plot(tipi)
## convert to PeriodicTS and remove NA's at the start and end
pctipi <- pcts(tipi) # PeriodicTS
pctipi <- window(pctipi, start = availStart(pctipi), end = availEnd(pctipi))
 plot(pctipi)
    plot(autocorrelations(pctipi, maxlag = 10))

    ## pc-origin is as.Date("1970-01-01") - 4, Sunday
    expect_equal(.date2pcweek(as.Date("1969-12-28")), c(0, 7))
    expect_equal(.date2pcweek(as.Date("1969-12-29")), c(1, 1))
    expect_equal(.date2pcweek(as.Date("1970-01-01")), c(1, 4))

    expect_equal(.date2pcweek("1969-12-28"), c(0, 7))
    expect_equal(.date2pcweek("1969-12-29"), c(1, 1))
    expect_equal(.date2pcweek("1970-01-01"), c(1, 4))
    
    expect_equal(pcweek2date(c(0, 7)), as.POSIXct("1969-12-28", "UTC"))
    expect_equal(pcweek2date(c(1, 1)), as.POSIXct("1969-12-29", "UTC"))
    expect_equal(pcweek2date(c(1, 4)), as.POSIXct("1970-01-01", "UTC"))

    expect_equal(Cyclic(BuiltinCycle(7), "2020-04-06")@pcstart, c(2624, 1))
    expect_equal(Cyclic(BuiltinCycle(5), "2020-04-06")@pcstart, c(2624, 1))

    
    ap7to9 <- window(ap, seasons = 7:9)
    expect_equal(allSeasons(window(ap, seasons = 7:9)), c("July", "August", "September") )
    expect_equal(unitSeason(ap7to9), "Month")
    expect_equal(unitCycle(ap7to9), "Year")

    expect_output(show(ap7to9))
    
    ## start2pc_time(c(1,1), ap7to9@cycle)

    as.Date(as(ap, "Cyclic"))

    as.Date(ap)
    as.Date(ap7to9)
    as.Date(as(ap7to9, "Cyclic"))

    as.Date("2019-08-01") + period(1, "months") * .cycle_offsets(ap7to9@cycle, 12, 2)

    as_date(ap)
    as_date(ap7to9)
    as_date(as(ap7to9, "Cyclic"))

    expect_identical(as.POSIXct(ap), as_datetime(ap))
    as.Date(ap)
    
    # as.pcdate(as(ap7to9, "Cyclic"))
    # pcdate_ap7to9 <- as.pcdate(ap7to9)
    # 
    # pcd1 <- pcdate(as.Date("2020-04-01"), BuiltinCycle(4))
    # expect_equal(pcdate("2020-04-01", BuiltinCycle(4)), pcd1)
    # expect_equal(pcdate(c(2020, 2), BuiltinCycle(4)), pcd1)
                        
    
    # format(pcdate_ap7to9)
    # expect_output(str(pcdate_ap7to9))


    
    Cyclic(cycle = pdcWeekend2, start = c(12, 1)) # Saturday
    Cyclic(cycle = pdcWeekend2, start = c(12, 2)) # Sunday
    Cyclic(cycle = pdcWeekend2, start = c(12, 3)) # weekday is NA


    pdcWeekendP <- new("PartialCycle", orig = new("DayWeekCycle"), subindex = 6:7)

    Cyclic(3, c(1,1))    

    .get_origin(BuiltinCycle(7))

    
    .get_period(BuiltinCycle(7))  # [1] "1d 0H 0M 0S"
    .get_period(BuiltinCycle(2))  # [1] "12H 0M 0S"
    .get_period(BuiltinCycle(48)) # [1] "30M 0S"

    .cycle_and_pair2time(BuiltinCycle(7), c(2624, 5)) # [1] "2020-04-10"
    .cycle_and_pair2time(BuiltinCycle(7), c(2624, 2624, 5, 6)) # [1] "2020-04-10"
    .cycle_and_time2pair(BuiltinCycle(7), as.Date("2020-04-10")) # [1] 2624    5

    expect_equal(.cycle_and_time2pair(BuiltinCycle(12), as.Date("1970-04-01")),
                 c(1970, 4) )
    expect_equal(.cycle_and_time2pair(BuiltinCycle(4), as.Date("1970-04-01")),
                 c(1970, 2) )

    expect_equal(.cycle_and_time2pair(BuiltinCycle(5), as.Date("1970-01-01")), c(1, 4 ))
    expect_equal(.cycle_and_time2pair(BuiltinCycle(5), as.Date("1970-01-02")), c(1, 5 ))
    expect_equal(.cycle_and_time2pair(BuiltinCycle(5), as.Date("1970-01-03")), c(1, NA))
    expect_equal(.cycle_and_time2pair(BuiltinCycle(5), as.Date("1970-01-04")), c(1, NA))

    .cycle_and_pair2time(BuiltinCycle(7), c(1, 6)) == pcweek2date(c(1,6))

    expect_equal(.cycle_and_pair2time(BuiltinCycle(7), c(1, 6)),
                 as.POSIXct("1970-01-03", "UTC"))
    ## Saturdays are not included in five day week, so:
    expect_equal(.cycle_and_pair2time(BuiltinCycle(5), c(1, 6)),
                 as_datetime(NA, "UTC") )

    .cycle_and_pair2time(BuiltinCycle(4), c(2020, 2))  #  "2020-04-01 UTC"

    .cycle_and_pair2time(BuiltinCycle(2), c(1, 1)) # [1] "1969-12-29 09:00:00 UTC"
    .cycle_and_pair2time(BuiltinCycle(2), c(1, 2)) # [1] "1969-12-29 21:00:00 UTC"
    .cycle_and_pair2time(BuiltinCycle(2), c(4, 1)) # [1] "1970-01-01 09:00:00 UTC"
    .cycle_and_pair2time(BuiltinCycle(2), c(4, 2))  # [1] "1970-01-01 21:00:00 UTC"

    twoweeks <- seq(as.Date("2020-04-13"), by = "day", length.out = 14)
    # pcdate(twoweeks, BuiltinCycle(7))
    
    twoyears <- seq(as.Date("2020-04-13"), by = "month", length.out = 14)
    # pcdate(twoyears, BuiltinCycle(12))

    eightquarters <- seq(as.Date("2020-01-01"), by = "3 months", length.out = 14)
    # pcdate(eightquarters, BuiltinCycle(4))
    # as.pcdate(pcts(dataFranses1996, nseasons = 4))
    # as.pcdate(pcts(dataFranses1996, nseasons = 4))

    expect_equal(.nperiods(BuiltinCycle(4), seq(as.Date("2020-04-01"), by ="3 months", length.out = 3)
                           ), c(202, 203, 204) )

    
    expect_equal(allSeasons(new("BareCycle")), character(0))

    bc4 <- BareCycle(4)
    allSeasons(bc4)
   
    .get_period_units(bc4)  # [1] "seconds"
    .get_period_length(bc4) # [1] 1
    .get_period(bc4)        # [1] "1S"
    .get_origin(bc4)        # [1] "1970-01-01 UTC"
    .nperiods(bc4, 5)       
    .cycle_and_time2pair(bc4, 5) # [1] 2 1
    .cycle_and_time2pair(bc4, 13) # [1] 4 1
    as.Date(.cycle_and_time2pair(bc4, 13)) # [1] "1970-01-05" "1970-01-02"
    # pcdate(13, bc4) # [1] "C4 S1"
    expect_error(date(bc4), "date is undefined for Cycle objects")


    .cycle_and_pair2time(bc4, c(4, 1)) # [1] "1970-01-01 00:00:13 UTC"
    .cycle_and_time2pair(bc4, 5) # [1] 2 1
    .cycle_and_time2pair(bc4, 9) # [1] 3 1

    fourseasons <- pcCycle(4, seasons = c("Spring", "Summer", "Autumn", "Winter"))
    .get_period_units(fourseasons)  
    .get_period_length(fourseasons) 
    .get_period(fourseasons)        
    .get_origin(fourseasons)        
    .nperiods(fourseasons, 5)       


    ## .mark_invalid_seasons_in_pairs
    expect_equal(.mark_invalid_seasons_in_pairs(c(2020, 4), 7:9), c(2020, NA))
    expect_equal(.mark_invalid_seasons_in_pairs(c(2020, 9), 7:9), c(2020, 9))
    expect_equal(.mark_invalid_seasons_in_pairs(c(2020, NA), 7:9), c(2020, NA))

    ## more pairs
    morepairs <- c(rep(2020, 12), 1:12)
    morepairs.marked <- c(rep(2020, 12), c(rep(NA, 6), 7:9, rep(NA, 3)))
    expect_equal(.mark_invalid_seasons_in_pairs(morepairs, 7:9), morepairs.marked)
    expect_equal(.mark_invalid_seasons_in_pairs(c(1:6, c(1:3, 6,4,7)), 4:6),
                 c(1:6, NA, NA, NA,  6,  4, NA) )

    
    .recode_seasons_in_pairs(c(5,5,5, 1,2,3), 7:9, 12)

    expect_equal(ind2pctime(13, c(1, 1), 4), c(4, 1))
    expect_equal(pctime2ind(c(4,1), c(1, 1), 4), 13)
    expect_equal(pctime2ind(c(4,4), c(1, 1), 4), 16)
    ## more pairs
    expect_equal(pctime2ind(c(4,4,4, 1,2,3), c(1, 1), 4), 13:15)

    expect_equal(.allseas(BuiltinCycle(4), letters[1:4]), letters[1:4])
    expect_equal(.allseas(BuiltinCycle(4, first = 2), letters[1:4]), letters[c(2,3,4, 1)])


    ## Pctime
    Pctime(Sys.time(), BuiltinCycle(4))   # from datetime
    pct2020_01_01 <- Pctime("2020-01-01", BuiltinCycle(4)) # from date
    Pctime(pct2020_01_01) # from Pctime, no op.

    expect_equal(cycle(Pctime(ap)), as.numeric(cycle(ap)))
    expect_equal(cycle(Pctime(ap7to9)), rep(1:3, length.out = length(ap7to9)))
    
    Pctime(c(4,1), bc4) # [1] "C4 S1"
    expect_error(Pctime(bc4), "cycle not specified and cannot be inferred")
    expect_error(Pctime(13, bc4), "the length of pairs must be even")

    Pctime(c(4, 1), fourseasons) # [1] "C4 Spring"
    
    Pctime(c(2020, 1), ap7to9@cycle) # [1] "Y2020 Jul"
    Pctime(c(2020, 2), ap7to9@cycle) # [1] "Y2020 Aug"
    Pctime(c(2020, 3), ap7to9@cycle) # [1] "Y2020 Sep"

    Pctime(as.POSIXct(Sys.time()), BuiltinCycle(4))

    pct1 <- Pctime(as.Date("2020-04-01"), BuiltinCycle(4))
    expect_equal(Pctime("2020-04-01", BuiltinCycle(4)), pct1)
    expect_equal(Pctime(c(2020, 2),   BuiltinCycle(4)), pct1)
    expect_equal(Pctime(c(2020, 2),   BuiltinCycle(4)), Pctime("2020-04-01", BuiltinCycle(4)))
    expect_output(str(pct1))

    pct.ap <- Pctime(ap)
    expect_true(!inherits(pct.ap[[1]], "Pctime"))

    expect_true(inherits(pct.ap[1], "Pctime"))
    expect_true(inherits(pct.ap[], "Pctime"))
    expect_output(print(pct.ap[1])) #a bug was causing printing to fail, so check this works

    pct.ap[1] <- as_date("2020-04-01")
    Cyclic(pct.ap[1])
    expect_warning(Cyclic(pct.ap), "got datetime start with length > 1, using first")

    
    Cyclic(cycle = pdcWeekend2, start = as_datetime("2020-04-11")) # Sat
    Cyclic(cycle = pdcWeekend2, start = as.Date("2020-04-11"))
    
    as_Pctime(as(ap, "Cyclic"))
    ## TODO: think: one of as.Date() and date() may be redundant
    ##       but: date has assignment counterpart, "date<-"
    ##       but: as.Date() has '...' argument
    date(as(ap, "Cyclic")) # ok but TODO: maybe format as  date or time?
    date(ap)   # TODO: unintended, should define a method for PeriodicTimeSeries 

    
    format(Pctime(c(4, 1) , pdcWeekendP))
    seq(Pctime(c(4, 1) , pdcWeekendP), length.out = 4)
    as.POSIXct(Pctime(c(4, 1) , pdcWeekendP))
    
    pdcWP1 <- as.Date(Cyclic(cycle = pdcWeekendP, start = c(1,1))) # "1970-01-03"
    format(pdcWP1)
    pdcWP2 <- as.Date(Cyclic(cycle = pdcWeekendP, start = c(1,2))) # "1970-01-04"
    format(pdcWP2)
    
    # pdcP1 <- as.pcdate(Cyclic(cycle = pdcWeekendP, start = c(1,1))) # "1970-01-03"
    # format(pdcP1)
    # pdcP2 <- as.pcdate(Cyclic(cycle = pdcWeekendP, start = c(1,2))) # "1970-01-04"
    # format(pdcP2)

    as.Date(pctipi)
    # as.pcdate(pctipi)
    # format(as.pcdate(pctipi))

    as_Pctime(pctipi)
    format(as_Pctime(pctipi))

    as.Date(window(pctipi, seasons = 1:2))
    as.Date(window(pctipi, seasons = c(1L, 3L)))
    as.Date(window(pctipi, seasons = c(1L, 4L)))
    as.Date(window(pctipi, seasons = c(2L, 3L)))
    as.Date(window(pctipi, seasons = c(2L, 4L)))
    as.Date(window(pctipi, seasons = 3:4))
    
    # as.pcdate(window(pctipi, seasons = 1:2))
    # as.pcdate(window(pctipi, seasons = c(1L, 3L)))
    # as.pcdate(window(pctipi, seasons = c(1L, 4L)))
    # as.pcdate(window(pctipi, seasons = c(2L, 3L)))
    # as.pcdate(window(pctipi, seasons = c(2L, 4L)))
    # as.pcdate(window(pctipi, seasons = 3:4))

    # seqD1 <- seq(pcdate("2020-01-01", BuiltinCycle(4)), length.out = 12)
    # expect_equal(class(seqD1), c("pcdate", "Date"))


    
})
