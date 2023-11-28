test_that("the new periodic classes are ok",
{
    ##
    b1 <- new("BareCycle", nseasons = 1)
    b12 <- new("BareCycle", nseasons = 12)

    ts1 <- new("PeriodicTS", cycle = b1, 1:10)
    ts2 <- new("PeriodicTS", cycle = b12, AirPassengers)
    ap.ts <- new("PeriodicTS_ts", AirPassengers)
    expect_identical(S3Part(ap.ts, strictS3 = TRUE), AirPassengers)

    expect_error(pcts(AirPassengers, nseasons = 4, keep = TRUE),
                 "please change the frequency of the ts object or use keep = FALSE" )
    
    expect_output(show(ts1))
    plot(ts1)
    
    ap.mts <- new("PeriodicTS_ts", AirPassengers) # multivar ts with one variable is ok

    ## `z' is from help page of ts()
    m <- matrix(rnorm(300), 100, 3)
    z <- ts(m, start = c(1961, 1), frequency = 12)

    pcts(1:10, nseasons = 2)
    expect_error(pcts(1:10), "nseasons is missing and cannot be inferred")

    
    pcts.m <- pcts(m, nseasons = 4)
    expect_error(pcts(m), "nseasons is missing and cannot be inferred")

    ## 2020-04-19: not an error any more
    ## expect_error(pcts.m[1, ],
    ##         "use x\\[\\]\\[i, \\] or x\\[\\]\\[i,j\\] if you wish to use matrix indexing")
    expect_identical(pcts.m[1, ], pcts.m[1, 1:ncol(m)])
    
    ## TODO: maybe need to check validity
    ##       Then these would give error (number of seasons is integer(0)
    z.ts  <- new("PeriodicTS", z) ## this creates an invalid object, similar to below
    z.mts <- new("PeriodicMTS", z) ## TODO: something wrong here! doesn't set nseasons
                                   ##       and the show() method throws error

    expect_error(new("PeriodicTS_ts", z),
                 'not a scalar time series; consider "PeriodicMTS_ts"')

    as(z, "PeriodicMTS") 
    expect_error(as(z, "PeriodicTS"),
                 "the time series is multivariate")

    as(z[ , 1], "PeriodicMTS") 
    as(z[ , 1], "PeriodicTS")

    ## artificially create 'mts' with one time series
    z1 <- ts(matrix(1:12, ncol = 1), frequency = 4, class = c("mts", "ts", "matrix"))
    z1.ts <- as(z1, "PeriodicTS")
    window(z1.ts)
    window(z1.ts, start = c(1,2))
    window(z1.ts, start = c(1,2), end = c(3,3))
    window(z1.ts,                 end = c(3,3))

    window(z1.ts, start = c(1,2), end = c(2,1)) <- NA

    zm <- ts(matrix(1:36, ncol = 3), frequency = 4, class = c("mts", "ts", "matrix"))
    zm.ts <- as(zm, "PeriodicMTS")
    window(zm.ts)
    window(zm.ts, start = c(1,2))
    window(zm.ts, start = c(2,1), end = c(3,4))
    window(zm.ts, start = c(1,2), end = c(3,3))
    window(zm.ts,                 end = c(3,3))

    window(zm.ts, start = c(1,2), end = c(2,1)) <- NA
    
    z.ts <- pcts(z[ , 1]) # ok
    z.mts <- pcts(z) # ok
    z.mts_ts <- new("PeriodicMTS_ts", z) # ok
    expect_identical(S3Part(z.mts_ts, strictS3 = TRUE), z)

    expect_output(show(window(z.ts, start = c(1967, 1))))
    
    z.mts_ts_keep <- pcts(z, keep = TRUE)
    expect_error(pcts(z, nseasons = 5, keep = TRUE),
                 "please change the frequency of the ts object or use keep = FALSE")

    z.ts_ts_keep <- pcts(z[ , 1], keep = TRUE)
    expect_error(pcts(z[ , 1], nseasons = 5, keep = TRUE),
                 "please change the frequency of the ts object or use keep = FALSE")

    pcts(AirPassengers, nseasons = 12, keep = TRUE)
    pcts(z, nseasons = 12, keep = TRUE)
        
    as(AirPassengers, "PeriodicTS")
    as(AirPassengers, "PeriodicTS_ts")
    pcts(AirPassengers)

    as(AirPassengers, "PeriodicTS")
    as(AirPassengers, "PeriodicTS_ts")

    ## as(AirPassengers, "PeriodicMTS")
    as(AirPassengers, "PeriodicMTS_ts")
    as(z, "PeriodicMTS_ts")

    new("PeriodicTS_ts", 1:12, frequency = 4)
    new("PeriodicTS_ts", matrix(1:12, ncol = 1), frequency = 4)
    expect_error(new("PeriodicTS_ts", matrix(1:24, ncol = 2), frequency = 4),
                 "not a scalar time series")

    new("PeriodicMTS_ts", 1:12, frequency = 4)
    new("PeriodicMTS_ts", matrix(1:12, ncol = 1), frequency = 4)

    cycle(z.mts_ts)
    time(z.mts_ts)

    expect_output(show(ap.ts) )
    expect_output(show(ap.mts))
    expect_output(show(z.mts))
    pcts(AirPassengers)

    monthplot(ap.ts)
    monthplot(ap.mts)
    monthplot(z.mts)

    boxplot(ap.ts)
    boxplot(ap.mts)
    boxplot(z.mts)

    nTicks(ap.ts)
    frequency(ap.ts)
    deltat(ap.ts)

    as.matrix(z.mts)

    pcts(AirPassengers)# missing nseasons, gets it from the object
    ## pcts(1:12) # this probably should issue warning

    expect_error(pcts(1:12), "nseasons is missing and cannot be inferred")
    pcts(1:12, nseasons = 4)
    pcts(1:12, nseasons = 4, start = c(2020, 1))
    pcts(1:12, nseasons = 4, start = c(2020, 2))
    pcts(1:12, nseasons = 4, start = "2020-04-01")

    pcts(1:12, nseasons = BuiltinCycle(4))
    pcts(1:12, nseasons = BuiltinCycle(4), start = c(2020, 1))
    pcts.seq12 <- pcts(1:12, nseasons = BuiltinCycle(4), start = c(2020, 2))
    pcts(1:12, nseasons = BuiltinCycle(4), start = "2020-04-01")

    expect_output( show(pcts.seq12) )
    cycle(pcts.seq12)
    
    pcts(as.matrix(z))
    
    Vec(z)
    tsVector(z)
    tsMatrix(z)
    ## pcArray(z)
    pcArray(ap.ts)
    pctsArray(ap.ts)

    z[] ## ts
    z.mts[]
    expect_identical(z.mts[], z.mts@.Data)           # ...MTS
    expect_identical(z.mts[[1]][], z.mts[[1]]@.Data) # ...TS

    identical(z.mts$"Series 1", z.mts[[1]])

    summary(z.mts)

    pcCycle(z.mts)
    pcCycle(z.mts, "")
    pcCycle(z.mts, "SimpleCycle")

    pcCycle(z[ , 1], "")
    pcCycle(z[ , 1], type = "SimpleCycle")

    pcCycle(as(z.mts, "Cyclic"))
    
    start(ap.ts)
    end(ap.ts)
    
    ## 2020-04-14: defined these methods for cyclic
    ##
    ## expect_error(nTicks(new("Cyclic", cycle = pcCycle(4))),
    ##              "unable to find an inherited method for function")
    ## expect_error(end(new("Cyclic", cycle = pcCycle(4))),
    ##              "unable to find an inherited method for function")
    pcc4 <- new("Cyclic", cycle = pcCycle(4))
    expect_equal(nTicks(pcc4), 1)
    expect_equal(start(pcc4), c(1, 1))
    expect_equal(end(pcc4), c(1, 1))
    expect_equal(cycle(pcc4), 1)
    
    expect_equal(nTicks(1:12), 12)
    expect_equal(nTicks(matrix(1:12, nrow = 6)), 6)

    ap <- pcts(AirPassengers)
    window(ap, start = c(1958, 1))
    window(ap, end = c(1958, 1))
    window(ap, start = c(1958, 1),  end = c(1960, 1))
    summary(ap)

    window(ap, seasons = 1:3)
    
    ap7to9 <- window(ap, seasons = 7:9)
    expect_equal(allSeasons(ap7to9), c("July", "August", "September"))
    unitSeason(ap7to9)
    unitCycle(ap7to9)
    ap7to9@cycle[1:2]
    ap7to9@cycle[1:2, abb = TRUE]

    ## removed
    ## start2pc_time(c(1,1), ap7to9@cycle)
    ## start2pc_time("1954-08-01", ap7to9@cycle) # TODO: process properly


    
    expect_output(show(ap7to9))
    
    head(z.mts)
    tail(z.mts)

    availStart(z.mts)
    availEnd(z.mts)

    plot(z.mts)
    
    ## TODO: this gives an error  in pcMatrix(z.mts[1]) since  nTicks is not a multiple of
    ##       the number of seasons
    ## plot(z.mts[1])

    as(z.mts[[1]], "ts")

    ## added after setting coerce method from PeriodicTS and PeriodicMTS to "Cyclic",
    ##     see the comments in PeriodicTSClasses.org
    pcfr <- pcts(dataFranses1996)
    expect_identical(as(pcfr, "Cyclic"), as(pcfr[[1]], "Cyclic"))

    pcfr2to4 <- pcfr[2:4]
    window(pcfr2to4, seasons = 1:2)

    expect_equivalent(pcMean(ap), pcMean(ap@.Data, nseasons = 12))
    expect_equivalent(pcMean(pcfr2to4, na.rm = TRUE),
                      pcMean(pcfr2to4@.Data, nSeasons(pcfr2to4), na.rm = TRUE) )

    ## drop first row to test non-full year
    pcMean(pcfr2to4@.Data[-1, ], nSeasons(pcfr2to4), na.rm = TRUE)

    expect_equal(pc_apply(ap@.Data, 12, mean), pc_mean(ap@.Data, 12))
    expect_equivalent(pc_apply(ap@.Data, 12, median), pcApply(ap@.Data, 12, median))

    ## argument nseasons is not used for PeriodicTS objects
    expect_error(pcApply(ap, 12, median),
                 "object 'FUN' of mode 'function' was not found")
    pcApply(ap, median)
    
    pcApply(pcfr2to4, median) # NA's
    expect_equivalent(pcApply(pcfr2to4@.Data, 4, median, na.rm = TRUE),
                      pcApply(pcfr2to4, median, na.rm = TRUE) )
    

    pct1990_Q3 <- Pctime(c(1990, 3), pcCycle(pcfr2to4))
    expect_identical(pcfr2to4[as_date("1990-07-01")], pcfr2to4[pct1990_Q3])

    ap_num <- as.numeric(AirPassengers)
    pcts(ap_num, 12)               # generic 12 seasons
    pcts(ap_num, BuiltinCycle(12)) # months
    pcts(ap_num, BuiltinCycle(12), start = c(1949, 1)) # months

    pcts(as.data.frame(dataFranses1996), nseasons = 4)
    df_mat <- as.matrix(dataFranses1996)
    pcts(df_mat[ , 1:3], 4)               # generic 4 seasons
    pcts(df_mat[ , 1:3], BuiltinCycle(4)) # quarters
    pcts(df_mat[ , 1:3], BuiltinCycle(4), start = c(1955, 1) ) # quarters

    expect_equal(AirPassengers[c(1, 143, 144)],
                 ap[c(1, 143, 144)] )
    expect_equal(ap[as.Date("1960-11-01")], 390)
    expect_equal(ap[Pctime(c(1949, 1960, 1960, 1, 11,12), pcCycle(ap))],
                 c(112, 390, 432) )
    expect_equal(ap[Pctime(c(1949, 1960, 1960, 1, 11,12), pcCycle(ap))],
                 ap[c(1, 143, 144)] )

    ## 1990 Q4
    expect_equal(pcfr2to4[as.Date("1990-10-01")],                  # Date
                 pcfr2to4[Pctime(c(1990, 4), pcCycle(pcfr2to4))] ) # Pctime

    expect_equal(pcfr2to4[as.Date("1990-10-01")],                  # matix indexing
                 pcfr2to4[144, ] )
    pcfr2to4[Pctime(c(1990, 4), pcCycle(pcfr2to4)), 1:2] # 1st two variables

    tsVec(pcfr2to4)
    
    dell <- pcts(four_stocks_since2016_01_01$DELL)
    dell[as.Date("2020-04-17")]
    dim(four_stocks_since2016_01_01$DELL) # [1] 923   6
    dim(dell) # [1] 958   6
    expect_equal(dell[as.Date("2020-04-17")], dell[958,])

    ## a subseries which starts and ends wtih NA's
    pcpres <- window(pcts(presidents), end = c(1972, 4))
    
    availStart(pcpres) # 1945 2
    availEnd(pcpres)   # 1972 2

    both <- na.trim(pcpres) # same as "both"
    expect_identical(na.trim(pcpres), both)
    expect_identical(na.trim(pcpres, "left"), window(pcpres, start = availStart(pcpres)))
    expect_identical(na.trim(pcpres, "right"), window(pcpres, end = availEnd(pcpres)))

    
    cguk <- pcfr[c("CanadaUnemployment", "GermanyGNP", "UKTotalInvestment")]
    
    availStart(cguk)
    availStart(cguk, TRUE)
    expect_identical(availStart(cguk), availStart(cguk, TRUE))
    availStart(cguk, FALSE)

    availEnd(cguk)
    availEnd(cguk, TRUE)
    expect_identical(availEnd(cguk), availEnd(cguk, TRUE))
    availEnd(cguk, FALSE)

    both <- na.trim(cguk) # same as "both"
    expect_identical(na.trim(cguk), both)
    expect_identical(na.trim(cguk, "left"), window(cguk, start = availStart(cguk, FALSE)))
    expect_identical(na.trim(cguk, "right"), window(cguk, end = availEnd(cguk, FALSE)))
})

