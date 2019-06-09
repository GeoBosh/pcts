test_that("the new periodic classes are ok",
{
    ##
    b1 <- new("BareCycle", nseasons = 1)
    b12 <- new("BareCycle", nseasons = 12)

    ts1 <- new("PeriodicTS", cycle = b1, 1:10)
    ts2 <- new("PeriodicTS", cycle = b12, AirPassengers)
    ap.ts <- new("PeriodicTS_ts", AirPassengers)
    expect_identical(S3Part(ap.ts, strictS3 = TRUE), AirPassengers)

    ap.mts <- new("PeriodicTS_ts", AirPassengers) # multivar ts with one variable is ok

    ## `z' is from help page of ts()
    z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
    z.ts  <- new("PeriodicTS", z)

    expect_error(new("PeriodicTS_ts", z),
                 'not a scalar time series; consider "PeriodicMTS_ts"')

    z.mts <- new("PeriodicMTS", z) ## TODO: something wrong here!
    z.mts <- new("PeriodicMTS_ts", z) # ok
    expect_identical(S3Part(z.mts, strictS3 = TRUE), z)

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

    cycle(z.mts)
    time(z.mts)

    expect_output(show(ap.ts) )
    expect_output(show(ap.mts))

    monthplot(ap.ts)
    monthplot(ap.mts)

    boxplot(ap.ts)
    boxplot(ap.mts)

    nTicks(ap.ts)


})
