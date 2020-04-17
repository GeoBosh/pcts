context("utils")

test_that("functions in utils.R are ok", {
    ## todo: not sure what to do here. Can't throw error since 'methods'
    ##       makes calls with no arguments
    bc <- new("BareCycle")
    expect_equal(nSeasons(bc), integer(0))
    
    ## expect_output(.reportClassName(bc, "BareCycle"))
    ## expect_silent(.reportClassName(bc, "SimpleCycle"))
})
