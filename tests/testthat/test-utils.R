context("utils")

test_that("functions in utils.R are ok", {
    ## todo: not sure what to do here. Can't throw error since 'methods'
    ##       makes calls with no arguments
    bc <- new("BareCycle")
    expect_equal(nSeasons(bc), integer(0))
    
    ## expect_output(.reportClassName(bc, "BareCycle"))
    ## expect_silent(.reportClassName(bc, "SimpleCycle"))

    ls_before <- ls()
    names.exdata <- pcts_exdata(NA)
    expect_type(names.exdata, "character")
    
    val1 <- pcts_exdata()
    expect_equal(val1, names.exdata)
    expect_true( all(names.exdata %in% ls()) )
    rm(list = names.exdata)

    val2 <- pcts_exdata(c("ap7to9", "pcfr2to4"))
    expect_equal(val2, c("ap7to9", "pcfr2to4"))
    expect_true( all(c("ap7to9", "pcfr2to4") %in% ls()) )
    expect_false( any(c("ap", "pcfr") %in% ls()) )
})
