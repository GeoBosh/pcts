#    expect_that( , is_identical_to(  ))
#    make_expectation( , "is_identical_to")

test_that("pclsdf works correctly",
{
    ## sintercept = TRUE, sslope = FALSE, intercept = FALSE, slope = FALSE
    expect_equal_to_reference({
         res <- pclsdf(austres[-89], 4, lags = 1:3,
                       sintercept = FALSE, sslope = FALSE, intercept = FALSE, slope = FALSE,
                       contrasts = NULL)
         res$fit <- NULL # for now, res$fit contains environments, trouble for checking...
         res
    }, "pclsdf_FFFF.rds")


    contr <- ""
    for(i in c(FALSE,TRUE))
        for(si in c(FALSE,TRUE))
            for(s in c(FALSE,TRUE))
                for(ss in c(FALSE,TRUE)){
                    nam <- paste0(ifelse(c(si,ss,i,s),"T","F"), collapse = "")
                    res <- pclsdf(austres[-89], 4, lags = 1:3,
                                  sintercept = si, sslope = ss, intercept = i, slope = s,
                                  contrasts = NULL)
                    res$fit <- NULL # for now, res$fit contains environments, 

                    expect_equal_to_reference( res, paste0("pclsdf_", nam, ".rds"))
                }
    
})





# pclsdf(austres, 4, lags = 1:3)
# pclsdf(austres, 4, lags = 1:3, sintercept = TRUE)
# pclsdf(austres, 4, lags = 1:3, sintercept = TRUE, sslope = TRUE)
# 
# x <- rep(1:4,10)
# pclsdf(x, 4, lags = 1:3, sintercept = TRUE, sslope = TRUE)
# 
# # this is for the version when contrasts arg. was passed on directly to lm.
# # tmp1 <- pclsdf(austres, 4, lags = 1, sintercept = FALSE, sslope = TRUE, contrasts = list(Season = "contr.sum" ))
# 
# contr <- ""
# for(i in c(FALSE,TRUE))
#     for(si in c(FALSE,TRUE))
#         for(s in c(FALSE,TRUE))
#             for(ss in c(FALSE,TRUE)){
#                 nam <- paste0("mo",  paste0(ifelse(c(i,si,s,ss), "T", "F"), collapse = ""),
#                               contr) 
#                 print(nam)
#                 fit <- pclsdf(austres[-89], 4, lags = 1:3, intercept = i, sintercept = si,
#                               slope = s, sslope = ss, contrasts = NULL)
#                 print(fit)
#                 assign(nam,fit)
#             }
# 
# contr <- "contr.sum"
# for(i in c(FALSE,TRUE))
#     for(si in c(FALSE,TRUE))
#         for(s in c(FALSE,TRUE))
#             for(ss in c(FALSE,TRUE)){
#                 nam <- paste0("mo",  paste0(ifelse(c(i,si,s,ss), "T", "F"), collapse = ""),
#                                      contr)
#                 print(nam)
#                 fit <- pclsdf(austres[-89], 4, lags = 1:3, intercept = i, sintercept = si,
#                               slope = s, sslope = ss, contrasts = "contr.sum")
#                 print(fit)
#                 assign(nam,fit)
#             }
# 
# contr <- "iden"
# for(i in c(FALSE,TRUE))
#     for(si in c(FALSE,TRUE))
#         for(s in c(FALSE,TRUE))
#             for(ss in c(FALSE,TRUE)){
#                 nam <- paste0("mo",  paste0(ifelse(c(i,si,s,ss), "T", "F"), collapse = ""),
#                               contr) 
#                 print(nam)
#                 fit <- pclsdf(austres[-89], 4, lags = 1:3, intercept = i, sintercept = si,
#                               slope = s, sslope = ss, contrasts = identity(4))
#                 print(fit)
#                 assign(nam,fit)
#             }
