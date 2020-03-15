## code to prepare `nsadata.csv` dataset goes here

## TODO: document and rename, see Lina's thesis

## usethis::use_data("Experimental/nsadata.csv")

datansa <- read.csv("nsadata.csv")
datansa <- ts(datansa, start = c(1919, 1), frequency = 4)

## now `datansa` is a ts, so this doesn't work:
## nsaauto_old <- ts(datansa$AUTOMOTIVEPRODNSA[113:328], start = c(1947, 1), frequency = 4)
##
## this does: 
##    tmp <- datansa[ , "AUTOMOTIVEPRODNSA"][113:328]
##    nsaauto_old <- ts(tmp, start = c(1947, 1), frequency = 4)
## but it can be done also this way:
nsaauto <- window(datansa[ , "AUTOMOTIVEPRODNSA"], start = c(1947, 1))
## identical(nsaauto, nsaauto_old) # TRUE
