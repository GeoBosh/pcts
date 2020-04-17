## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pcts)
pd <- packageDescription("pcts")

## -----------------------------------------------------------------------------
ap <- pcts(AirPassengers)
ap

## -----------------------------------------------------------------------------
window(ap, start = c(1952, 3))
window(ap, end = c(1959, 2))

## -----------------------------------------------------------------------------
window(ap, seasons = 7:9)

## -----------------------------------------------------------------------------
data(dataFranses1996)
class(dataFranses1996)
colnames(dataFranses1996)
dim(dataFranses1996) # c(148, 19)

## -----------------------------------------------------------------------------
pcfr <- pcts(dataFranses1996)
colnames(pcfr)[2:3]

## -----------------------------------------------------------------------------
tipi <- dataFranses1996[ , "USTotalIPI"]
plot(tipi)

## -----------------------------------------------------------------------------
pctipi <- pcts(tipi)
pctipi <- window(pctipi, start = availStart(pctipi), end = availEnd(pctipi))
plot(pctipi)

## -----------------------------------------------------------------------------
pcfr2to3 <- pcfr[2:3]
plot(pcfr2to3)

## -----------------------------------------------------------------------------
pcfr2to2  <- pcfr[2]
pcfr2to2a <- pcfr["USTotalIPI"] # same

## -----------------------------------------------------------------------------
pcfr2 <- pcfr[[2]]
pcfr2a <- pcfr[["USTotalIPI"]] # same
pcfr2b <- pcfr$USTotalIPI      # same
identical(pcfr2, pcfr2a) # TRUE
identical(pcfr2, pcfr2b) # TRUE

c1 <- cycle(pcfr)
head(c1, 8)
frequency(pcfr)

