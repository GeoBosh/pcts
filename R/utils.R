## taken from "pcts"
.reportClassName <- function(object, class, trailer = "\n"){
    if(class(object) == class)
        cat('An object of class "', class, '"', trailer, sep = "")
    NULL
}

## ## s - character string.
## ## v - numeric vector.
## ## none - character string.
## .formatNameNumeric <- function(s, v, none = " <None>"){
##     if(length(v) == 0)
##         paste0(s, none)
##     else{
##         wrk <- paste0(v, collapse=" ")
##         paste0(s, wrk)
##     }
## }

## .capturePrint <- function(x){ # used by show() methods to print polynomials
##     capture.output(print(x))
## }

## # stats:::format.perc() is not exported, copy it here.
## .stats.format.perc <- function (probs, digits)
##     paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), "%")
