## TODO: remove workdata() later
pcts_exdata <- function(x, envir = parent.frame()){
    allnames <- c("ap", "ap7to9", "pcfr", "pcfr2to4")
    if(missing(x))
        x <- allnames
    else if(isNA(x))
        return(allnames)
    else
        stopifnot(is.character(x), length(x) > 0, all(x %in% allnames))

    wrk <- vector("list", length(allnames))
    names(wrk) <- allnames
    
    if(any(c("ap", "ap7to9") %in% x)){
        wrk$ap <- pcts(datasets::AirPassengers)
        if("ap7to9" %in% x)
            wrk$ap7to9 <- window(wrk$ap, seasons = 7:9)
    }

    ## without '::' there is a note from R CMD check
    ##     no visible binding for global variable dataFranses1996
    if(any(c("pcfr", "pcfr2to4") %in% x)){
        wrk$pcfr <- pcts(pcts::dataFranses1996)
        if("pcfr2to4" %in% x)
            wrk$pcfr2to4 <- wrk$pcfr[2:4]
    }

    ## retain only requested objects
    wrk <- wrk[x]

    list2env(wrk, envir = envir)
    x
}

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
