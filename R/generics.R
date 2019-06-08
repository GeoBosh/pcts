## TODO: pcMean is currently not exported, is this intentional with the view of removing it?
setGeneric("pcMean", def = function(object, ...){ standardGeneric("pcMean") } )
setGeneric("pcTest", def = function(x, nullmodel, nseasons, ...){ standardGeneric("pcTest") } )

modelCycle <- function(object){
    NULL
}
setGeneric("modelCycle")

## TODO: this needs change
"modelCycle<-" <- function(object, ..., value){
    ## object@modelCycle <- value
    ## object
    stop("there is no applicable method for 'modelCycle<-' with signature '", 
         class(object), "'")
}
setGeneric("modelCycle<-")

pc.stop <-   # 2014-08-24 moved from smallutil.r; 2016-03-28 moved from  pc05season.r
  function(x){
    fn <- sys.call(which=-1) # stop automatically converts fn to the function name (maybe)
    stop(fn, " not defined for objects of class ", class(x), call.=FALSE)
  }

pc.phis2 <- function(x,p) pc.stop(x)
pc.bU <-
pc.fL <- function(x,p,from=1,to=6) pc.stop(x)

setGeneric("pc.phis2")
setGeneric("pc.fL")
setGeneric("pc.bU")
