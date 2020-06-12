## TODO: pcMean is currently not exported, is this intentional with the view of removing it?
setGeneric("pcTest", def = function(x, nullmodel, nseasons, ...){ standardGeneric("pcTest") } )

setGeneric("pcMean", def = function(object, ...){ standardGeneric("pcMean") } )
## 2020-06-11 new
setGeneric("pcIntercept", def = function(object, ...){ standardGeneric("pcIntercept") } )
setGeneric("pcApply", def = function(object, ...){ standardGeneric("pcApply") } )

modelCycle <- function(object){
    NULL
}
setGeneric("modelCycle")

## "modelCycle<-" <- function(object, ..., value){
##     ## object@modelCycle <- value
##     ## object
##     stop("there is no applicable method for 'modelCycle<-' with signature '", 
##          class(object), "'")
## }
setGeneric("modelCycle<-", function(object, ..., value){ standardGeneric("modelCycle<-") })

## Periodic Levinson-Durbin related generics.
## *TODO:* should think of better names for these functions.
setGeneric("pc.phis2", function(x, p){ standardGeneric("pc.phis2") })
setGeneric("pc.fL", function(x, p, from = 1, to = 6){ standardGeneric("pc.fL") })
setGeneric("pc.bU", function(x, p, from = 1, to = 6){ standardGeneric("pc.bU") })
