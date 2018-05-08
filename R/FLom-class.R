#' @title S4 class \code{FLom}
#'
#' @description The \code{FLom} class stores the information relative to the operating model of the MSE.
#'
#' @section Slots:
#' \describe{
#'    \item{catch.dev}{\code{FLQuant}  with catch-at-age deviances}
#'    \item{index.qdev}{\code{FLQuants} with catchability deviances for abundance indices}
#'    \item{ctrl}{\code{mseCtrl} with control parameter for the oem}
#'  }
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name FLoem-class
#' @rdname FLoem-class
#' @aliases FLoem-class
#' @examples
#' data(ple4)
#' data(ple4.index)
#'
#' obj <- sca(stock=ple4, indices=FLIndices(ple4.index))
#' obj
#'

FLom <- setClass("FLom", 
	slots=c(
		stock="FLStock",
		sr="FLSR",
		brp="FLPar",
		fleetBehaviour="mseCtrl"
		
	)
)

#' @rdname FLom-class
#' @template bothargs
#' @aliases FLom FLom-methods
setGeneric("FLom")

setMethod("initialize", "FLom",
    function(.Object,
             ...,
             stock, sr, brp, fleetBehaviour) {
      if (!missing(stock)) .Object@stock <- stock 
      if (!missing(sr)) .Object@sr <- sr
      if (!missing(brp)) .Object@brp <- brp
      if (!missing(fleetBehaviour)) .Object@fleetBehaviour <- fleetBehaviour
      .Object <- callNextMethod(.Object, ...)
      .Object
})

setValidity("FLom",
  function(object) {
    # stk and sr must be compatible
	sd <- dim(object@stock)
	rd <- dim(object@sr@residuals)
    if (!all.equal(sd[-1], rd[-1])) "Stock and stock recruitment residuals must have the same dimensions." else TRUE

	# recruitment must be the same age
	sd <- dimnames(object@stock@stock.n)$age[1]
    rd <- dimnames(object@sr@residuals)$age[1]
    if (!all.equal(sd, rd)) "Stock and stock recruitment residuals must use the recruitment age." else TRUE
    
})

#
#  accessor methods
#

#' @rdname FLom-class
setMethod("stock", "FLom", function(object) object@stock)

#' @rdname FLom-class
setReplaceMethod("stock", signature("FLom", "FLStock"), function(object, value){
	object@stock <- value
	object
})

#' @rdname FLom-class
setMethod("sr", "FLom", function(object) object@sr)

#' @rdname FLom-class
#' @param value the new object
#' @aliases sr<- sr<--methods
setGeneric("sr<-", function(object, value) standardGeneric("sr<-"))
#' @rdname FLom-class
setReplaceMethod("sr", signature("FLom", "FLSR"), function(object, value){
	object@sr <- value
	object
})

#' @rdname FLom-class
#' @aliases brp brp-methods
setGeneric("brp", function(object, ...) standardGeneric("brp"))
#' @rdname FLom-class
setMethod("brp", "FLom", function(object) object@brp)

#' @rdname FLom-class
#' @param value the new object
#' @aliases brp<- brp<--methods
setGeneric("brp<-", function(object, value) standardGeneric("brp<-"))
#' @rdname FLom-class
setReplaceMethod("brp", signature("FLom", "FLPar"), function(object, value){
	object@brp <- value
	object
})

#' @rdname FLom-class
#' @aliases fleetBehaviour fleetBehaviour-methods
setGeneric("fleetBehaviour", function(object, ...) standardGeneric("fleetBehaviour"))
#' @rdname FLom-class
setMethod("fleetBehaviour", "FLom", function(object) object@fleetBehaviour)

#' @rdname FLom-class
#' @param value the new object
#' @aliases fleetBehaviour<- fleetBehaviour<--methods
setGeneric("fleetBehaviour<-", function(object, value) standardGeneric("fleetBehaviour<-"))
#' @rdname FLoem-class
setReplaceMethod("fleetBehaviour", signature("FLom", "mseCtrl"), function(object, value){
	object@fleetBehaviour <- value
	object
})

#' @rdname FLoem-class
setMethod("show", signature(object = "FLom"),
  function(object)
  {
    cat("Operating Model\n")

	cat("Stock:\n", summary(object@stock))

	cat("\nStock-recruitment:\n", summary(object@sr))

	cat("\nReference points:\n", object@brp)

	cat("\nFleet behaviour:\n", object@fleetBehaviour)

 })


#
# Other methods
#


