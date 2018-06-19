# FLom-class.R - DESC
# mse/R/FLom-class.R

# Copyright European Union, 2018
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' A class for an operating model (OM)
#'
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#' Suspendisse eu sapien vitae arcu lobortis ultrices vitae ac velit. Curabitur id 
#' 
#' @name FLom
#' @rdname FLom-class
#' @docType class
#' @aliases FLom-class
#'
#' @slot stock The population and catch history, `FLStock`.
#' @slot sr The stock-recruitment relationship, `FLSR`.
#' @slot brp The estimated reference points, `FLPar`.
#' @slot fleetBehaviour Dynamics of the fishing fleet to be used in projections, `mseCtrl`.
#'
#' @section Validity:
#'
#'   \describe{
#'     \item{stock and sr dimensions}{Dimensions 2:6 of the `stock` and `sr` slots must match.}
#'     \item{rec age}{Stock and stock recruitment residuals must use the recruitment age.}
#' }
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLom'))}
#'
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#'
#' @section Constructor:
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All unspecified slots are then created to match the
#' requirements of the class validity function.
#'
#' @section Methods:
#' Methods exist for various calculations based on values stored in the class:
#'
#' \describe{
#'     \item{METHOD}{Neque porro quisquam est qui dolorem ipsum.}
#' }
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
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

#' @rdname FLom-class
setMethod("show", signature(object = "FLom"),
  function(object)
  {
    cat("Operating Model\n")

	cat("Stock:\n", summary(object@stock))

	cat("\nStock-recruitment:\n", summary(object@sr))

	cat("\nReference points:\n", object@brp)

	cat("\nFleet behaviour:\n", object@fleetBehaviour)

 })
