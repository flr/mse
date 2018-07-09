#' @title S4 class \code{FLmse}
#'
#' @description The \code{FLmse} class stores information relative to the MSE's management procedure'.
#'
#' @section Slots:
#' \describe{
#'    \item{om}{\code{FLom} with the operating model.}
#'    \item{tracking}{\code{FLQuant} with record of decisions made during the mp cycle.}
#'    \item{genArgs}{\code{list} with assorted arguments required to run the MSE cycle.}
#'  }
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name FLmse-class
#' @rdname FLmse-class
#' @aliases FLmse-class
#' @examples
#'

FLmse <- setClass("FLmse", contains="FLom", 
	slots=c(
		tracking="FLQuant",
		genArgs="list"
	)
)

#' @rdname FLmse-class
#' @template bothargs
#' @aliases FLmse FLmse-methods
setGeneric("FLmse")

setMethod("initialize", "FLmse",
    function(.Object,
             ...,
             stock, sr, brp, fleetBehaviour, tracking, genArgs) {
      if (!missing(stock)) .Object@stock <- stock 
      if (!missing(sr)) .Object@sr <- sr
      if (!missing(brp)) .Object@brp <- brp
      if (!missing(fleetBehaviour)) .Object@fleetBehaviour <- fleetBehaviour
      if (!missing(tracking)) .Object@tracking <- tracking
      if (!missing(genArgs)) .Object@genArgs <- genArgs
      .Object <- callNextMethod(.Object, ...)
      .Object
})

#setValidity("FLom",
#  function(object) {
#    # stk and sr must be compatible
#	sd <- dim(object@stock)
#	rd <- dim(object@sr@residuals)
#    if (!all.equal(sd[-1], rd[-1])) "Stock and stock recruitment residuals must have the same dimensions." else TRUE

#	# recruitment must be the same age
#	sd <- dimnames(object@stock@stock.n)$age[1]
#    rd <- dimnames(object@sr@residuals)$age[1]
#    if (!all.equal(sd, rd)) "Stock and stock recruitment residuals must use the recruitment age." else TRUE
#    
#})

#
#  accessor methods
#

#' @rdname FLmse-class
#' @aliases tracking tracking-methods
setGeneric("tracking", function(object, ...) standardGeneric("tracking"))
#' @rdname FLom-class
setMethod("tracking", "FLmse", function(object) object@tracking)

#' @rdname FLmse-class
#' @param value the new object
#' @aliases tracking<- tracking<--methods
setGeneric("tracking<-", function(object, value) standardGeneric("tracking<-"))
#' @rdname FLom-class
setReplaceMethod("tracking", signature("FLmse", "FLQuant"), function(object, value){
	object@tracking <- value
	object
})

#' @rdname FLmse-class
#' @aliases genArgs genArgs-methods
setGeneric("genArgs", function(object, ...) standardGeneric("genArgs"))
#' @rdname FLom-class
setMethod("genArgs", "FLmse", function(object) object@genArgs)

#' @rdname FLmse-class
#' @param value the new object
#' @aliases genArgs<- genArgs<--methods
setGeneric("genArgs<-", function(object, value) standardGeneric("genArgs<-"))
#' @rdname FLom-class
setReplaceMethod("genArgs", signature("FLmse", "list"), function(object, value){
	object@genArgs <- value
	object
})

#
# Other methods
#


