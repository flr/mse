#' @title S4 class \code{FLmp}
#'
#' @description The \code{FLmp} class stores information relative to the MSE's management procedure'.
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
#' @name FLmp-class
#' @rdname FLmp-class
#' @aliases FLmp-class
#' @examples
#'

FLmp <- setClass("FLmp", contains="FLom", 
	slots=c(
		tracking="FLQuant",
		genArgs="list"
	)
)

#' @rdname FLmp-class
#' @template bothargs
#' @aliases FLmp FLmp-methods
setGeneric("FLmp")

setMethod("initialize", "FLmp",
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

#' @rdname FLmp-class
#' @aliases tracking tracking-methods
setGeneric("tracking", function(object, ...) standardGeneric("tracking"))
#' @rdname FLom-class
setMethod("tracking", "FLmp", function(object) object@tracking)

#' @rdname FLmp-class
#' @param value the new object
#' @aliases tracking<- tracking<--methods
setGeneric("tracking<-", function(object, value) standardGeneric("tracking<-"))
#' @rdname FLom-class
setReplaceMethod("tracking", signature("FLmp", "FLQuant"), function(object, value){
	object@tracking <- value
	object
})

#' @rdname FLmp-class
#' @aliases genArgs genArgs-methods
setGeneric("genArgs", function(object, ...) standardGeneric("genArgs"))
#' @rdname FLom-class
setMethod("genArgs", "FLmp", function(object) object@genArgs)

#' @rdname FLmp-class
#' @param value the new object
#' @aliases genArgs<- genArgs<--methods
setGeneric("genArgs<-", function(object, value) standardGeneric("genArgs<-"))
#' @rdname FLom-class
setReplaceMethod("genArgs", signature("FLmp", "list"), function(object, value){
	object@genArgs <- value
	object
})

#
# Other methods
#


