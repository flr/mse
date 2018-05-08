#' @title S4 class \code{FLoem}
#'
#' @description The \code{FLoem} class stores the information relative to the observation error model of the MSE.
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

FLoem <- setClass("FLoem", 
	contains = "mseCtrl", 
	slots=c(
		observations="list",
		deviances="list" # these should be FLQuants
	),
	prototype=c( # default is perfect info
	method=function(stk, deviances, observations, vy0, ay, tracking){
		dataYears <- vy0
		assessmentYear <- ac(ay)
		stk0 <- stk[,dataYears]
		idx0 <- FLIndices(a=FLIndex(index=stock.n(stk)[]*0.01))
		range(idx0[[1]])[c("startf","endf")] <- c(0,0)
		list(stk=stk0, idx=idx0, deviances, observations, tracking=tracking)
	},
	args=NULL,
	deviances=NULL,
	observations=NULL	
	)
)

#' @rdname FLoem-class
#' @template bothargs
#' @aliases FLoem FLoem-methods
setGeneric("FLoem")

setMethod("initialize", "FLoem",
    function(.Object,
             ...,
             method, args, observations, deviances) {
      if (!missing(method)) .Object@method <- method 
      if (!missing(args)) .Object@args <- args
      if (!missing(observations)) .Object@observations <- observations
      if (!missing(deviances)) .Object@deviances <- deviances
      .Object <- callNextMethod(.Object, ...)
      .Object
})

setValidity("FLoem",
  function(object) {
    # Observations and deviances must be the same
	obs <- names(object@observations)
	dev <- names(object@deviances)
    if (all.equal(obs, dev)) "Observations and deviances must be the same." else TRUE

})

#
#  accessor methods
#

#' @rdname FLoem-class
#' @aliases observations observations-methods
setGeneric("observations", function(object, ...) standardGeneric("observations"))
#' @rdname FLoem-class
setMethod("observations", "FLoem", function(object) object@observations)

#' @rdname FLoem-class
#' @param value the new object
#' @aliases observations<- observations<--methods
setGeneric("observations<-", function(object, value) standardGeneric("observations<-"))
#' @rdname FLoem-class
setReplaceMethod("observations", signature("FLoem", "list"), function(object, value){
	object@observations <- value
	object
})

#' @rdname FLoem-class
#' @aliases deviances deviances-methods
setGeneric("deviances", function(object, ...) standardGeneric("deviances"))
#' @rdname FLoem-class
setMethod("deviances", "FLoem", function(object) object@deviances)

#' @rdname FLoem-class
#' @param value the new object
#' @aliases deviances<- deviances<--methods
setGeneric("deviances<-", function(object, value) standardGeneric("deviances<-"))
#' @rdname FLoem-class
setReplaceMethod("deviances", signature("FLoem", "list"), function(object, value){
	object@deviances <- value
	object
})

#' @rdname FLoem-class
setMethod("show", signature(object = "FLoem"),
  function(object)
  {
    cat("Observation Error Model\n")

	cat("Observations of:", names(object@observations), "\n")

	for(i in names(object@deviances)){
		cat("\nDeviances for ", i, ":\n", sep="")
		print(summary(object@deviances[[i]]))
	}
 })


#
# Other methods
#


