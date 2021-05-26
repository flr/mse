# FLoem-class.R - DESC
# mse/R/FLoem-class.R

# Copyright European Union, 2018
# Author: Ernesto JARDIM (MSC) <ernesto.jardim@msc.org>
#         Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# FLoem {{{

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
#' data(cod)
#'

FLoem <- setClass("FLoem", 
	contains = "mseCtrl", 
	slots=c(
		observations="list",
		deviances="list" # these should be FLQuants
	),
	prototype=c( # default is perfect info
	method=function(stk, deviances, observations, args, tracking){
		dataYears <- ac(args$y0:args$dy)
		assessmentYear <- ac(args$ay)
		stk0 <- stk[,dataYears]
		idx0 <- FLIndices(a=FLIndex(index=stock.n(stk)[,dataYears]*0.01))
		range(idx0[[1]])[c("startf","endf")] <- c(0,0)
		list(stk=stk0, idx=idx0, deviances=deviances, observations=observations, tracking=tracking)
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

# }}}

#  accessor methods {{{

#' @rdname FLoem-class
#' @aliases observations observations-methods
setGeneric("observations", function(object, ...) standardGeneric("observations"))

#' @rdname FLoem-class

setMethod("observations", "FLoem",
  function(object, ...) {
    
    res <- object@observations

    # If extra args, subset lists
    args <- list(...)

    if(length(args) > 0) {
      for(i in args)
        res <- res[[i]]
    }

    return(res)
  })

#' @rdname FLoem-class
#' @param value the new object
#' @aliases observations<- observations<--methods

setGeneric("observations<-", function(object, i, value) standardGeneric("observations<-"))

#' @rdname FLoem-class

setReplaceMethod("observations", signature(object="FLoem", i="missing", value="list"),
  function(object, value){
	object@observations <- value
	object
})

#' @rdname FLoem-class

setReplaceMethod("observations", signature(object="FLoem", i="ANY", value="ANY"),
  function(object, i, value){
	object@observations[[i]] <- value
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

# }}}

# show {{{

#' @rdname FLoem-class

setMethod("show", signature(object = "FLoem"),
  function(object)
  {
    cat("Observation Error Model\n")

	cat("Observations of:", names(object@observations), "\n")

	for(i in names(object@deviances)){
		cat("\nDeviances for ", i, ":\n", sep="")
		cat(summary(object@deviances[[i]]), "\n")
	}
 })
# }}}

# iter {{{

#' @rdname FLoem-class

setMethod("iter", signature(obj = "FLoem"),
  function(obj, iter){
    # TODO Delve deeper into list
	  deviances(obj) <- lapply(deviances(obj), FLCore::iter, iter)
	  observations(obj) <- lapply(observations(obj), FLCore::iter, iter)
	  obj
})

# }}}

# combine {{{

#' @rdname FLoem-class

setMethod("combine", signature(x = "FLoem", y = "FLoem"), function(x, y, ...){
	
  args <- c(list(x, y), list(...))

	if(length(args) > 2) {
		return(combine(combine(x, y), ...))
	} else {
		obj <- x
		dev <- deviances(obj)
		obs <- observations(obj)
		for(i in 1:length(dev))
      dev[[i]] <- combine(deviances(x)[[i]], deviances(y)[[i]])
		for(i in 1:length(obs))
      obs [[i]] <- combine(observations(x)[[i]], observations(y)[[i]])
		dev -> deviances(obj)
		obs -> observations(obj)
		return(obj)
	}
})
# }}}
