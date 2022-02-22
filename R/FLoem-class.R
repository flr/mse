# FLoem-class.R - DESC
# mse/R/FLoem-class.R

# Copyright European Union, 2018
# Author: Ernesto JARDIM (MSC) <ernesto.jardim@msc.org>
#         Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# FLoem {{{

#' @title Specification for the observation error model (OEM).
#' @description The \code{FLoem} class stores the method, arguments and
#' observations that define the way observations are collected from an operating
#' model at each time step in the management procedure. This class extends
#' *mseCtrl* through the addition of two lists used to gather past and new
#' observations, and deviances to use on each step in the observation process.
#'
#' @slot method The function to be run in the module call, class *function*.
#' @slot args Arguments to be passed to the method, of class *list*.
#' @slot observations Past observations, class *list*.
#' @slot deviances Observation deviances, class *list*.
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name FLoem-class
#' @rdname FLoem-class
#' @examples
# TODO ADD example

FLoem <- setClass("FLoem", 
	contains = "mseCtrl", 
	slots=c(
		observations="list",
		deviances="list"      # TODO these should be FLQuants
	),
	prototype=c(
	  method=perfect.oem,
	  args=NULL,
	  observations=NULL,
  	deviances=NULL
	)
)

#' @rdname FLoem-class
#' @template bothargs
#' @aliases FLoem FLoem-methods
setGeneric("FLoem")

setMethod("initialize", "FLoem",
  function(.Object, ..., method, args, observations, deviances) {

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
  
  if (!all.equal(obs, dev))
    "Observations and deviances names must match."
  else
    TRUE
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

setReplaceMethod("observations", signature(object="FLoem", i="ANY", value="FLStock"),
  function(object, i, value){
	object@observations$stk <- value
	object
})

setReplaceMethod("observations", signature(object="FLoem", i="ANY", value="FLStocks"),
  function(object, i, value){
	object@observations$stk <- value
	object
})

setReplaceMethod("observations", signature(object="FLoem", i="ANY", value="FLIndex"),
  function(object, i, value){
	object@observations$idx <- FLIndices(value)
	object
})

setReplaceMethod("observations", signature(object="FLoem", i="ANY", value="FLIndices"),
  function(object, i, value){
	object@observations$idx <- value
	object
})


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

    # DEVIANCES, nested lapply (idx/stk - (slot) - element)
	  deviances(obj) <- lapply(deviances(obj),
      function(x) lapply(x, FLCore::iter, iter))

    # OBSERVATIONS
    if(any(c("stk", "idx") %in% names(observations(obj))))
  	  observations(obj) <- lapply(observations(obj), FLCore::iter, iter)
	  else
      observations(obj) <- lapply(setNames(nm=names(observations(obj))), function(i)
           lapply(observations(obj)[[i]], "iter", iter))

    return(obj)
})

# }}}

# combine {{{

#' @rdname FLoem-class

setMethod("combine", signature(x="FLoem", y="FLoem"),
  function(x, y, ..., check=FALSE) {

    args <- c(list(x=x, y=y), list(...))
    
    # OBSERVATIONS
    obs <- lapply(args, slot, "observations")
    
    # DEVIANCES
    devs <- lapply(args, slot, "deviances")

    # IF no observations
    if(length(obs) < 1)
      return(x)

    # NAMES to check or build on
    nms <- names(obs[[1]])

    # FLom: DOES obs[[1]] contain stk | idx? 
    if(any(c("stk", "idx") %in% nms)) {

        # FIND elements in observations
        elems <- setNames(nm=names(obs[[1]]))
        
        # COMBINE across elements
        obs <- lapply(elems, function(e)
          Reduce(combine, lapply(obs, "[[", e)))

    # FLombf
    } else {

      obs <- lapply(setNames(nm=nms), function(s) {

        # EXTRACT biol observations
        bios <- lapply(obs, "[[", s)

        # FIND elements in observations
        elems <- setNames(nm=names(bios[[1]]))
        
        # COMBINE across elements
        lapply(elems, function(e)
          Reduce(combine, lapply(bios, "[[", e)))
      })
    }

    # DEVIANCES

    # ASSIGN to x
    observations(x) <- obs

    return(x)
  }
)
# }}}

# fwdWindow {{{

setMethod("window", signature(x="FLoem"),
  function(x, ...) {

    # observations
    observations(x) <- rapply(observations(x), window,
      classes=c("FLStock", "FLIndex", "FLIndexBiomass"),
      how="replace", ...)

    # deviances
    deviances(x) <- rapply(deviances(x), window, classes=c("FLQuant"),
      how="replace",  ...)

    return(x)
  }
) # }}}

# propagate {{{
setMethod("propagate", signature(object="FLoem"),
	function(object, iter, fill.iter=TRUE) {

    # observations
    if(!is.null(observations(object)) & length(observations(object)) > 0) {
      observations(object) <- rapply(observations(object), propagate,
        how='replace', iter=iter, fill.iter=fill.iter)
    }

    # deviances
    if(!is.null(deviances(object)) & length(deviances(object)) > 0) {
      deviances(object) <- rapply(deviances(object), propagate,
        how='replace', iter=iter, fill.iter=fill.iter)
    }
    return(object)
  }
) # }}}
