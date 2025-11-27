# mseCtrl.R - DESC
# mse/R/mseCtrl-class.R

# Copyright European Union, 2018-2021
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# mseCtrl class {{{

#' @title S4 class \code{mseCtrl}
#'
#' @description The \code{mseCtrl} class stores information about how a specific
#' module will be run. The function contained in the *method* slot will be
#' called with three sets of argument: those contained in the *args* slot, the
#' *args* argument to the call to the *mp* function, and the inputs defined by
#' type of module being defined by a particular object. Please see the "Module
#' dispatch" section in the *mse* Technical Manual.
#'
#' @slot method The function to be run in the module call, class *function*.
#' @slot args Arguments to be passed to the method, of class *list*.
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name mseCtrl-class
#' @rdname mseCtrl-class
#' @aliases mseCtrl-class
#' @examples
#' ctl <- mseCtrl(method=function(stk, args, alpha) ssb(stk) * alpha,
#'   args=list(alpha=0.5))
#' ctl

mseCtrl <- setClass("mseCtrl",
    slots = c(method = "function",
    args   = "list"))

#' @rdname mseCtrl-class
#' @template bothargs
#' @aliases mseCtrl mseCtrl-methods
setMethod("initialize", "mseCtrl",
    function(.Object,
             ...,
             method, args) {
      if (!missing(method))
        .Object@method <- switch(class(method),
          "function"=method,
          "character"=get(method))
      if (!missing(args)) .Object@args <- args
      .Object <- callNextMethod(.Object, ...)
      .Object
    })

#
#  accessor methods
#

#' @rdname mseCtrl-class
#' @aliases method method-methods
setGeneric("method", function(object, ...) standardGeneric("method"))

#' @rdname mseCtrl-class
#' @examples
#' method(ctl)
setMethod("method", "mseCtrl", function(object) object@method)

#' @rdname mseCtrl-class
#' @aliases method<- method<--methods
setGeneric("method<-", function(object, ..., value) standardGeneric("method<-"))

#' @rdname mseCtrl-class
#' @examples
#' method(ctl) <- function(stk, args, beta) ssb(stk) * beta
setReplaceMethod("method", signature("mseCtrl", "function"), function(object, value){
	object@method <- value
	object
})

#' @rdname mseCtrl-class
#' @aliases args args-methods
setGeneric("args", useAsDefault = base::args)

#' @rdname mseCtrl-class
#' @examples
#' args(ctl)
setMethod("args", "mseCtrl", function(name) name@args)

#' @rdname mseCtrl-class
#' @aliases args<- args<--methods
setGeneric("args<-", function(object, ..., value) standardGeneric("args<-"))

#' @rdname mseCtrl-class
#' @examples
#' args(ctl) <- list(beta=0.9)
setReplaceMethod("args", signature("mseCtrl", "list"), function(object, value){
	object@args <- value
	object
})

# }}}

# show {{{

#' @rdname mseCtrl-class
setMethod("show", signature(object = "mseCtrl"),
  function(object)
  {
    cat("Method:\n")
    print(object@method)

    cat("Arguments:\n")
    print(object@args)
 }) # }}}

# exists {{{

#' @rdname mseCtrl-class
#' @aliases exists,mseCtrl-method
setGeneric("exists")

#' @rdname mseCtrl-class
#' @examples
#' exists(ctl)

setMethod("exists", "mseCtrl", function(x) !is.null(body(x@method)))
# }}}

# debug & undebug {{{

#' @rdname debug-mse

setMethod("debug", signature(fun="mseCtrl", text="missing"),
  function(fun) {
    debug(fun@method)
  }
)

#' @rdname debug-mse

setMethod("undebug", signature(fun="mseCtrl", signature="missing"),
  function(fun) {
    debug(fun@method)
  }
)
# }}}

# iter (mseCtrl) {{{
setMethod("iter", signature(obj = "mseCtrl"),
  function(obj, iter) {
  
  args(obj) <- lapply(obj@args, function(x) {
			if(is(x, "FLQuant") | is(x, "FLQuants") | is(x, 'FLPar'))
        FLCore::iter(x, iter)
      else
        x
		})

	do.call(class(obj), list(obj))
})
# }}}
