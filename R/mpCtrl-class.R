# mpCtrl.R - DESC
# mse/R/mpCtrl-class.R

# Copyright European Union, 2018-2021
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# mpCtrl class {{{

#' @title S4 class \code{mpCtrl}
#'
#' @description The \code{mpCtrl} class defines which modules will be run my a
#' call to the `mp` function. It contains a series of objects of class *mseCtrl*
#' only for those modules required by the defined MP.
#'
#' @slot est Specification for the stock status estimator, class *mseCtrl*.
#' @slot phcr Specification for the harvest control rule parametrization, class *mseCtrl*.
#' @slot hcr Specification for the harvest control rule, class *mseCtrl*.
#' @slot isys Specification for the implementation system, class *mseCtrl*.
#' @slot tm Specification for technical measures, class *mseCtrl*.
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name mpCtrl-class
#' @rdname mpCtrl-class
#' @examples
#' mpCtrl(list(
#'   est = mseCtrl(method=perfect.sa),
#'   hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
#'   trigger=41500, target=0.27))))

mpCtrl <- setClass("mpCtrl", contains="list")

#' @rdname mpCtrl-class
#' @template bothargs
#' @aliases mpCtrl mpCtrl-methods

setMethod("initialize", "mpCtrl",
    function(.Object, ...) {
      .Object <- callNextMethod(.Object, ...)
      .Object
    })

# TODO
# validity
# initialize with ...

#' @rdname mpCtrl-class
#' @aliases est est-methods
setGeneric("est", function(object, ...) standardGeneric("est"))

#' @rdname mpCtrl-class
setMethod("est", "mpCtrl", function(object) object$est)

#' @rdname mpCtrl-class
#' @aliases est<- est<--methods
setGeneric("est<-", function(object, value) standardGeneric("est<-"))

#' @rdname mpCtrl-class
setReplaceMethod("est", signature("mpCtrl", "function"), function(object, value){
	object$est <- value
	object
})

#' @rdname mpCtrl-class
#' @aliases phcr phcr-methods
setGeneric("phcr", function(object, ...) standardGeneric("phcr"))

#' @rdname mpCtrl-class
setMethod("phcr", "mpCtrl", function(object) object$phcr)

#' @rdname mpCtrl-class
#' @aliases phcr<- phcr<--methods
setGeneric("phcr<-", function(object, value) standardGeneric("phcr<-"))

#' @rdname mpCtrl-class
setReplaceMethod("phcr", signature("mpCtrl", "function"), function(object, value){
	object$phcr <- value
	object
})

#' @rdname mpCtrl-class
#' @aliases hcr hcr-methods
setGeneric("hcr", function(object, ...) standardGeneric("hcr"))

#' @rdname mpCtrl-class
setMethod("hcr", "mpCtrl", function(object) object$hcr)

#' @rdname mpCtrl-class
#' @aliases hcr<- hcr<--methods
setGeneric("hcr<-", function(object, value) standardGeneric("hcr<-"))

#' @rdname mpCtrl-class
setReplaceMethod("hcr", signature("mpCtrl", "function"), function(object, value){
	object$hcr <- value
	object
})

#' @rdname mpCtrl-class
#' @aliases isys isys-methods
setGeneric("isys", function(object, ...) standardGeneric("isys"))

#' @rdname mpCtrl-class
setMethod("isys", "mpCtrl", function(object) object$isys)

#' @rdname mpCtrl-class
#' @aliases isys<- isys<--methods
setGeneric("isys<-", function(object, value) standardGeneric("isys<-"))

#' @rdname mpCtrl-class
setReplaceMethod("isys", signature("mpCtrl", "function"), function(object, value){
	object$isys <- value
	object
})

#' @rdname mpCtrl-class
#' @aliases tm tm-methods
setGeneric("tm", function(object, ...) standardGeneric("tm"))

#' @rdname mpCtrl-class
setMethod("tm", "mpCtrl", function(object) object$tm)

#' @rdname mpCtrl-class
#' @aliases tm<- tm<--methods
setGeneric("tm<-", function(object, value) standardGeneric("tm<-"))

#' @rdname mpCtrl-class
setReplaceMethod("tm", signature("mpCtrl", "function"), function(object, value){
	object$tm <- value
	object
})

# }}}

# show {{{

#' @rdname mpCtrl-class
#' @examples
#' mpCtrl(list(
#'   est = mseCtrl(method=perfect.sa),
#'   hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
#'   trigger=41500, target=0.27))))
setMethod("show", signature(object = "mpCtrl"),
  function(object) {

    cat("An object of class 'mpCtrl'\n\n")

    Map(function(mod, modname) {
    
      # module name
      cat(paste("Module: ", modname), "\n")
      
      # method
      cat(paste("  method:", find.original.name(method(mod))), "\n")
      
      # args
      cat(paste("  args:"), "\n")
      
      # args
      if(length(args(mod)) > 0) {
        idx <- unlist(lapply(args(mod), is, "FLArray"))

        # numeric
        if(any(!idx)) {
          nargs <- args(mod)[!idx]
          cat(paste0("     ", paste(paste(names(nargs), nargs, sep=" = "),
            collapse=", "), "."), "\n")
        }

        # FLQuant: dim, year range
        if(any(idx)) {
          qargs <- args(mod)[idx]

          Map(function(arg, argname) {
            cat(paste0("    ", argname, " (", class(arg)  ,"): [",
              paste(dim(arg), collapse=" "), "]"), "\n")
          }, arg=qargs, argname=names(qargs))
        }
      }
    }, mod=object, modname=names(object))

 }) # }}}

# iters {{{

#' @rdname mpCtrl-class
setMethod("iters", signature(object = "mpCtrl"), function(object, iter){

	ctrl <- lapply(object, function(x) {
		lst0 <- lapply(x@args, function(y){
			if(is(y, "FLQuant")) FLCore::iter(y, iter) else y	
		})
		args(x) <- lst0
		x
	})
	mpCtrl(ctrl)
}) # }}}

# iter {{{

#' @rdname mpCtrl-class
setMethod("iter", signature(obj = "mpCtrl"), function(obj, iter){
  ctrl <- lapply(obj, iter, iter=iter)
  return(mpCtrl(ctrl))
}) # }}}

# exists {{{

#' @rdname mseCtrl-class
setGeneric("exists", useAsDefault = base::exists)
setMethod("exists", "mpCtrl", function(x) length(x) != 0)
# }}}

# Element accessors {{{

#' @rdname mpCtrl-class
#' @aliases method,mpCtrl-method
setMethod("method", signature("mpCtrl"),
  function(object, element) {
    return(object[[element]]@method)
  })

#' @rdname mpCtrl-class
#' @aliases method<-,mpCtrl-method
setReplaceMethod("method", signature(object="mpCtrl", value="function"),
  function(object, element, value) {
    object[[element]]@method <- value
    return(object)
  })

#' @rdname mpCtrl-class
#' @aliases args<-,mpCtrl-method
setReplaceMethod("args", signature(object="mpCtrl", value="function"),
  function(object, element, value) {
    object[[element]]@args <- value
    return(object)
  })

# DEBUG base::args does not have ...
#' @rdname mpCtrl-class
#' @aliases method,mpCtrl-method
#setMethod("args", signature("mpCtrl"),
#  function(name, element) {
#    return(name[[element]]@args)
#  })
# }}}

# debug & undebug  {{{

#' @rdname debug-mse
#' Objects of calss mpCtrl contain one or more modules (mseCtrl). The module to
#' debug is specified as a character argument matching its name, e.g.
#' *debug(mpCtrl, "est")*.

setMethod("debug", signature(fun="mpCtrl", text="character"),
  function(fun, text) {
    debug(fun[[text]]@method)
  }
)

#' @rdname debug-mse

setMethod("undebug", signature(fun="mpCtrl", signature="character"),
  function(fun, signature) {
    undebug(fun[[signature]]@method)
  }
)

#' @rdname debug-mse
#' @details Calling *undebug* on an mpCtrl without specifying a module will check for
#' the debugging status of each of them, and undebug if TRUE.

setMethod("undebug", signature(fun="mpCtrl", signature="missing"),
  function(fun) {
    for(i in names(fun)) {
      if(isdebugged(fun[[i]]@method))
        undebug(fun[[i]]@method)
    }
  }
)
# }}}
