#' @title S4 class \code{mseCtrl}
#'
#' @description The \code{mseCtrl} class stores information about how a specific module will be run.
#'
#' @section Slots:
#' \describe{
#'    \item{method}{\code{character} with the name of the method to be run. Note a function of method must exist in the environment with the same name.}
#'    \item{args}{\code{list} with arguments to be passed to the function defined in \code{method}}
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name mseCtrl-class
#' @rdname mseCtrl-class
#' @aliases mseCtrl-class
#' @examples

mseCtrl <-
  setClass("mseCtrl",
           slots = c(method = "function",
                     args   = "list"))

#' @rdname mseCtrl-class
#' @template bothargs
#' @aliases mseCtrl mseCtrl-methods
setMethod("initialize", "mseCtrl",
    function(.Object,
             ...,
             method, args) {
      if (!missing(method)) .Object@method <- method
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
setMethod("method", "mseCtrl", function(object) object@method)

#' @rdname mseCtrl-class
#' @aliases method<- method<--methods
setGeneric("method<-", function(object, value) standardGeneric("method<-"))
#' @rdname mseCtrl-class
setReplaceMethod("method", signature("mseCtrl", "function"), function(object, value){
	object@method <- value
	object
})

#' @rdname mseCtrl-class
#' @aliases args args-methods
setGeneric("args", useAsDefault = base::args)
#' @rdname mseCtrl-class
setMethod("args", "mseCtrl", function(name) name@args)

#' @rdname mseCtrl-class
#' @aliases args<- args<--methods
setGeneric("args<-", function(object, value) standardGeneric("args<-"))
#' @rdname mseCtrl-class
setReplaceMethod("args", signature("mseCtrl", "list"), function(object, value){
	object@args <- value
	object
})

#' @rdname mseCtrl-class
setMethod("show", signature(object = "mseCtrl"),
  function(object)
  {
    cat("Method:\n")
    print(object @ method)

    cat("Arguments:\n")
    print(object @ args)
 })

#
# Other methods
#

#' @rdname mseCtrl-class
#' @aliases exists,mseCtrl-method
setGeneric("exists")
#' @rdname mseCtrl-class
setMethod("exists", "mseCtrl", function(x) !is.null(body(x@method)))

