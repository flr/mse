#' @title S4 class \code{FLiem}
#'
#' @description The \code{FLiem} class stores the information relative to the implementation error model of the MSE.
#'
#' @section Slots:
#' \describe{
#'    \item{method}{\code{character} with the name of the method to be run. Note a function of method must exist in the environment with the same name.}
#'    \item{args}{\code{list} with arguments to be passed to the function defined in \code{method}}
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name FLiem-class
#' @rdname FLiem-class
#' @aliases FLiem-class
#' @examples

FLiem <- setClass("FLiem", contains = "mseCtrl")

#' @rdname FLiem-class
#' @template bothargs
#' @aliases FLiem FLiem-methods
setMethod("initialize", "FLiem",
    function(.Object, ...) {
      .Object <- callNextMethod(.Object, ...)
      .Object
    })

#' @rdname FLiem-class
setMethod("iters", signature(object = "FLiem"),
  function(object, iter) {

  args(object) <- lapply(object@args, function(x) {
			if(is(x, "FLQuant")) FLCore::iter(x, iter) else x
		})

	do.call(class(object), list(object))
})

setMethod("iters", signature(object = "NULL"),
  function(object, iter) {
    return(NULL)
  })
