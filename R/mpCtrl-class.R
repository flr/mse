#' @title S4 class \code{mpCtrl}
#'
#' @description The \code{mpCtrl} class defines which moules will be runned and carries the specifications of those modules in \code{mseCtrl} elements.
#'
#' @section Slots:
#' \describe{
#'    \item{est}{\code{mseCtrl} object with estimator of stock abundance specifications.}
#'    \item{phcr}{\code{mseCtrl} object with specifications about parameters needed for harvest control rule which must e computed from estimator results, e.g. Fmsy.}
#'    \item{hcr}{\code{mseCtrl} object with harvest control rule specifications.}
#'    \item{is}{\code{mseCtrl} object with management system specifications, e.g. translation of F into catch limits.}
#'    \item{tm}{\code{mseCtrl} object with technical measures specifications.}
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name mpCtrl-class
#' @rdname mpCtrl-class
#' @aliases mpCtrl-class
#' @examples

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

#
#  accessor methods
#

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

#' @rdname mpCtrl-class
setMethod("show", signature(object = "mpCtrl"),
  function(object)
  {
    cat("Modules:\n")
    print(names(object))
 })

#' @rdname mpCtrl-class
setMethod("iters", signature(object = "mpCtrl"), function(object, iter){

	ctrl <- lapply(object, function(x) {
		lst0 <- lapply(x@args, function(y){
			if(is(y, "FLQuant")) FLCore::iter(y,iter) else y	
		})
		args(x) <- lst0
		x
	})
	mpCtrl(ctrl)
})


#
# Other methods
#

#' @rdname mpCtrl-class
#' @aliases exists,mpCtrl-method
setGeneric("exists")
#' @rdname mseCtrl-class
setMethod("exists", "mpCtrl", function(x) length(x) != 0)

#' @rdname mpCtrl-class
#' @aliases method<-,mpCtrl-method
setReplaceMethod("method", signature(object="mpCtrl", value="function"),
  function(object, element, value) {
    object[[element]]@method <- value
    return(object)
  })

#' @rdname mpCtrl-class
#' @aliases method,mpCtrl-method
setMethod("method", signature("mpCtrl"),
  function(object, element) {
    return(object[[element]]@method)
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
