#' @title S4 class \code{mpCtrl}
#'
#' @description The \code{mpCtrl} class defines which moules will be runned and carries the specifications of those modules in \code{mseCtrl} elements.
#'
#' @section Slots:
#' \describe{
#'    \item{ctrl.est}{\code{mseCtrl} object with estimator of stock abundance specifications.}
#'    \item{ctrl.phcr}{\code{mseCtrl} object with specifications about parameters needed for harvest control rule which must e computed from estimator results, e.g. Fmsy.}
#'    \item{ctrl.hcr}{\code{mseCtrl} object with harvest control rule specifications.}
#'    \item{ctrl.is}{\code{mseCtrl} object with management system specifications, e.g. translation of F into catch limits.}
#'    \item{ctrl.tm}{\code{mseCtrl} object with technical measures specifications.}
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
#' @aliases ctrl.est ctrl.est-methods
setGeneric("ctrl.est", function(object, ...) standardGeneric("ctrl.est"))
#' @rdname mpCtrl-class
setMethod("ctrl.est", "mpCtrl", function(object) object$ctrl.est)

#' @rdname mpCtrl-class
#' @aliases ctrl.est<- ctrl.est<--methods
setGeneric("ctrl.est<-", function(object, value) standardGeneric("ctrl.est<-"))
#' @rdname mpCtrl-class
setReplaceMethod("ctrl.est", signature("mpCtrl", "function"), function(object, value){
	object$ctrl.est <- value
	object
})

#' @rdname mpCtrl-class
#' @aliases ctrl.phcr ctrl.phcr-methods
setGeneric("ctrl.phcr", function(object, ...) standardGeneric("ctrl.phcr"))
#' @rdname mpCtrl-class
setMethod("ctrl.phcr", "mpCtrl", function(object) object$ctrl.phcr)

#' @rdname mpCtrl-class
#' @aliases ctrl.phcr<- ctrl.phcr<--methods
setGeneric("ctrl.phcr<-", function(object, value) standardGeneric("ctrl.phcr<-"))
#' @rdname mpCtrl-class
setReplaceMethod("ctrl.phcr", signature("mpCtrl", "function"), function(object, value){
	object$ctrl.phcr <- value
	object
})

#' @rdname mpCtrl-class
#' @aliases ctrl.hcr ctrl.hcr-methods
setGeneric("ctrl.hcr", function(object, ...) standardGeneric("ctrl.hcr"))
#' @rdname mpCtrl-class
setMethod("ctrl.hcr", "mpCtrl", function(object) object$ctrl.hcr)

#' @rdname mpCtrl-class
#' @aliases ctrl.hcr<- ctrl.hcr<--methods
setGeneric("ctrl.hcr<-", function(object, value) standardGeneric("ctrl.hcr<-"))
#' @rdname mpCtrl-class
setReplaceMethod("ctrl.hcr", signature("mpCtrl", "function"), function(object, value){
	object$ctrl.hcr <- value
	object
})


#' @rdname mpCtrl-class
#' @aliases ctrl.is ctrl.is-methods
setGeneric("ctrl.is", function(object, ...) standardGeneric("ctrl.is"))
#' @rdname mpCtrl-class
setMethod("ctrl.is", "mpCtrl", function(object) object$ctrl.is)

#' @rdname mpCtrl-class
#' @aliases ctrl.is<- ctrl.is<--methods
setGeneric("ctrl.is<-", function(object, value) standardGeneric("ctrl.is<-"))
#' @rdname mpCtrl-class
setReplaceMethod("ctrl.is", signature("mpCtrl", "function"), function(object, value){
	object$ctrl.is <- value
	object
})

#' @rdname mpCtrl-class
#' @aliases ctrl.tm ctrl.tm-methods
setGeneric("ctrl.tm", function(object, ...) standardGeneric("ctrl.tm"))
#' @rdname mpCtrl-class
setMethod("ctrl.tm", "mpCtrl", function(object) object$ctrl.tm)

#' @rdname mpCtrl-class
#' @aliases ctrl.tm<- ctrl.tm<--methods
setGeneric("ctrl.tm<-", function(object, value) standardGeneric("ctrl.tm<-"))
#' @rdname mpCtrl-class
setReplaceMethod("ctrl.tm", signature("mpCtrl", "function"), function(object, value){
	object$ctrl.tm <- value
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
		if(!is.null(x)) {
			lst0 <- lapply(x@args, function(y){
				if(is(y, "FLQuant")) FLCore::iter(y,iter) else y	
			})
			args(x) <- lst0
		}
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

