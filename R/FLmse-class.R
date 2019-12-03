# FLmse-class.R - DESC
# mse/R/FLmse-class.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# FLmse {{{

#' @title S4 class \code{FLmse}
#'
#' @description The \code{FLmse} class stores information relative to the MSE's management procedure'.
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
#' @name FLmse-class
#' @rdname FLmse-class
#' @aliases FLmse-class
#' @examples
#'

FLmse <- setClass("FLmse", contains="FLom", 
	slots=c(
		tracking="FLQuant",
    control="mpCtrl",
    oem="FLoem",
    # TODO genArgs vs. mpargs
		genArgs="list"
	)
)

#' @rdname FLmse-class
#' @template bothargs
#' @aliases FLmse FLmse-methods
setGeneric("FLmse")

setMethod("initialize", "FLmse",
    function(.Object,
             ...,
             stock, sr, refpts, fleetBehaviour, tracking, control, oem, genArgs) {
      if (!missing(stock)) .Object@stock <- stock 
      if (!missing(sr)) .Object@sr <- sr
      if (!missing(refpts)) .Object@refpts <- refpts
      if (!missing(fleetBehaviour)) .Object@fleetBehaviour <- fleetBehaviour
      if (!missing(tracking)) .Object@tracking <- tracking
      if (!missing(control)) .Object@control <- control
      if (!missing(oem)) .Object@oem <- oem
      if (!missing(genArgs)) .Object@genArgs <- genArgs
      .Object <- callNextMethod(.Object, ...)
      .Object
}) # }}}

# accessor methods {{{

# tracking

#' @rdname FLmse-class
#' @aliases tracking tracking-methods
setGeneric("tracking", function(object, ...) standardGeneric("tracking"))

#' @rdname FLmse-class
setMethod("tracking", "FLmse", function(object) object@tracking)

#' @rdname FLmse-class
#' @param value the new object
#' @aliases tracking<- tracking<--methods
setGeneric("tracking<-", function(object, value) standardGeneric("tracking<-"))

#' @rdname FLom-class
setReplaceMethod("tracking", signature("FLmse", "FLQuant"), function(object, value){
	object@tracking <- value
	object
})

# control

#' @rdname FLmse-class
#' @aliases control control-methods
setGeneric("control", function(object, ...) standardGeneric("control"))

#' @rdname FLmse-class
setMethod("control", "FLmse", function(object) object@control)

#' @rdname FLmse-class
#' @param value the new object
#' @aliases control<- control<--methods
setGeneric("control<-", function(object, value) standardGeneric("control<-"))

#' @rdname FLom-class
setReplaceMethod("control", signature("FLmse", "FLQuant"), function(object, value){
	object@control <- value
	object
})

# oem

#' @rdname FLmse-class
#' @aliases oem oem-methods
setGeneric("oem", function(object, ...) standardGeneric("oem"))

#' @rdname FLmse-class
setMethod("oem", "FLmse", function(object) object@oem)

#' @rdname FLmse-class
#' @param value the new object
#' @aliases oem<- oem<--methods
setGeneric("oem<-", function(object, value) standardGeneric("oem<-"))

#' @rdname FLom-class
setReplaceMethod("oem", signature("FLmse", "FLQuant"), function(object, value){
	object@oem <- value
	object
})

# genArgs

#' @rdname FLmse-class
#' @aliases genArgs genArgs-methods
setGeneric("genArgs", function(object, ...) standardGeneric("genArgs"))

#' @rdname FLmse-class
setMethod("genArgs", "FLmse", function(object) object@genArgs)

#' @rdname FLmse-class
#' @param value the new object
#' @aliases genArgs<- genArgs<--methods
setGeneric("genArgs<-", function(object, value) standardGeneric("genArgs<-"))

#' @rdname FLmse-class
setReplaceMethod("genArgs", signature("FLmse", "list"), function(object, value){
	object@genArgs <- value
	object
})




# }}}

# plot {{{
setMethod("plot", signature(x="FLmse", y="missing"),
  function(x, ...) {
    plot(stock(x), ...)
  }
) 

setMethod("plot", signature(x="FLom", y="FLmse"),
  function(x, y, ...) {

    args <- list(...)

    stocks <- lapply(c(list(x, y), args), stock)
 
    # SORT OUT names
    if(is.null(names(stocks)))
      names(stocks) <- rep(character(1), length(stocks))
    idx <- names(stocks) == character(1)
    names(stocks)[idx] <- unlist(lapply(stocks, name))[idx]
    idx <- names(stocks) == character(1)
    names(stocks)[idx] <- c("OM", paste0("MP", seq(sum(idx))))[idx]

    plot(FLStocks(stocks))
  }
)


# }}}

# summary {{{
setMethod("summary", signature(object="FLmse"),
  function(object) {

    callNextMethod()

    cat("\n")

    # tracking
    cat("-- tracking\n")
    tracking <- yearMeans(tracking(object))
 
    # CONVERT dimnames$year to range 
    ydns <- dimnames(tracking(object))$year
    dimnames(tracking)$year <- paste(c(ydns[1], ydns[length(ydns)]), collapse="-")

    cat(show(tracking), "\n")

    # control
    cat("-- control\n")
    control <- object@control

    for(i in names(control)) {
      cat(paste0(i, ":"), "\n")
      cat("\tMethod: ", find.original.name(method(control[[i]])), "\n")
    }

    # oem
    cat("-- oem\n")
    oem <- oem(object)

    # TODO
    show(oem)
    
    # genArgs
    cat("-- genArgs\n")
    genArgs <- genArgs(object)

    cat(
      paste0(names(genArgs), unname(unlist(lapply(genArgs, function(x) {
        if(length(x) == 1) paste(":", x)
        else paste(":", paste(x[1], x[length(x)], sep="-"))
      }))), collapse=", "), "\n")


  }
) # }}}
