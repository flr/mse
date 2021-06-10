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
#'    \item{args}{\code{list} with assorted arguments required to run the MSE cycle.}
#'  }
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name FLmse-class
#' @rdname FLmse-class
#' @aliases FLmse-class
#' @examples
#'

FLmse <- setClass("FLmse",
	slots=c(
    om="FLo",
		tracking="FLQuants",
    control="mpCtrl",
    oem="FLoem",
    # TODO args vs. mpargs
		args="list"
	)
)

#' @rdname FLmse-class
#' @template bothargs
#' @aliases FLmse FLmse-methods
setGeneric("FLmse")

setMethod("initialize", "FLmse",
    function(.Object,
             ...,
             om, tracking, control, oem, args) {
      if (!missing(om)) .Object@om <- om 
      if (!missing(tracking)) .Object@tracking <- tracking
      if (!missing(control)) .Object@control <- control
      if (!missing(oem)) .Object@oem <- oem
      if (!missing(args)) .Object@args <- args
      .Object <- callNextMethod(.Object, ...)
      .Object
}) # }}}

# accessor methods {{{

# om

#' @rdname FLmse-class
#' @aliases om om-methods
setGeneric("om", function(object, ...) standardGeneric("om"))

#' @rdname FLmse-class
setMethod("om", "FLmse", function(object) object@om)

#' @rdname FLmse-class
#' @param value the new object
#' @aliases om<- om<--methods
setGeneric("om<-", function(object, value) standardGeneric("om<-"))

#' @rdname FLom-class
setReplaceMethod("om", signature("FLmse", "FLo"), function(object, value){
	object@oem <- value
	object
})

# tracking

#' @rdname FLmse-class
#' @aliases tracking tracking-methods
setGeneric("tracking", function(object, ...) standardGeneric("tracking"))

#' @rdname FLmse-class
setMethod("tracking", "FLmse", function(object) {
  if(length(object@tracking) == 1)
    return(object@tracking[[1]])
  else
    return(object@tracking)
  }
)

#' @rdname FLmse-class
#' @param value the new object
#' @aliases tracking<- tracking<--methods
setGeneric("tracking<-", function(object, ..., value) standardGeneric("tracking<-"))

#' @rdname FLom-class
setReplaceMethod("tracking", signature("FLmse", "FLQuants"),
  function(object, value) {
	  object@tracking <- value
  	object
})

setReplaceMethod("tracking", signature("FLmse", "FLQuant"),
  function(object, biol, value) {
    if(length(object@tracking) == 1)
  	  object@tracking[[1]] <- value
    else
  	  object@tracking[[biol]] <- value

  	object
})

# control

#' @rdname FLmse-class
#' @aliases control control-methods
setGeneric("control", function(object, ...) standardGeneric("control"))

#' @rdname FLmse-class
setMethod("control", "FLmse",
  function(object, i="missing") {
    if(missing(i))
      return(object@control)
    else
      return(object@control[[i]])
  })

#' @rdname FLmse-class
#' @param value the new object
#' @aliases control<- control<--methods
setGeneric("control<-", function(object, ..., value) standardGeneric("control<-"))

#' @rdname FLom-class
setReplaceMethod("control", signature("FLmse", "mpCtrl"),
  function(object, value) {
	  object@control <- value
	  object
})

setReplaceMethod("control", signature("FLmse", "mseCtrl"),
  function(object, i, value) {
	  object@control[[i]] <- value
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
setReplaceMethod("oem", signature("FLmse", "FLoem"), function(object, value){
	object@oem <- value
	object
})

# args

#' @rdname FLmse-class
setMethod("args", "FLmse", function(name) name@args)

#' @rdname FLmse-class
setReplaceMethod("args", signature("FLmse", "list"), function(object, value){
	object@args <- value
	object
})

# }}}

# om accessors {{{

setMethod("refpts", signature(object="FLmse"),
  function(object) {
    return(refpts(om(object)))
  }
)

setMethod("sr", signature(object="FLmse"),
  function(object) {
    return(sr(om(object)))
  }
) # }}}

# plot {{{
setMethod("plot", signature(x="FLmse", y="missing"),
  function(x, ...) {
    # PLOT om
    plot(om(x), ...)
  }
) 

setMethod("plot", signature(x="FLom", y="FLmse"),
  function(x, y, ...) {

    args <- list(...)
    fms <- unlist(lapply(args, is, "FLmse"))

    stocks <- lapply(c(list(x, om(y)), args[fms]), stock)

    # WINDOW om
    minyear <- min(unlist(lapply(stocks[-1], function(x) dims(x)$minyear)))
    stocks[[1]] <- window(stocks[[1]], end=minyear)

    # SORT OUT names
    if(is.null(names(stocks)))
      names(stocks) <- rep(character(1), length(stocks))
    idx <- names(stocks) == character(1)
    if(length(idx) > 0)
      names(stocks)[idx] <- unlist(lapply(stocks, name))[idx]
    idx <- names(stocks) == character(1)
    if(length(idx) > 0)
      names(stocks)[idx] <- c("OM", paste0("MP", seq(sum(idx)-1)))[idx]
 
    # PLOT FLStocks + args
    nfs <- unlist(lapply(args, function(x) !is(x, "FLmse")))
    do.call("plot", c(list(x=FLStocks(stocks)), args[nfs]))
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
    # TODO tracking <- yearMeans(tracking(object))
 
    # CONVERT dimnames$year to range 
    # ydns <- dimnames(tracking(object))$year
    # dimnames(tracking)$year <- paste(c(ydns[1], ydns[length(ydns)]), collapse="-")

    # cat(show(tracking), "\n")

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
    
    # args
    cat("-- args\n")
    args <- args(object)

    cat(
      paste0(names(args), unname(unlist(lapply(args, function(x) {
        if(length(x) == 1) paste(":", x)
        else paste(":", paste(x[1], x[length(x)], sep="-"))
      }))), collapse=", "), "\n")


  }
) # }}}

# metrics {{{

setMethod("metrics", signature(object="FLmse", metrics="ANY"),
  function(object, metrics) {
    metrics(om(object), metrics)
})

setMethod("metrics", signature(object="FLmse", metrics="missing"),
  function(object) {
    metrics(om(object))
}) # }}}
