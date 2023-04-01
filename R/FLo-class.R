# FLo-class.R - DESC
# mse/R/FLo-class.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# FLo: OM parent class {{{

setClass("FLo",
  slots=c(
    name="character",
    fleetBehaviour="mseCtrl",
    projection="mseCtrl"))

# }}}

# accessors methods {{{

#' @rdname FLo-class

setMethod("name", "FLo", function(object) object@name)

#' @rdname FLo-class
#' @param value Object to assign in slot

setReplaceMethod("name", signature("FLo", "character"), function(object, value){
	object@name <- value
	object
})

#' @rdname FLo-class

setMethod("refpts", "FLo", function(object) object@refpts)

#' @rdname FLo-class
#' @param value Object to assign in slot

setReplaceMethod("refpts", signature("FLo", "FLPar"), function(object, value){
	object@refpts <- value
	object
})

#' @rdname FLo-class

setGeneric("fleetBehaviour", function(object, ...) standardGeneric("fleetBehaviour"))

#' @rdname FLo-class

setMethod("fleetBehaviour", "FLo", function(object) object@fleetBehaviour)

#' @rdname FLo-class
#' @param value Object to assign in slot

setGeneric("fleetBehaviour<-", function(object, value) standardGeneric("fleetBehaviour<-"))

#' @rdname FLo-class

setReplaceMethod("fleetBehaviour", signature("FLo", "mseCtrl"), function(object, value){
	object@fleetBehaviour <- value
	object
})

#' @rdname FLo-class

setGeneric("projection", function(object, ...) standardGeneric("projection"))

#' @rdname FLo-class

setMethod("projection", "FLo", function(object) object@projection)

#' @rdname FLo-class
#' @param value Object to assign in slot

setGeneric("projection<-", function(object, value) standardGeneric("projection<-"))

#' @rdname FLo-class

setReplaceMethod("projection", signature("FLo", "mseCtrl"), function(object, value){
	object@projection <- value
	object
})

# }}}

# show {{{
setMethod('show', signature('FLo'),
  function(object) {
    summary(object)
  }
) # }}}

# metrics {{{
setMethod("metrics", signature(object="FLo", metrics="list"),
  function(object, metrics) {
    return(FLQuants(lapply(metrics, function(x)
      do.call(x, list(object)))))
  }
)


# }}}

# debug {{{

#' @rdname debug-mse
#' @details For objects of classes *FLom* and *FLombf*, *debug* and *undebug* will set
#' and unset the debugging flag on the function stored in the *projection* slot.

setMethod("debug", signature(fun="FLo"),
  function(fun) {
    debug(method(projection(fun)))
  })

#' @rdname debug-mse

setMethod("undebug", signature(fun="FLo"),
  function(fun) {
    undebug(method(projection(fun)))
  })
# }}}

# fwd.om {{{

#' A method to project the operating model (OM)
#'
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#' Suspendisse eu sapien vitae arcu lobortis ultrices vitae ac velit. Curabitur id 
#'
#' @name fwd.om
#' @rdname fwd.om
#' @aliases fwd.om
#' @param object the OM as a FLStock
#' @param ctrl the fwdControl object with objectives and constraints
#' @param ... 
 
fwd.om <- function(om, ctrl, ...){
	
  args <- list(...)

	args$object <- om
	args$control <- ctrl

  # TODO ADD tryCatch
  om <- tryCatch(do.call("fwd", args),
    # error, RETURN om TODO: track
    error = function(e) {
      stop()
    }
  )

	list(om=om)
}
# }}}

# relative (metrics) {{{

relative <- list(
  `SB/SB[MSY]`=function(x) setunits(unitSums(ssb(x)) %/% refpts(x)$SBMSY, ""),
  `SB/SB[0]`=function(x) setunits(unitSums(ssb(x)) %/% refpts(x)$SB0, ""),
  `B/B[0]`=function(x) setunits(unitSums(tsb(x)) %/% refpts(x)$B0, ""),
  `F/F[MSY]`=function(x) setunits(unitMeans(fbar(x)) %/% refpts(x)$FMSY, "")) 

# }}}

# total (metrics) {{{

total <- list(
  R = function(x) areaSums(unitSums(rec(x))),
  SB = function(x) areaSums(unitSums(ssb(x))),
  C = function(x) areaSums(unitSums(catch(x))),
  F = function(x) areaMeans(unitMeans(fbar(x))))

# }}}
