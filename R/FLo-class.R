# FLo-class.R - DESC
# /FLo-class.R

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

# summary {{{

setMethod("summary", signature(object="FLo"),
  function(object) {

    # refpts
    cat("-- refpts\n")
    
    for(i in names(refpts(object))) {
      cat(i, ":\n", sep="")
      print(refpts(object)[[i]], reduced=TRUE)
    }
    cat("\n")

    # fleetBehaviour
    cat("-- fleetBehaviour\n")
    behaviour <- fleetBehaviour(object)
    
    cat("Method: ", find.original.name(method(behaviour)), "\n")
    cat("Args: ", names(unlist(args(behaviour))), "\n", sep="\t")
    cat("", unlist(args(behaviour)), "\n", sep="\t")

    # projection
    cat("-- projection\n")
    projection <- projection(object)

    cat("Method: ", find.original.name(method(projection)), "\n")
    cat("Args: ", names(unlist(args(projection))), "\n", sep="\t")
    cat("", unlist(args(projection)), "\n", sep="\t")
  }
)
# }}}

# metrics {{{
setMethod("metrics", signature(object="FLo", metrics="list"),
  function(object, metrics) {
    return(lapply(metrics, function(x)
      do.call(x, list(object))))
  }
) # }}}

# find.original.name {{{

find.original.name <- function(fun) {

  # 'NULL' function
  if(is.null(formals(fun)))
     if(is.null(do.call(fun, args=list())))
       return("NULL")
  
  ns <- environment(fun)
  objects <- ls(envir = ns)
  
  if(isNamespace(ns))
    name <- getNamespaceName(ns)
  else
    name <- environmentName(ns)

  for (i in objects) {
    if (identical(fun, get(i, envir = environment(fun)))) {
        return(paste(name, i, sep="::"))                   
    }
  }
  return("NULL")
}
# }}}

setMethod("debug", signature(fun="FLo"),
  function(fun) {
    debug(method(projection(fun)))
  })

setMethod("undebug", signature(fun="FLo"),
  function(fun) {
    undebug(method(projection(fun)))
  })
