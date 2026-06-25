# FLmses-class.R - DESC
# /FLmses-class.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# FLmses CLASS {{{
setClass("FLmses",
  representation("FLlst", performance="data.table"))

# }}}

# FLmses() {{{

setGeneric("FLmses", function(object, performance, ...)
  standardGeneric("FLmses"))

setMethod("FLmses", signature(object="missing", performance="missing"),
  function(...) {
    args <- list(...)
    FLmses(FLlst(args))
  }
)

setMethod("FLmses", signature(object="FLmse", performance="ANY"),
  function(object, performance, ...) {

    args <- c(object, list(...))

    return(FLmses(args, performance=data.table(performance)))

  }
)

setMethod("FLmses", signature(object="list", performance="data.frame"),
  function(object, performance) {

    res <- FLmses(object)
    performance(res) <- data.table(performance)

    return(res)
  }
)

setMethod("FLmses", signature(object="list", performance="missing"),
 function(object, statistics="missing", years="missing", metrics="missing", type="NA") {

    # COMPUTE performance IF statistics
    if(!missing(statistics)) {
      # AND for years
      if(!missing(years)) {
        perf <- performance(object, statistics=statistics, years=years,
          metrics=metrics, type=type)
      } else {
        perf <- performance(object, statistics=statistics,
          metrics=metrics, type=type)
      }
    } else {

      # CHECK performance tables
      perf <- lapply(object, attr, 'performance')

      # IF all elements have 'performance' attr
      if(!any(unlist(lapply(perf, is.null)))) {

        perf <- rbindlist(perf, idcol="mp")

        # EMPTY individual tables
        object <- lapply(object, function(x) {
          attr(x, "performance") <- NULL
          return(x)
        })
      } else {
        perf <- data.table()
      }
    }
    
    res <- new("FLmses", .Data=object, performance=perf,
      names=names(object))

    return(res)
  }
)
# }}}

# c(FLmses) {{{

setMethod("c", "FLmses",
  function(x, ...) {

    # PARSE args
    args <- list(...)

  # CONVERT FLo elements to list(FLmses)
    id <- vapply(args, is, logical(1), 'FLo')

    if(any(id))
      args[id] <- Map(function(val, nm)
        setNames(list(FLmse(om=val)), nm), args[id], names(args[id]))

    # CONVERT FLmses elements to lists
    id <- vapply(args, is, logical(1), 'FLmse')

    if(any(id))
      args[id] <- Map(function(val, nm)
        setNames(list(val), nm), args[id], names(args[id]))

    # AND FLmses too
    id <- vapply(args, is, logical(1), 'FLmses')

    if(any(id))
      args[id] <- lapply(args[id], unclass)

    res <- c(unclass(x), Reduce('c', args))

    # MERGE performance, ADD run and
    res <- FLmses(res)

    return(res)
  })

# }}}

# $<- {{{
setReplaceMethod("$", signature(x="FLmses", value="FLmse"),
	function(x, name, value) {

    nms <- names(x)
    
    x@.Data[[as.character(name)]] <- value

    names(x@.Data) <- c(nms, name)

    return(x)
  }
)
# }}}

# [, [[ {{{

setMethod("[", signature(x="FLmses", i="ANY", j="missing", drop="ANY"),
  function(x, i, drop=FALSE) {

    # GET 
    if(is.numeric(i)) {
      i <- names(x)[i]
    }
    
    # SUBSET in list, need to unclass
    x@.Data <- unclass(x)[i]
    
    if(nrow(performance(x)) > 0)
      performance(x) <- x@performance[run %in% i,]

    return(x)
  }
)

# }}}

# tracking {{{
setMethod("tracking", signature(object="FLmses"),
  function(object, ...) {
    
    # COMBINE tracking from each FLmse
    track <- rbindlist(lapply(object@.Data, tracking), idcol="run")
    
    return(track)
  }
)
# }}}
