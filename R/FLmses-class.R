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

    # IF args not all FLmse, RETURN list
    cls <- unlist(lapply(args, is, 'FLmse'))
    if(!all(cls)) {
      return(c(unclass(x), args))
    }

    # GET arg names
    argnms <- sys.call()
    nams <- as.character(argnms)[-1]

    # .Data
    data <- c(x@.Data, unlist(lapply(args, "slot", ".Data")))

    names(data) <- unlist(c(list(names(x)), lapply(args, names)))

    # MERGE performance, ADD run and
    perf <- rbindlist(c(list(performance(x)), lapply(args, performance)))

    res <- FLmses(data, performance=perf)

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
