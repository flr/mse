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

# TODO: COLLATE individual performance tables

# FLmses(object=missing, ...)

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
  function(object, statistics="missing", years="missing", metrics="missing") {

    # CHECK performance tables
    perf <- lapply(object, attr, 'performance')

    # IF all elements have 'performance' attr
    if(!all(unlist(lapply(perf, is.null)))) {

      perf <- rbindlist(perf, idcol="mp")

      # EMPTY individual tables
      object <- lapply(object, function(x) {
        attr(x, "performance") <- NULL
        return(x)
      })
    } else {
      perf <- data.table()
    }
    
    res <- new("FLmses", .Data=object, performance=perf,
      names=names(object))
  
    return(res)

    # TODO: COMPUTE performance IF statistics
    if(!missing(statistics)) {
      # AND for years
      if(missing(years)) {
        years <- do.call(seq,
          unname(dims(stock(object[[1]]))[c('minyear', 'maxyear')]))
      }
      perf <- performance(object, statistics=statistics, years=years,
        metrics=metrics)
    }
  }
)
# }}}

# plot

# [, [[

# DROP mps from performance table

# 
