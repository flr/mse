# FLmses-class.R - DESC
# /FLmses-class.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# CLASS {{{
setClass("FLmses",
  representation("FLlst", performance="data.table"))

# }}}

# FLmses() constructor

setGeneric("FLmses", function(object, performance, ...)
  standardGeneric("FLmses"))

setMethod("FLmses", signature(object="list", performance="missing"),
  function(object, statistics="missing", years="missing", metrics="missing") {

    # COMPUTE performance IF statistics
    if(!missing(statistics)) {
      # years
      if(missing(years)) {
        years <- do.call(seq,
          unname(dims(stock(object[[1]]))[c('minyear', 'maxyear')]))
      }
      perf <- performance(object, statistics=statistics, years=years,
        metrics=metrics)
    }

    # 

  }
)

#setMethod("FLmses", signature(object="FLmse", performance="missing"),
#  function(object, statistics="missing", years="missing", metrics="missing") {

# plot

# [, [[

# 
