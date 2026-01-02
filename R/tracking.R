# tracking.R - DESC
# mse/R/tracking.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# track<-(data.table, numeric) {{{

setReplaceMethod("track", signature(object="data.table", value="numeric"),
  function(object, step, year=dimnames(value)$year, ..., value) {

    # ADD step if missing
    object <- .addMetricDT(object, step)

    nos <- object[, length(unique(iter))] * length(year)

    # ASSIGN
    object[eval(object[, year %in% ..year & metric == step]),
      data := rep_len(value, nos)]

    return(object[])
  }
)
# }}}

# track<-(data.table, FLQuant) {{{

setReplaceMethod("track", signature(object="data.table", value="FLQuant"),
  function(object, step, year=dimnames(value)$year, ..., value) {

    # ADD step if missing
    object <- .addMetricDT(object, step)

    # ASSIGN
    object[eval(object[, year %in% ..year & metric == step]), data := c(value)]

    return(object[])
  }
)
# }}}

# track<-(data.table, fwdControl) {{{

setReplaceMethod("track", signature(object="data.table", value="fwdControl"),
  function(object, step, year=unique(value$year), ..., value) {

    # ADD step if missing
    object <- .addMetricDT(object, step)

    # ASSIGN
    object[eval(object[, year %in% ..year & metric == step]), data := value$value]

    return(object[])
  }
)

# }}}

# track<-(data.table, FLQuants) {{{

setReplaceMethod("track", signature(object="data.table", value="FLQuants"),
  function(object, step, year=dimnames(value)$year, ..., value) {

    # ADD step if missing
    object <- .addMetricDT(object, step)

    # ASSIGN
    for(i in names(value))
      object[eval(object[, biol == i & year %in% ..year & metric == step]),
        data := c(value[[i]])]
   
    return(object[])
  }
)
# }}}

# functions {{{

.addMetricDT <- function(x, step) {

  test <- step %in% .subset2(x[J(step), on='metric', mult='first', nomatch=0], 'metric')

  if(test)
    return(x)

  dtnew <- x[metric == x[1, metric],]
  dtnew[, metric := step][, data := as.numeric(NA)]
  
  return(rbindlist(list(x, dtnew)))
}
# }}}
