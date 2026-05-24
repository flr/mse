# tracking.R - DESC
# mse/R/tracking.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# track<-(data.table, numeric) {{{

setReplaceMethod("track", signature(object="data.table", value="numeric"),
  function(object, step, year=dimnames(value)$year, biol=1, ..., value) {

    # ADD step if missing
    object <- .addMetricDT(object, step)

    # COMPUTE number of elements
    nos <- object[, length(unique(iter))] * length(year)

    # GET biol name
    bion <- ifelse(is.numeric(biol), object[, unique(biol)][biol], stock)

    # ASSIGN
    object[eval(object[, biol == bion & year %in% ..year & metric == step]),
      data := rep_len(value, nos)]

    return(object[])
  }
)
# }}}

# track<-(data.table, FLQuant) {{{

setReplaceMethod("track", signature(object="data.table", value="FLQuant"),
  function(object, step, year=dimnames(value)$year, biol=1, ..., value) {

    # ADD step if missing
    object <- .addMetricDT(object, step)

    # GET biol name
    bion <- ifelse(is.numeric(biol), object[, unique(biol)][biol], stock)

    # ASSIGN
    object[eval(object[, biol == bion & year %in% ..year & metric == step]),
      data := c(value)]

    return(object[])
  }
)
# }}}

# track<-(data.table, fwdControl) {{{

setReplaceMethod("track", signature(object="data.table", value="fwdControl"),
  function(object, step, year=unique(value$year), biol=1, ..., value) {

    # ADD step if missing
    object <- .addMetricDT(object, step)

    # SET biol
    if(!is.na(value$biol[1])) {
      biol <- value$biol[1]
    }

    # GET biol name
    bion <- ifelse(is.numeric(biol), object[, unique(biol)][biol], stock)

    # SUBSET first year
    value <- value[value$year == min(value$year),]

    # IF fisheries, merge by biol
    if(all(!is.na(value$fishery))) {
      # AGGREGATE
      if(value$quant[1] %in% c("catch", "landings", "discards", "effort"))
        value <- apply(iters(value), 2:3, sum, na.rm=TRUE)['value',]
      else
        value <- apply(iters(value), 2:3, mean, na.rm=TRUE)['value',]
    } else {
      value <- value$value
    }

    # ASSIGN
    object[eval(object[, biol == bion & year %in% ..year & metric == step]), data := value]

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

# .functions {{{

# .addMetricDT: Adds a metric if not included
.addMetricDT <- function(x, step) {

  test <- step %in% .subset2(x[J(step), on='metric', mult='first', nomatch=0], 'metric')

  if(test)
    return(x)

  dtnew <- x[metric == x[1, metric],]
  dtnew[, metric := step][, data := as.numeric(NA)]
  
  return(rbindlist(list(x, dtnew)))
}
# }}}
