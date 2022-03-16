# tracking.R - DESC
# /tracking.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# track<- FLQuants, fwdControl{{{

#' @rdname tracking
#' @examples
#' tracking <- FLQuants(FLQuant(dimnames=list(metric="hcr",
#'   year=1990:1992, iter=1:10), units=""))
#' track(tracking, "hcr", 1990) <- fwdControl(year=1990, quant="fbar", value=0.15)
#' tracking

setReplaceMethod("track", signature(object="FLQuants", value="fwdControl"),
  function(object, step, year=value$year, iter=seq(dim(object[[1]])[6]), ..., value) {

    # SINGLE stock
    if(length(unique(value$biol)) == 1) {
      object[[1]][step, ac(year),,,, iter] <- value@iters[1, 'value',]

    # MULTIPLE stocks
    } else {

      # FIND biols in control with value
      ids <- unique(value$biol)

      for(i in ids[!is.na(ids)]) {
        object[[i]][step, ac(year),,,, iter] <- value@iters[value$biol == i & !is.na(value$value), 'value', ]
      }
    }

    return(object)
  }
)

# }}}

# track<- FLQuants, FLQuant {{{

#' @rdname tracking
#' @examples
#' tracking <- FLQuants(
#'   A=FLQuant(dimnames=list(metric="conv.est", year=1990:1992), units=""),
#'   B=FLQuant(dimnames=list(metric="conv.est", year=1990:1992), units=""))
#' track(tracking, "conv.est", 1990) <- FLQuants(A=FLQuant(0), B=FLQuant(1))
#' tracking

setReplaceMethod("track", signature(object="FLQuants", value="FLQuant"),
  function(object, step, year=dimnames(value)$year, ..., value) {

    # CHECK step exists
    if(!step %in% dimnames(object[[1]])[[1]])
      object <- lapply(object, function(x)
        expand(x, metric=c(dimnames(x)$metric, step)))

    object[[1]][step, ac(year)] <- c(value)

    return(object)
  }
)
# }}}

# track<- FLQuants, numeric{{{

setReplaceMethod("track", signature(object="FLQuants", value="numeric"),
  function(object, step, year=dimnames(value)$year, ..., value) {

    # CHECK step exists
    if(!step %in% dimnames(object[[1]])[[1]])
      object <- lapply(object, function(x)
        expand(x, metric=c(dimnames(x)$metric, step)))

    if(length(object) == 1)
      object[[1]][step, ac(year)] <- c(value)
    else {
      len <- length(object)
      value <- rep(value, length=len)
      for(i in seq(len))
        object[[i]][step, ac(year)] <- c(value[i])
    }

    return(object)
  }
)
# }}}

# track<- FLQuants, FLQuants{{{

#' @rdname tracking
#' @examples
#' # When tracking multiple stocks
#' tracking <- FLQuants(
#'   A=FLQuant(dimnames=list(metric="conv.est", year=1990:1992), units=""),
#'   B=FLQuant(dimnames=list(metric="conv.est", year=1990:1992), units=""))
#' # FLQuants
#' track(tracking, "conv.est", 1990) <- FLQuants(A=FLQuant(0), B=FLQuant(1))
#' tracking
#' # numeric
#' track(tracking, "conv.est", 1990) <- 3
#' tracking
#' track(tracking, "conv.est", 1990) <- c(1,2)
#' tracking
#' # fwdControl
#' track(tracking, "conv.est", 1990) <- fwdControl(
#'   list(year=1990, biol=1, quant="fbar", value=0.20),
#'   list(year=1990, biol=2, quant="fbar", value=0.18))
#' tracking

setReplaceMethod("track", signature(object="FLQuants", value="FLQuants"),
  function(object, step, year=dimnames(value)$year, ..., value) {
    
    # CHECK step exists
    if(!step %in% dimnames(object[[1]])[[1]])
      object <- lapply(object, function(x)
        expand(x, metric=c(dimnames(x)$metric, step)))
    
    for(i in names(object))
      object[[i]][step, ac(year)] <- value[[i]]

    return(object)
  }
)

# }}}

# track<- FLQuant, numeric{{{

setReplaceMethod("track", signature(object="FLQuant", value="numeric"),
  function(object, step, year=dimnames(value)$year, ..., value) {

    # CHECK step exists
    if(!step %in% dimnames(object[[1]])[[1]])
      object <- expand(object, metric=c(dimnames(object)$metric, step))
    object[step, ac(year)] <- c(value)

    return(object)
  }
)
# }}}

