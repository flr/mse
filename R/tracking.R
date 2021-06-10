# tracking.R - DESC
# /tracking.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

setGeneric("track<-", function(object, ..., value)
  standardGeneric("track<-"))

setReplaceMethod("track", signature(object="FLQuants", value="fwdControl"),
  function(object, step, year=value$year, ..., value) {

    object[[1]][step, ac(year)] <- value$value

    return(object)
  }
)

setReplaceMethod("track", signature(object="FLQuants", value="FLQuant"),
  function(object, step, year=dimnames(value)$year, ..., value) {

    object[[1]][step, ac(year)] <- c(value)

    return(object)
  }
)

setReplaceMethod("track", signature(object="FLQuants", value="numeric"),
  function(object, step, year=dimnames(value)$year, ..., value) {

    object[[1]][step, ac(year)] <- c(value)

    return(object)
  }
)
