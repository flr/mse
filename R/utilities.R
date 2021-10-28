# utilities.R - DESC
# /utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# merge {{{

setGeneric("merge", useAsDefault = base::merge)

setMethod("merge", signature(x="FLQuant", y="data.table"),
  function(x, y, by="iter", ...) {

  # CONVERT FLQ to df, all options
  xd <- as.data.frame(x, cohort=TRUE, date=TRUE)

  # MERGE by
  return(merge(xd, y, by=by))
})

# }}}

.combinegoFish <- function(...) {
  
  res <- list(...)
	
  return(
  list(
    om = Reduce("combine", lapply(res, '[[', 1)),
    tracking = Reduce("combine", lapply(res, '[[', 2)),
    oem = Reduce("combine", lapply(res, '[[', 3))
		)
  )
} 



