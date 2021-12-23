# utilities.R - DESC
# mse/R/utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# merge (FLQuant, data.table) {{{

setMethod("merge", signature(x="FLQuant", y="data.table"),
  function(x, y, by="iter", ...) {

  # CONVERT FLQ to df, all options
  xd <- as.data.frame(x, cohort=TRUE, date=TRUE)

  # MERGE by
  return(merge(xd, y, by=by))
})

# }}}

# .combinegoFish {{{
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
# }}}

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

