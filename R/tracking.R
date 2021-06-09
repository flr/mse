# tracking.R - DESC
# /tracking.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# tracking FLQuant / FLQuants

track <- function(list, metric, year) {
    lapply(list[[1]], "[", metric, ac(year))
}

`track<-` <- function(list, metric, year, value) {

  if(is(value, "FLQuant")) {
    list[[1]][[1]][metric, ac(year)] <- value
  
  } else if(is(value, "numeric")) {
    list[[1]][[1]][metric, ac(year)] <- value
  
  } else if(is(value, "FLQuants")) {
    for(i in names(value))
      list[[1]][[i]][metric, ac(year)] <- value[[i]]
  }
  
  return(list)
}

`add<-` <- function(list, step, value) {
  list[["control"]][[step]] <- as(value, "data.frame")
  return(list) 
}
