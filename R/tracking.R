# tracking.R - DESC
# /tracking.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# track
track <- function(om, metric) {

  res <- do.call(metric, list(om))

  if(is(res, "list"))
    res <- ubind(res) 

  return(res)
}
