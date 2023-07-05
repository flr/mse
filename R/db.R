# db.R - DESC
# /db.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

#' Creates a single table of output from an FLmse object
#'
#' @param object Input object with MSE results
' @param metrics List of metrics to be computed on relevant slots
#'
#' @return A `data.table` object

setGeneric("db", function(object, ...) standardGeneric("db"))

setMethod("db", signature(object="FLmse"),
  function(object, metrics=list(SB=ssb, Rec=rec, F=fbar, C=catch, B=stock)) {

  # EXTRACT metrics(stock)
  ms <- data.table(as.data.frame(metrics(stock(object), metrics=metrics),
    drop=FALSE, date=TRUE))
  setnames(ms, "qname", "metric")

  # EXTRACT refpts
  rp <- data.table(as.data.frame(refpts(object)))
  setnames(rp, "params", "metric")

  # REPEAT by years
  yrs <- unique(ms$year)
  drp <- dim(rp)[1]

  rp <- rp[rep(seq(drp), length(yrs)),]
  rp[, year:=rep(yrs, each=drp)]

  # EXTRACT tracking
  tr <- data.table(as.data.frame(tracking(object), drop=TRUE))

  # OUTPUT
  out <- rbindlist(list(ms, tr, rp), use.names=TRUE, fill=TRUE)

  # DROP unused dims
  ddi <- c("age", "unit", "season", "area", "date")
  dix <- dim(stock(object))[c(1, 3, 4, 5, 3)] == 1
  dip <- ddi[dix]
  out[, (dip) := NULL]
 
  # CHANGE character NA to string(1)  
  dop <- ddi[!dix]
  for (j in dop)
    set(out, which(is.na(out[[j]])), j, "all")

  return(out)
  }
)
