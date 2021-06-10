# test-tracking.R - DESC
# /test-tracking.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

iy <- 1990
vy <- 1990:2000

args <- list(it=25, management_lag=1)

metric <- c("F.om", "B.om", "C.om", "C.oem", "F.est", "B.est", "C.est",
  "conv.est")

steps <- c("phcr", "hcr", "isys", "tm", "iem", "fb")

object <- FLQuants(FLQuant(NA, dimnames=list(
  metric=c(metric, steps[steps %in% names(ctrl)]),
  year=unique(c((iy - args$management_lag + 1):iy, vy)),
  iter=1:args$it)))

value <- fwdControl(year=1991, quant="fbar", value=0.15)


track(object, "hcr") <- value

value <- catch(om)[, ac(1991)]
