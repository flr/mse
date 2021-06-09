# test-mp_FLom.R - DESC
# /test-mp_FLom.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(ple4om)


# args

mpargs <- list(iy=2017)

#

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=FLQuant(0.3)))))

r0 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

#

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=catchSSB.hcr, args=list(dtarget=0.40, dlimit=0.10,
  lambda=1, MSY=140000))))

r0 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

#
