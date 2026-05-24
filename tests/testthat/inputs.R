# data.R - DESC
# /home/mosqu003/Active/mse_FLR/mse/tests/testthat/data.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(plesim)

# FLom
flom <- window(om, end=2025)

# FLmse
control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=14000, target=0.18))))

flmse <- mp(flom, control=control, args=list(iy=2000))

# FLmses
control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=12000, target=0.12))))

flmses <- FLmses(A=flmse, B=mp(flom, control=control, args=list(iy=2000)))

# FLStock
fls <- stock(window(om, end=2025))

# FLStocks
flss <- FLStocks(lapply(c(0.76, 1.26), function(x) fwd(fls, sr=sr(om),
  fbar=fbar(fls)[,-1] * x)))

# FLQuants
flqs <- metrics(flom)

# SAVE
save(flom, flmse, flmses, fls, flss, flqs, file="inputs.rda", compress="xz")

