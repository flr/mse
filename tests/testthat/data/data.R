# flom.R - DESC
# /home/mosqu003/Active/mse_FLR/mse/tests/testthat/data/flom.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# --- FLom {{{

# LOAD data

data(plesim)

# SET control
control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=14000, target=0.16))))

# DO run, run3

run <- mp(om, oem=oem, ctrl=control, args=list(iy=2025))

run3 <- mp(om, oem=oem, ctrl=control, args=list(iy=2025, frq=3))

# DO runs

plan(multicore, workers=5)

runs <- mps(om, oem=oem, ctrl=control, args=list(iy=2025),
  hcr=list(target=seq(0.14, 0.18, length=5)))

# SAVE
save(om, run, run3, runs, file="flom.rda", compress="xz")
# }}}

# --- FLombf {{{

load("ombf.rda")

om <- ombf

# SET control
control <- mpCtrl(list(
  # EST
  est = mseCtrl(method=perfect.sa),
  # HCR
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0, output="catch",
    trigger=mean(refpts(om)$SBMSY) * 1.1, target=mean(refpts(om)$MSY))),
  # ISYS
  isys=mseCtrl(method=split.is, args=list(split=c(0.10, 0.70, 0.15, 0.05)))
))

# DO run, run3

run <- mp(om, ctrl=control, args=list(iy=2025))

run3 <- mp(om, ctrl=control, args=list(iy=2025, frq=3))

# DO runs

plan(multicore, workers=5)

runs <- mps(om, ctrl=control, args=list(iy=2025),
  hcr=list(target=mean(refpts(om)$MSY) * seq(0.85, 1.15, length=5)))

# SAVE
save(om, run, run3, runs, file="flombf.rda", compress="xz")

# }}}

# --- FLombf(1) {{{

load("ombf.rda")

om <- ombf2

# SET control
control <- mpCtrl(list(
  # EST
  est = mseCtrl(method=perfect.sa),
  # HCR
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0, output="catch",
    trigger=mean(refpts(om)[[1]]$SBMSY) * 1.1, target=mean(refpts(om)[[1]]$MSY))),
  # ISYS
  isys=mseCtrl(method=split.is, args=list(split=c(0.10, 0.70, 0.15, 0.05)))
))

# DO run, run3

run <- mp(om, ctrl=control, args=list(iy=2025, stock=1))

# TODO: stock = 2, stock = c (1,2)

run3 <- mp(om, ctrl=control, args=list(iy=2025, frq=3, stock=1))

# DO runs

plan(multicore, workers=5)

runs <- mps(om, ctrl=control, args=list(iy=2025),
  hcr=list(target=mean(refpts(om)$FMSY) * seq(0.85, 1.15, length=5)))

# SAVE
save(om, run, run3, runs, file="flombf.rda", compress="xz")

# }}}

# --- FLombf(2) {{{

load("ombf.rda")

om <- ombf2

# SET control
control <- mpCtrl(list(
  # EST
  est = mseCtrl(method=perfect.sa),
  # HCR
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
    trigger=mean(refpts(om)[[2]]$SBMSY) * 1.1, target=mean(refpts(om)[[2]]$FMSY))),
  # ISYS
  isys=mseCtrl(method=split.is, args=list(split=c(0.10, 0.70, 0.15, 0.05)))
))

# CHECK: SET fixedC.hcr control
control <- mpCtrl(list(
  # EST
  est = mseCtrl(method=perfect.sa),
  # HCR
  hcr = mseCtrl(method=fixedC.hcr, args=list(ctrg=100)),
  # ISYS
  isys=mseCtrl(method=split.is, args=list(split=c(0.10, 0.70, 0.15, 0.05)))
))

# DO run, run3

run <- mp(om, ctrl=control, args=list(iy=2025, stock=2))

# TODO: stock = 2, stock = c (1,2)

run3 <- mp(om, ctrl=control, args=list(iy=2025, frq=3, stock=2))

# DO runs

plan(multicore, workers=5)

runs <- mps(om, ctrl=control, args=list(iy=2025),
  hcr=list(target=mean(refpts(om)$FMSY) * seq(0.85, 1.15, length=5)))

# SAVE
save(om, run, run3, runs, file="flombf2.rda", compress="xz")

# }}}
