# helper_mse.R - DESC
# /home/mosqu003/Active/mse_FLR/mse/tests/testthat/helper_mse.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# -- SETUP: Load test data and define test statistics {{{

# Load example operating model
load(system.file("data", "plesim.rda", package="mse"))

# Add missing reference points
refpts(om)$MSY <- refpts(om)$SBMSY * 0.30
refpts(om)$Blim <- refpts(om)$SBMSY * 0.20
refpts(om)$SBlim <- refpts(om)$SBMSY * 0.20
refpts(om)$Ftarget <- refpts(om)$FMSY

# Load statistics
data(statistics)

# Create test objects

flom <- fwd(window(om, end=2035), 
  control=fwdControl(year=2026:2035, quant='fbar', value=0.11))

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.20))))

flmse <- mp(flom, ctrl=control, args=list(iy=2025))

flmses <- mps(flom, ctrl=control, args=list(iy=2022),
  hcr=list(ftrg=c(0.15, 0.18, 0.21)))

performance(flmses) <- performance(flmses, statistics=statistics, type="test")

save(flom, flmse, flmses, file="setup.rda", compress="xz")

# }}}
