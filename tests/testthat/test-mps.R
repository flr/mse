# test-mps.R - DESC
# mse/tests/testthat/test-mps.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# dataset contains both OM (FLom) and OEM (FLoem)
data(plesim)

# Set control: sa and hcr
control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=10000, target=0.18))))

# RUN mps w/one hcr argument

plan(sequential)

tes11 <- mps(om=om, ctrl=control, args=list(iy=2025, fy=2030),
  hcr=list(trigger=seq(9e3, 12e3, length=3)))

plan(multicore, workers=3)

tes12 <- mps(om=om, ctrl=control, args=list(iy=2025, fy=2030),
  hcr=list(trigger=seq(9e3, 16e3, length=3)))

# CHECK: run on 3 cores
lapply(tes12, function(x) tracking(x)[metric == 'pid', unique(data)])


tes13 <- mps(om=om, ctrl=control, args=list(iy=2025, fy=2030),
  hcr=list(trigger=seq(12e3, 16e3, length=3)), statistics=statistics[1:4])

# CHECK: reports years in progress bar

tes14 <- mps(om=om, ctrl=control, args=list(iy=2025, fy=2030),
  hcr=list(trigger=seq(12e3, 16e3, length=4)))

performance(tes14)


# RUN mps w/two hcr arguments

tes21 <- mps(om=om, ctrl=control, args=list(iy=2025, fy=2030),
  hcr=list(trigger=seq(12e3, 16e3, length=3),
    lim=seq(4000, 6000, length=3)))

tes22 <- mps(om=om, ctrl=control, args=list(iy=2025, fy=2030),
  hcr=combinations(list(trigger=seq(12e3, 16e3, length=3),
    lim=seq(4000, 6000, length=3))))

