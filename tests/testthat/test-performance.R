# test-performance.R - DESC
# /home/mosqu003/Projects/FLR/code/mse/mse/tests/testthat/test-performance.R

# Copyright (c) WMR, 2025.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(plesim)

# ADD missing refpts
refpts(om)$MSY <- refpts(om)$SBMSY * 0.30
refpts(om)$Blim <- refpts(om)$SBMSY * 0.20
refpts(om)$SBlim <- refpts(om)$SBMSY * 0.20
refpts(om)$Ftarget <- refpts(om)$FMSY

# LOAD statistics
data(statistics)

# OBJECTS

flom <- fwd(window(om, end=2035),  control=fwdControl(year=2026:2035,
  quant='fbar', value=0.11))

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.20))))

flmse <- mp(flom, ctrl=control, args=list(iy=2025))

flmses <- mps(flom, ctrl=control, args=list(iy=2022),
  hcr=list(ftrg=c(0.15, 0.18, 0.21)))

# flombf(1)
# flombf(2)

# - FLQuants

x <- metrics(flom)

per100  <- performance(x, statistics=statistics, refpts=refpts(om),
  om="ple", run="r00", type="test")

# - FLom

x <- flom

per201  <- performance(x, statistics=statistics, refpts=refpts(om),
  run="r00", type="test")

per202  <- performance(x, statistics=statistics, run="r00", type="test")

per203  <- performance(x, run="r00", type="test")

# - FLmse

x <- flmse

per301  <- performance(x, statistics=statistics, run="r00", type="test")

per302  <- performance(x, run="r00", type="test")

# - FLmses

x <- flmses

per401  <- performance(x, statistics=statistics, type="test")

per402  <- performance(x, type="test")

# RETURN data.table

x <- mps(flom, ctrl=control, args=list(iy=2021, fy=2034),
  hcr=list(ftrg=c(0.15, 0.18, 0.21)), statistics=statistics)

print(x)

# - list

# list(FLo)

x <- list(A=flom, B=flom)

per511  <- performance(x, statistics=statistics, type="test")

# list(FLmse, FLo)

x <- c(flmses, A=flom)

per521  <- performance(x, statistics=statistics, type="test")

# list(FLmses)

x <- list(A=flmses, B=flmses)

per531  <- performance(x, statistics=statistics, type="test")

# list(FLQuants)

x <- list(A=metrics(flom), B=metrics(om))

per541  <- performance(x, statistics=statistics, refpts=refpts(flom),
  om="ple", run="r00", type="test")

# - FLStock

x <- stock(flom)

per501  <- performance(x, statistics=statistics[c("C", "F")],
  metrics=list(C=catch, F=fbar), om="om00", type="test")

per502  <- performance(x, statistics=statistics[c("C", "F")],
  om="om00", type="test")

per503  <- performance(x, statistics=statistics[c("C", "F", "FMSY")],
  refpts=refpts(om), om="om00", type="test")

# - FLStocks

x <- FLStocks(A=stock(flom), B=stock(flom))

per601  <- performance(x, statistics=statistics[c("C", "F")],
  metrics=list(C=catch, F=fbar), om="om00", type="test")

per602  <- performance(x, statistics=statistics[c("C", "F")],
  om="om00", type="test")

per603  <- performance(x, statistics=statistics[c("C", "F", "FMSY")],
  refpts=refpts(om), om="om00", type="test")
