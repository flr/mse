# test-mp_FLom.R - DESC
# /test-mp_FLom.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2



# TEST

load_all()
library(testthat)

# DATA: om, oem

data(ple4om)

# args

mpargs <- list(iy=2017)


# --- RUN 1: perfect.sa + fixedF.hcr: fbar=0.3

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=FLQuant(0.3)))))

r1 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

plot(om, R1=r1)

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=FLQuant(0.5)))))

r1b <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

plot(om, R1=r1, R1b=r1b)


# --- RUN 2: perfect.sa + catchSSB.hcr: dtarget=0.40, dlimit=0.10

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=catchSSB.hcr, args=list(dtarget=0.40, dlimit=0.10,
  lambda=1, MSY=6000))))

r2 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

plot(om, R2=r2)

data(statistics)

performance(r2, statistics=statistics["SBMSY"], metrics=list(SB=ssb))

dep <- function(x)
  ssb(x) %/% ssb(x)[,1]
inddep <- list(~yearMeans(DEP), names="DEP", desc="Depletion")

performance(r2, statistics=list(DEP=inddep), metrics=list(DEP=dep))[, mean(data)]

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=catchSSB.hcr, args=list(dtarget=0.25, dlimit=0.10,
  lambda=1, MSY=100000))))

r2b <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

plot(om, R2=r2, R2b=r2b)


# --- RUN 3: perfect.sa + ices.hcr: sblim=200k, sbsafe=300k, ftrg=0.15

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr,
    args=list(fmin=0.05, ftrg=0.15, sblim=200000, sbsafe=300000))))

r3 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)


ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr,
    args=list(fmin=0.05, ftrg=0.30, sblim=200000, sbsafe=300000))))

r3b <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)


plot(om, R3=r3, R3b=r3b)


# --- RUN 4: perfect.sa + ices.hcr + tac.is: sblim=200k, sbsafe=300k, ftrg=0.15

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr,
    args=list(fmin=0.05, ftrg=0.15, sblim=200000, sbsafe=300000)),
  isys = mseCtrl(method=tac.is, args=list(initac=25000))))

r4 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)


ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr,
    args=list(fmin=0.05, ftrg=0.25, sblim=200000, sbsafe=300000)),
  isys = mseCtrl(method=tac.is, args=list(initac=25000))))

r4b <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)


plot(om, R4=r4, R4b=r4b)


# --- RUN 5: a4a.sa + ices.hcr + tac.is: sblim=200k, sbsafe=300k, ftrg=0.15

library(FLa4a)

ctrl <- mpCtrl(list(
  est = mseCtrl(method=sca.sa),
  hcr = mseCtrl(method=ices.hcr, args=list(fmin=0.05, ftrg=0.15, sblim=200000,
    sbsafe=300000)),
  isys = mseCtrl(method=tac.is)))

r5 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

library(doParallel)
registerDoParallel(2)

r5p <- mp(om, oem=oem, args=mpargs, ctrl=ctrl, parallel=TRUE)

all.equal(r5, r5p)

plot(om, r5)

plot(om, R1=r1, R2=r2, R3=r3, R4=r4, R5=r5)

