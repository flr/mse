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


# --- SIMPLE tests:

mpargs <- list(iy=2017)

# RUN 1: perfect.sa + fixedF.hcr: fbar=0.3

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=FLQuant(0.3)))))

r1 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

fbar(r1)

plot(om, R1=r1)


# RUN 2: perfect.sa + catchSSB.hcr: dtarget=0.40, dlimit=0.10

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=catchSSB.hcr, args=list(dtarget=0.40, dlimit=0.10,
  lambda=1, MSY=100000))))

r2 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

plot(om, R2=r2)


performance(r2, indicators=indicators["SBMSY"], metrics=list(SB=ssb))

performance(om(r2), indicators=indicators["SBMSY"], metrics=list(SB=ssb))


performance(stock(om(r2)), indicators=indicators["SBMSY"],
  metrics=list(SB=ssb), refpts=refpts(om(r2)))


# RUN 3: perfect.sa + ices.hcr: blim=200k, bsafe=300k, ftrg=0.15

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr,
    args=list(fmin=0.05, ftrg=0.15, blim=200000, bsafe=300000))))

r3 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

plot(om, R3=r3)


# RUN 4: perfect.sa + ices.hcr + tac.is: blim=200k, bsafe=300k, ftrg=0.15

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr,
    args=list(fmin=0.05, ftrg=0.15, blim=200000, bsafe=300000)),
  isys = mseCtrl(method=tac.is, args=list(initac=25000))))

r4 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

plot(om, R4=r4)


# RUN 5: a4a.sa + ices.hcr + tac.is: blim=200k, bsafe=300k, ftrg=0.15

library(FLa4a)

ctrl <- mpCtrl(list(
  est = mseCtrl(method=sca.sa),
  hcr = mseCtrl(method=ices.hcr, args=list(fmin=0.05, ftrg=0.15, blim=200000,
    bsafe=300000)),
  isys = mseCtrl(method=tac.is)))

r5 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

library(doParallel)
registerDoParallel(3)

r5p <- mp(om, oem=oem, args=mpargs, ctrl=ctrl, parallel=TRUE)

all.equal(r5, r5p)

plot(om, r5)

plot(om, R1=r1, R2=r2, R3=r3, R4=r4, R5=r5)

