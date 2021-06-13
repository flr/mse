# test-tune.R - DESC
# /test-tune.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# TEST

load_all()
library(testthat)

# DATA: om, oem

data(ple4om)
data(indicators)


# --- SIMPLE tests:

mpargs <- list(iy=2017)


# RUN 2: perfect.sa + catchSSB.hcr: dtarget=0.40, dlimit=0.10

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=catchSSB.hcr, args=list(dtarget=0.40, dlimit=0.10,
  lambda=1, MSY=100000))))

r2 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

plot(om, r2)

# DEBUG
plot(FLQuants(Map(append, metrics(om), metrics(r2)))) +
  ylim(c(0, NA)) +
  geom_flpar(data=FLPars(SB=refpts(om)$SBMSY, F=refpts(om)$FMSY), x=1990)

performance(r2, indicators=indicators["PSBMSY"], metrics=list(SB=ssb))[, mean(data)]

t2 <- tunebisect(om, oem=oem, args=mpargs, control=ctrl, metrics=list(SB=ssb),
  tune=list(dtarget=c(0.10, 0.90)), pyears=list(2020:2030),
  indicator=indicators['PSBMSY'])

plot(om, R2=r2, T2=t2)


# RUN 3: perfect.sa + ices.hcr: blim=200k, bsafe=300k, ftrg=0.15

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr,
    args=list(fmin=0.05, ftrg=0.15, blim=200000, bsafe=300000))))

r3 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

performance(r3, indicators=indicators["PSBMSY"], metrics=list(SB=ssb))[, mean(data)]

t3 <- tunebisect(om, oem=oem, args=mpargs, control=ctrl, metrics=list(SB=ssb),
  tune=list(ftrg=c(0.05, 0.40)), pyears=list(2020:2030),
  indicator=indicators['PSBMSY'])

performance(t3, indicators=indicators["PSBMSY"], years=list(2020:2030),
  metrics=list(SB=ssb))[, mean(data)]


control(t3)$hcr@args$ftrg

plot(om, R2=r2, T2=t2)


