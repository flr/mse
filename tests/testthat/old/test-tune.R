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

data(sol274)
data(statistics)


# --- SIMPLE tests:

mpargs <- list(iy=2021)


# RUN 2: perfect.sa + hockeystick.hcr(lim=2e4, trigger=4e4, target=0.21)

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=3.1e4, trigger=4.3e4,
    target=0.21, min=0.05, metric="ssb", output="fbar"))
))

r2 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

plot(om, r2)

# DEBUG
plot(FLQuants(Map(append, metrics(om), metrics(r2)))) +
  ylim(c(0, NA)) +
  geom_flpar(data=FLPars(SB=refpts(om)$SBMSY, F=refpts(om)$FMSY), x=1990)

performance(r2, statistics=statistics["PSBMSY"], metrics=list(SB=ssb))[, mean(data)]

t2 <- tunebisect(om, oem=oem, args=mpargs, control=ctrl, metrics=list(SB=ssb),
  tune=list(dtarget=c(0.10, 0.90)), pyears=list(2020:2030),
  indicator=statistics['PSBMSY'])

plot(om, R2=r2, T2=t2)


# RUN 3: perfect.sa + ices.hcr: blim=200k, bsafe=300k, ftrg=0.15

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr,
    args=list(fmin=0.05, ftrg=0.15, blim=200000, bsafe=300000))))

r3 <- mp(om, oem=oem, args=mpargs, ctrl=ctrl)

performance(r3, statistics=statistics["PSBMSY"], metrics=list(SB=ssb))[, mean(data)]

t3 <- tunebisect(om, oem=oem, args=mpargs, control=ctrl, metrics=list(SB=ssb),
  tune=list(ftrg=c(0.05, 0.40)), pyears=list(2020:2030),
  indicator=statistics['PSBMSY'])

performance(t3, statistics=statistics["PSBMSY"], years=list(2020:2030),
  metrics=list(SB=ssb))[, mean(data)]

control(t3)$hcr@args$ftrg

plot(om, R2=r2, T2=t2)


# --- grid

library(doParallel)
registerDoParallel(2)

rp <- mp(om, oem=oem, args=mpargs, ctrl=ctrl, parallel=TRUE)

ctrlb <- ctrl
ctrlb$hcr@args$ftrg <- 0.30
ctrls <- list(a=ctrl, b=ctrlb)

foo <- function(om, oem, args, ctrls) {

  res <- lapply(ctrls, function(x)
    mp(om, oem=oem, args=mpargs, ctrl=x, parallel=TRUE)
    )

plot(om, res[[1]], res[[1]])








ops <- list(a=ctrl, b=ctrlb)

res <-
  foreach(op = ops) %:% 
    foreach(i = 1) %do% {
    mp(om, oem=oem, args=mpargs, ctrl=op, parallel=TRUE)
    }

plot(om(res[[1]][[1]]),
res[[2]][[1]])

all.equal(res[[1]][[1]], res[[2]][[1]])

res[[1]][[1]]@control$hcr@args
res[[2]][[1]]@control$hcr@args
