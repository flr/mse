# model.R - Run analysis, write model results
# 2022_sol.27.4_assessment/model.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# - BUILDS simple OM from so.27.4 AAP run + McMC
# - FITS bevholt SRR
# - SETS future deviances on stk and idx.


library(AAP)
library(mse)
library(FLSRTMB)

# LOAD data

load('sol274/data.Rdata')

# RUN McMC fit

control <- AAP.control(pGrp=TRUE, qplat.surveys=8, qplat.Fmatrix=9,
  Sage.knots=6, Fage.knots=8, Ftime.knots=28, mcmc=TRUE)

system.time(
  mcfit <- aap(stock, indices, control=control, verbose=TRUE)
)

save(mcfit, file="sol274/mcfit.Rdata", compress="xz")

# SETUP for om, oem

load('sol274/mcfit.Rdata')

fy <- 2042
it <- 100
idx <- sample(seq(1000), it)

# SUBSAMPLE iters

runmc <- iter(stock + mcfit, idx)

dimnames(runmc) <- list(iter=seq(it))

# FIT SRR

srr <- as.FLSR(runmc, model="bevholtSV")

res <- parallel::mclapply(seq(dims(srr)$iter), function(i)
  srrTMB(iter(srr, i), spr0=spr0y(stock)), mc.cores=3)

# TODO: SIMPLIFY
pars <- Reduce(combine, lapply(res, params))
resid <- Reduce(combine, lapply(res, residuals))
fits <- Reduce(combine, lapply(res, fitted))

params(srr) <- pars
residuals(srr) <- resid
fitted(srr) <- fits
srr@model <- bevholt()$model

srr <- window(srr, end=fy)

residuals(srr)[, ac(2022:fy)] <- exp(residuals(srr)[, sample(ac(2000:2021),
  21)])

# OM

om <- FLom(stock=fwdWindow(runmc, end=fy), refpts=refpts, sr=srr,
  projection=mseCtrl(method=fwd.om))

# OEM

# observations: stk, idx, lens

index.q(indices$BTS)[] <- q.hat(mcfit)$BTS
index.q(indices$SNS)[] <- q.hat(mcfit)$SNS[1:6,]

obs <- list(stk=propagate(fwdWindow(runmc, end=fy), it),
    idx=lapply(fwdWindow(indices, end=fy), propagate, it))

# length samples
vbpars <- FLPar(linf=38.4, k=0.306, t0=-1.70)
ialk <- invALK(vbpars, age=1:10, cv=0.15, lmax=1.25)
lsam <- lenSamples(window(catch.n(obs$stk), end=2021), ialk)

obs$len <- window(lsam, end=2042)

# deviances

devs <- list(stk=list(
    catch.n=rlnorm(it, window(catch.n(runmc), end=fy) %=% 0, 0.15)),
  idx=lapply(obs$idx, function(x) rlnorm(it,
    window(index.q(x), end=fy) %=% 0, 0.20))
  #, lens=lsam %=% 0
)

oem <- FLoem(observations=obs, deviances=devs, method=sampling.oem)

# SAVE

save(om, oem, file="../data/sol274.Rdata", compress="xz")

# TEST

# BUG: mpCtrl(est=, hcr=)
# BUG: args from refpts, wrong dims

control <- mpCtrl(list(
  # perfect.sa
  est=mseCtrl(method=perfect.sa),
  # hockeystick.hcr(refpts)
  hcr=mseCtrl(method=hockeystick.hcr,
    args=list(lim=c(refpts$Blim), trigger=c(refpts$Btrigger),
      target=c(refpts$Fmsy))),
  # TAC fwd
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15))
))

plot_hockeystick.hcr(control$hcr)

tes <- mp(om, oem=oem, ctrl=control, args=list(iy=2021))

plot(om, `ICES MP`=tes)

# CHECK HCR decision, hcr ~ F.om
tracking(tes)[c('hcr', 'F.om'),]

# CHECK stf TAC, isys ~ C.om
tracking(tes)[c('isys', 'C.om'),]

# CHECK SA SB estimate
tracking(tes)[c('SB.om', 'SB.est'),]

# CHECK SA F estimate
tracking(tes)[c('F.om', 'F.est'),]
