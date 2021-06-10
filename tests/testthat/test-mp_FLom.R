# test-mp_FLom.R - DESC
# /test-mp_FLom.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(ple4om)

# args

mpargs <- list(iy=1994, fy=2021)

# perfect.sa + fixedF.hcr: fbar=0.3

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=FLQuant(0.3)))))

r0 <- mp(window(om, end=mpargs$fy), oem=oem, args=mpargs, ctrl=ctrl)

plot(om(r0))

# perfect.sa + catchSSB.hcr: dtarget=0.40, dlimit=0.10

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=catchSSB.hcr, args=list(dtarget=0.40, dlimit=0.10,
  lambda=1, MSY=100000))))

r1 <- mp(window(om, end=mpargs$fy), oem=oem, args=mpargs, ctrl=ctrl)

plot(om(r1))

# perfect.sa + ices.hcr: blim=200k, bsafe=300k, ftrg=0.15

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr, args=list(fmin=0.05, ftrg=0.15, blim=200000,
    bsafe=300000))))

r2 <- mp(window(om, end=mpargs$fy), oem=oem, args=mpargs, ctrl=ctrl)

plot(om(r2))

# perfect.sa + ices.hcr + tac.is: blim=200k, bsafe=300k, ftrg=0.15

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr, args=list(fmin=0.05, ftrg=0.15, blim=200000,
    bsafe=300000)),
  isys = mseCtrl(method=tac.is)))

r3 <- mp(window(om, end=mpargs$fy), oem=oem, args=mpargs, ctrl=ctrl)

plot(om(r3))

# a4a.sa + ices.hcr + tac.is: blim=200k, bsafe=300k, ftrg=0.15

library(FLa4a)

mpargs <- list(iy=1994, fy=1996)

ctrl <- mpCtrl(list(
  est = mseCtrl(method=sca.sa),
  hcr = mseCtrl(method=ices.hcr, args=list(fmin=0.05, ftrg=0.15, blim=200000,
    bsafe=300000)),
  isys = mseCtrl(method=tac.is)))

r4 <- mp(window(om, end=mpargs$fy), oem=oem, args=mpargs, ctrl=ctrl)

plot(om(r4))


sca.sa <- function(stk, idx, args, update=TRUE, dfm=c(0.75, 0.75), ...){

	args0 <- list(...)
	
  if(update) args0$fmodel <- defaultFmod(stk, dfm=dfm)
	
  stk <- replaceZeros(stk)
	idx <- replaceZeros(idx)
	
  args0$stock <- stk
	args0$indices <- idx
	if(is.null(args0$fit)) args0$fit <- 'MP'
	tracking <- args0$tracking
	args0$tracking <- NULL
	fit <- do.call('sca', args0)
	stk <- stk + fit
	track(tracking, "conv.est", ac(range(stk)["maxyear"] + 1)) <- fit@fitSumm["maxgrad",]
	list(stk = stk, tracking = tracking)
}





# PLOTS

library(patchwork)

(plot(om(r0)) + ggtitle("fixedF.hcr(fbar=0.3)")) +
(plot(om(r1)) + ggtitle("catchSSB(dtarget=0.40, dlim=0.10)")) +
(plot(om(r2)) + ggtitle("ices.hcr(blim=200k, bsafe=300k, ftrg=0.15)")) +
(plot(om(r3)) + ggtitle("ices.hcr(...) + tac.is"))
 
# 
res <- data.frame(ssb=c(ssb(stk)[, ac(ayrs)]), f=fs, ay=ayrs, dy=ayrs-1, my=ayrs+1)

iterSums(ssb(om(r0)) <
fbar(om(r0))

ggplot(res, aes(x=ssb, y=f)) +
  # geom_text(aes(label=ay))
  geom_point() +
  xlim(c(0, NA)) + ylim(c(0, NA)) +
  geom_segment(x=0, xend=blim, y=fmin, yend=fmin) +
  geom_segment(x=blim, xend=bsafe, y=fmin, yend=ftrg) +
  geom_segment(x=bsafe, xend=max(res$ssb), y=ftrg, yend=ftrg) +
  ylab(expression(bar(F))) + xlab("SSB (t)")


# TEST parallel

library(doParallel)
registerDoParallel(1)

ctrl <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=ices.hcr, args=list(fmin=0.05, ftrg=0.15, blim=200000,
    bsafe=300000)),
  isys = mseCtrl(method=tac.is)))

rp3 <- mp(window(om, end=mpargs$fy), oem=oem, args=mpargs, ctrl=ctrl)



an <- mp(om, oem, args=mpargs, ctrl=ctrl.sc)
