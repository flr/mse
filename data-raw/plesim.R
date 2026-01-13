# om.R - DESC
# mse/data-raw/om.R

# Copyright (c) WMR, 2025.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(mse)
library(FLRef)

iy <- 1960
fy <- 2025
ys <- seq(iy, fy)
its <- 100

# LOAD ple4 as reference
data(ple4)

stock.n(ple4) <- stock.n(ple4) / 100

# FIT SRR w/fixed steepness
sr <- srrTMB(as.FLSR(ple4, model=bevholtSV), spr0=mean(spr0y(ple4)), s=0.52)

# GET MSY refpts
brp <- computeFbrp(ple4, sr, proxy="msy") 

# SET dimensions in brp
fbar(brp) <- FLQuant(rep(0.01, length(ys)))

# CONVERT to FLStock
stk <- as(brp, "FLStock")

# FIX year dimnames & units
dimnames(stk) <- list(year=ys)
units(stk) <- standardUnits(stk)

# PROPAGATE to its
stk <- FLStockR(propagate(stk, its))

# ADD refpts
refpts(stk) <- Fbrp(brp)
b0 <- an(Fbrp(brp)["B0"])

# CREATE rffwd control:
control <- FLPar(Feq=0.21, Frate=0.08, Fsigma=0.10, SB0=c(Fbrp(brp)["B0"]),
  minyear=iy + 1, maxyear=fy, its=its)

# ADD rec devs
residuals(sr) <- rlnormar1(n=its, meanlog=0, sdlog=0.3, rho=0.1, years=ys)

# PROJCET forward stk history
run <- rffwd(stk, sr=sr, control=control, deviances=residuals(sr))

# PLOT
plotAdvice(run)

# -- OM

# CREATE om
om <- FLom(stock=run, refpts=refpts(run), sr=sr, name="PLE")

# EXTEND to 2055
om <- fwdWindow(om, end=2055,
  deviances=rlnormar1(n=its, meanlog=0, sdlog=0.3, rho=0.1, years=seq(2020, 2055)))

# GET refpts
brps <- brp(FLBRP(as(run, 'FLStock'), sr=list(model=sr@model, params=sr@params)))

# ADD renamed refpts
refpts(om) <- remap(refpts(brps))

# -- OEM

# GET indices
data(ple4.indices)

# BUILD pseudo-BTS
idx <- FLIndex(name="SUR", desc="An IBTS-like survey",
  sel.pattern=expand(sel.pattern(ple4.indices[[6]])[,1],
    year=seq(1980, 2025), fill=TRUE),
  index.q=quantMeans(expand(index.q(ple4.indices[[2]])[1:8,1],
    year=seq(1980, 2025), fill=TRUE)) *
      rlnorm(100, stock(run)[, ac(seq(1980,2025))] %=% 0, 0.2),
  catch.wt=stock.wt(run)[1:8, ac(seq(1980,2025))],
  catch.n=catch.n(run)[1:8, ac(seq(1980,2025))],
  range=c(startf=0.5, endf=0.5)
)

# CONSTRUCT at-age survey
idx <- survey(run, idx, stability=0.86)

# CONSTRUCT biomass index
idb <- as(idx, 'FLIndexBiomass')

# BUILD oem
oem <- FLoem(observations=list(stk=run, idx=FLIndices(SUR=idx, CPUE=idb)),
  method=sampling.oem)

oem <- fwdWindow(oem, end=2055)

# SAVE
save(om, oem, file="../data/plesim.rda", compress="xz")


# -- TESTS

# 

rule <- mpCtrl(list(

  # (est)imation method: shortcut.sa + SSB deviances
  est = mseCtrl(method=perfect.sa),

  # hcr: hockeystick (fbar ~ ssb | lim, trigger, target, min)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0, trigger=refpts(om)$BMSY,
      target=refpts(om)$FMSY, min=0,
    metric="ssb", output="fbar"))
))

run <- mp(om, ctrl=rule, args=list(iy=2025))

plot(om, run)

#

rule <- mpCtrl(list(

  # (est)imation method: shortcut.sa + SSB deviances
  est = mseCtrl(method=cpue.ind),

  # hcr: hockeystick (fbar ~ ssb | lim, trigger, target, min)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0, trigger=refpts(om)$BMSY,
      target=refpts(om)$FMSY, min=0,
    metric="ssb", output="fbar"))
))

# 

futmsy <- fwd(om, fbar=fbar(om)[, ac(2025:2055)] %=% refpts(om)$FMSY)

ssb(futmsy)[, '2055'] / refpts(om)$SBMSY

fut3msy <- fwd(om, fbar=fbar(om)[, ac(2025:2055)] %=% refpts(om)$FMSY * 3)

futf0 <- fwd(om, fbar=fbar(om)[, ac(2025:2055)] %=% 0)

ssb(futf0)[, '2055'] / refpts(om)$SB0

plot(stock(futmsy), stock(futf0))

plot(stock(fut3msy), stock(futf0))

