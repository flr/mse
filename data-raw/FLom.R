# FLombf.R - DESC
# mse/data-raw/FLombf.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(ss3om)

# --- LOAD SS run

out <- readOutputss3("albio", compress="bz2")

# --- ASSEMBLE OM: no seasons, 2 sexes

# FLStock
stk <- simplify(buildFLSss330(out), 'season')

# FLSR
srr <- buildFLSRss3(out)
params(srr) <- FLPar(as(params(srr)[,1,1], 'list'))

residuals(srr) <- expand(append(propagate(residuals(srr), 200),
  ar1rlnorm(0.6, 2016:2046, 200, 0, 0.4)), unit=c("F", "M"))

# FLIndexBiomass
idx <- buildFLIBss330(out, fleets=1:4)

# refpts
rps <- buildFLRPss330(out)

# results
res <- buildRESss330(out)

# FLom
om <- FLom(stock=stk, refpts=rps, sr=srr, projection=mseCtrl(method=fwd.om))


# --- EXTEND to future & propagate

# fwdWindow: selectivity, discards ratio, biology (m, mat, wts)

om <- fwdWindow(propagate(om, 200), end=2046)


# --- OEM

llcpue <- propagate(idx[[1]][,,,1], 200)
dimnames(llcpue) <- list(season="all")
range(llcpue, c("startf", "endf")) <- c(0, 0.25)
sel.pattern(llcpue) <- unitMeans(sel.pattern(llcpue))
llcpue <- fwdWindow(llcpue, end=2046)

# OBS ERROR, AR1, effort creep

oem <- FLoem(method=sampling.oem,
  observations=list(stk=nounit(stock(om)), idx=FLIndices(LLCPUE=llcpue)),
  deviances=list(
    stk=FLQuants(catch.n=rlnorm(200, unitSums(catch.n(stock(om))) %=% 0, 0.20)),
    idx=FLQuants(LLCPUE=rlnorm(200, log(index.q(llcpue)), 0.2))))

mseargs <- list(iy=2017)

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=2, k2=2, gamma=0.85, nyears=5, metric=function(x) unitSums(ssb(x))))
  ))

# RUN mp

trend <- mp(om, oem=oem, ctrl=control, args=mseargs)

# TODO CHECK iy is correct
# TODO dims stk, idx
# TODO check deviances years
# TODO check iters om vs. ctrl
# TODO stk(om) and units: merge



# fwd

f0 <- fwd(om, control=fwdControl(year=2018:2046, quant="fbar", value=0))

plot(f0)
