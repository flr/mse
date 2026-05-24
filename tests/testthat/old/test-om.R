# test-om.R - DESC
# /home/mosqu003/Active/MSE_PKG@flr/mse/tests/testthat/test-om.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# --- CREATE demo objects

fq <- FLQuant(NA, dimnames=list(age=1:9, year=10:30))

# FLBiol: wt, m, spwn, mat

bio <- FLBiol(
  wt=fq %=% seq(0.03, 8, length=9),
  m=fq %=% c(0.4, 0.35, rep(0.3, 6), 0.35),
  spwn=fq[1,] %=% 0.25,
  mat=fq %=% c(0, 0.2, 0.5, rep(1,6)))

# FLFisheries

fia <- FLFishery(
  effort=FLQuant(0.001, dimnames=list(year=10:30)),
  A=FLCatch(landings.wt=wt(bio), discards.wt=wt(bio),
    landings.n=fq %=% 1, discards.n=fq %=% 0,
    catch.sel=fq %=% c(0.1, 0.3, 0.4, 0.8, 1, 1, 1, 0.9, 0.8)))

fib <- FLFishery(
  effort=FLQuant(0.001, dimnames=list(year=10:30)),
  A=FLCatch(landings.wt=wt(bio) * 0.90, discards.wt=wt(bio) * 0.90,
    landings.n=fq %=% 1, discards.n=fq %=% 0,
    catch.sel=fq %=% c(0.1, 0.4, 0.8, rep(1, 6))))

fis <- FLFisheries(A=fia, B=fib)

# 

bio <- initiate(bio, B0=23000, h=0.8)

bio <- deplete(bio, sel=catch.sel(fia[[1]]), dep=0.4)

# ONE fishery, catch history

history <- FLQuants(catch=FLQuant(c(150, seq(200, 1200, length=9),
  seq(1500, 800, length=10)), dimnames=list(year=11:30)))

sim <- simulator(bio, fis['A'], B0=23000, h=0.80, dep=0.40,
  history=history)

plot(sim$biol)

# TWO fisheries

history <- list(catch=FLQuants(A=FLQuant(c(150, seq(200, 1200, length=9),
  seq(1050, 400, length=10)), dimnames=list(year=11:30)),
  B=FLQuant(c(15, seq(20, 120, length=9),
  seq(200, 800, length=10)), dimnames=list(year=11:30))))

sim <- simulator(bio, fis, B0=23000, h=0.80, dep=0.40,
  history=history)

plot(sim$biol)

plot(catch(sim$fisheries))

# --- iters

bio <- FLBiol(
  wt=rlnorm(100, log(fq %=% seq(0.03, 8, length=9)), 0.1),
  m=fq %=% c(0.4, 0.35, rep(0.3, 6), 0.35),
  spwn=fq[1,] %=% 0.25,
  mat=fq %=% c(0, 0.2, 0.5, rep(1,6)))

fis <- lapply(fis, propagate, 100)


# ONE fishery, catch history

history <- FLQuants(catch=FLQuant(c(150, seq(200, 1200, length=9),
  seq(1500, 800, length=10)), dimnames=list(year=11:30)))

history <- fwdControl(value=seq(0.9, 0.35, length=20), quant="ssb_end",
  year=11:30, relYear=10, biol=1, relBiol=1)

sim <- simulator(bio, fis['A'], B0=23000, h=0.80, dep=0.40,
  history=history)

plot(sim$biol)

mets <- FLQuants(
  catch=catch(sim$fisheries)$A,
  ssb=ssb(sim$biol, hr=catch.n(sim$fisheries[[1]][[1]]) / n(sim$biol)))


# BUG: harvest(FLBiol, FLFisheries) 
SSB <- FLQuants(
  F=ssb(sim$biol, f=harvest(sim$biol, sim$fisheries)),
  HR=ssb(sim$biol, hr=catch.n(sim$fisheries[[1]][[1]]) / n(sim$biol)),
  C=ssb(sim$biol, catch.n=catch.n(sim$fisheries[[1]][[1]])))


# TWO fisheries

history <- list(catch=FLQuants(A=FLQuant(c(150, seq(200, 1200, length=9),
  seq(1050, 400, length=10)), dimnames=list(year=11:30)),
  B=FLQuant(c(15, seq(20, 120, length=9),
  seq(200, 800, length=10)), dimnames=list(year=11:30))))

sim <- simulator(bio, fis, B0=23000, h=0.80, dep=0.40,
  history=history)

plot(sim$biol)

plot(catch(sim$fisheries))


