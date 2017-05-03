# mseBD.R - DESC
# /mseBD.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(mse)

library(ioalbmse)

data(oms)

discards.wt(omp) <- landings.wt(omp)
catch.wt(omp) <- landings.wt(omp)

# --- DEBUG
sel.pattern <- cpue$sel.pattern
cpue <- propagate(window(cpue$index, end=2024), 200)
sr <- osr
refpts <- rpts

years <- seq(2014, 2024, by=2)
y <- years[1]
freq <- years[2] - years[1]

dlag <- 1
mlag <- 1

# hcr <- ~pmax(Dlimit, pmin(Dtarget, ((lambda * MSY) / (Dtarget - Dlimit)) * (dep - Dlimit), lambda * MSY))

hcrparams <- FLPar(Dlimit=0.10, Dtarget=0.40, lambda=1.0, dltac=0.15, dhtac=0.15)

hcr <- ~ifelse(dep <= Dlimit, 0,
  ifelse(dep < Dtarget, (lambda * MSY) / (Dtarget - Dlimit) * (dep - Dlimit), lambda * MSY))

# --- TEST
its <- 150:200

omps <- FLCore::iter(omp, its)

osrs <- osr
osrs@params <- osrs@params[, 1]

R0 <- msePT(omps, sr=osrs, cpue=propagate(window(cpue$index, end=2024), length(its)),
  years=seq(2014, 2024, by=2), verbose=TRUE)

plot(om, R0$om)

R0 <- msePT(omp, sr=osr, cpue=propagate(window(cpue$index, end=2024), 200),
  years=seq(2014, 2024, by=2), verbose=TRUE,
  hcrparams=FLPar(Dlimit=0.15, Dtarget=0.45, lambda=1.0, dltac=0.20, dhtac=0.15))
