# mseMP.R - DESC
# /mseMP.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# omp: biol + fisheries
# years
# hcrparams
# oemparams
# imparams

# PKGS
# library(mse)
# library(FLash)
# library(FLBRP)
# 
# # DATA
# data(cod)
# # codsr <- fmle(as.FLSR(cod, model="ricker"))
# codrp <- brp(FLBRP(cod, sr=codsr))
# 
# # MP params
# omp <- fwdWindow(cod, codrp, end=40)
# years <- seq(31, 40, by=2)
# hcrparams <- FLPar()
# hcrparams <- list()
# 


mseIRate <- function(omp, sr, index, years, dlag=1, mlag=1, freq=1,
  hcrparams=FLPar(responsiveness=0.5, hr_multiplier=1, biomass_threshold=0.5,
    biomass_limit=0.2, maxTAC=500000),
  ref_years=seq(years[1] - 4, years[1] - 1), 
  oem=list(cpue=~0, effort=~0), imp=list(tac=~0), verbose=FALSE) {

  # SETUP

  # LOOP
  for (y in years) {

    # OBSERVATION + error
    # TODO USE oem()
    fages <-range(cod, c('minfbar', 'maxfbar'))
    cpue <- window(quantSums((stock.n(omp) * stock.wt(omp))[fages[1]:fages[2]]), end=y-1)

    # INDICATOR


    # HCR

    ftarget <- hcr(ple4[,'2000'], params=FLPar(btrigger=215000, fmsy=0.35))[[1]]


    # DECISION + error

    finalf <- ftarget


    # FWD

    res <- fwd(ple4, sr=p4sr, f=expand(ftarget, year=2001:2008))


  return(TRUE)
  }
}
