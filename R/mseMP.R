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
# 

#' 
data(cod)
omp <- cod
sr <- codsr
index <- stock(cod)
years <- seq(20, 30, by=2)
hcr <- ~pmax(Fmin, pmin(Ftarget, ssb * (Ftarget / (Btrigger - Blim))))
hcrparams <- FLPar(Btrigger=600, Blim=200, Ftarget=refpts$Fmsy, Fmin=0.05)
refpts <- FLPar(Fmsy=0.15)


mseMP <- function(
  # OM: FLStock + SR + RPs + index
  omp, sr, refpts, index,
  # years
  years, verbose=FALSE,
  # hcr
  hcr=~pmax(Fmin, pmin(Ftarget, ssb * (Ftarget / (Btrigger - Blim)))),
  # hcrparams
  hcrparams=FLPar(Fmin=0, Ftarget=refpts$Fmsy),
  # lags
  dlag=1, mlag=1, 
  # oem, imp
  oemparams, imparams) {

  # SETUP

  # MESSAGES
  if(verbose)
    pb <- utils::txtProgressBar(min = years[1], max = years[length(years)],
      initial = 1, style=3, title="Years:")

  # LOOP
  for (y in years) {

    # OBSERVATION + error
    # TODO USE oem()
    fages <-range(cod, c('minfbar', 'maxfbar'))
    cpue <- window(quantSums((stock.n(omp) * stock.wt(omp))[fages[1]:fages[2]]), end=y-dlag)

    # INDICATOR
    evalPredictModel(omp[,ac(y-1)], predictModel(model=hcr, params=rbind(refpts, hcrparams)))

    # HCR

    ftarget <- hcr(ple4[,'2000'], params=FLPar(btrigger=215000, fmsy=0.35))[[1]]

    # DECISION + error

    finalf <- ftarget

    # FWD

    res <- fwd(ple4, sr=p4sr, f=expand(ftarget, year=2001:2008))

    # PRINT
    if(verbose)
      setTxtProgressBar(pb, y)

  return(TRUE)
  }
}

