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
hcr <- ~pmin(FMSY, ssb * (FMSY / (Btrigger - Blim))),
hcrparams <- FLPar(Btrigger=600, Blim=200)
refpts <- FLPar(FMSY=0.15)


mseMP <- function(omp, sr, index, years, refpts,
  # hcr:
  hcr=~pmax(Fmin, pmin(Ftarget, ssb * (Ftarget / (Btrigger - Blim)))),
  # hcrparams:
  hcrparams=FLPar(Fmin=0, Ftarget=refpts$Fmsy),
  # lags
  dlag=1, mlag=1, 
  # oem, imp
  oemparams, imparams) {

  # SETUP

  # LOOP
  for (y in years) {

    # OBSERVATION + error
    # TODO USE oem()
    fages <-range(cod, c('minfbar', 'maxfbar'))
    cpue <- window(quantSums((stock.n(omp) * stock.wt(omp))[fages[1]:fages[2]]), end=y-dlag)

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

