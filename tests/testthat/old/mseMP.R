# mseMP.R - DESC
# /mseMP.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# XX {{{
# }}}

#' 
data(cod)
omp <- cod
sr <- codsr
index <- stock(cod)
years <- seq(20, 30, by=2)

hcr <- ~pmax(Fmin, pmin(Ftarget, ssb * (Ftarget / (Btrigger - Blim))))

hcr <- ~hockeystick

hockeystick <- function(ssb, params) {

  # GET expected
  res <- pmax(params$Fmin, pmin(params$Ftarget, ssb *
    (params$Ftarget / (params$Btrigger - params$Blim))))

  # LIMITS to change in F

  # LIMITS to change in TAC

  # CHECK B > Blim

  return(res)
}

hcrparams <- FLPar(Btrigger=600, Blim=200, Ftarget=refpts$Fmsy, Fmin=0.05)
refpts <- FLPar(Fmsy=0.15)

evalPredictModel(omp[,ac(y-1)], predictModel(model=hcr, params=rbind(refpts, hcrparams)))


