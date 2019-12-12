# sa.R - DESC
# /sa.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# perfect.sa {{{
perfect.sa <- function(stk, idx, genArgs, tracking, ...) {

  tracking["conv.est",ac(genArgs$ay)] <- 1

  list(stk=stk, tracking=tracking)
}
# }}}

# mean length of the catch - length based estimator
mlc.est <- function (stk, idx, genArgs, vbPars, ...){
    args <- list(...)
	tracking <- args$tracking
	vbObj <- a4aGr(
	    grMod=~linf*(1-exp(-k*(t-t0))),      
	    grInvMod=~t0-1/k*log(1-len/linf),      
	    params=FLPar(linf=vbPars$linf, k=vbPars$k, t0=vbPars$t0, units=c('cm','year-1','year')))
	flq <- flc <- catch.n(stk)
	flq[] <- predict(vbObj, t=range(stk)["min"]:range(stk)["max"]+0.5)
	flq <- quantSums(flc*flq)/quantSums(flc)
	attr(stk, "indicator") <- flq
    tracking["indicator.est", ac(genArgs$ay)] <- flq[,ac(genArgs$dy)]
    list(stk = stk, tracking = tracking)
}

