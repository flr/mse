# hcr.R - DESC
# mse/R/hcr.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# catchSSB.hcr {{{

#' A HCR to set total catch based on SSB depletion level
#'
#' @param stk The perceived FLStock.
#' @param dtarget=0.40 Depletion level from which catch is decreased.
#' @param dlimit=0.10 Depletion level at which fishing is stopped.
#' @param lambda=1 Multiplier for MSY level.
#' @param MSY Assumed or estimated MSY.
#' @param dtaclow=0.85 Maximum proportional decrease in allowable catch.
#' @param dtacupp=1.15 Maximum proportional increase in allowable catch.
#' @param args MSE arguments, class *list*.
#' @param tracking Structure for tracking modules outputs.
#'
#' @return A *list* with elements *ctrl*, of class *fwdControl*, and *tracking*.
#'
#' @examples
#' data(ple4om)
#' catchSSB.hcr(stock(om), MSY=140000, args=list(ay=2018, data_lag=1),
#'   tracking=FLQuant())
#' # APPLY hcr over a range of dtarget values
#' lapply(seq(0.30, 0.90, by=0.1), function(x) {
#'   catchSSB.hcr(stock(om), MSY=140000, dtarget=x, args=list(ay=2018, data_lag=1),
#'   tracking=FLQuant())$ctrl } )

catchSSB.hcr <- function(stk, dtarget=0.40, dlimit=0.10, lambda=1, MSY,
  dtaclow=0.85, dtacupp=1.15, args, tracking) {
  
  # args
  ay <- args$ay
  data_lag <- args$data_lag

  # COMPUTE depletion
  dep <- ssb(stk)[, ac(ay - data_lag)] / ssb(stk)[, 1]

  # RULE
  ca <- ifelse(dep <= dlimit, 0,
    ifelse(dep < dtarget, (lambda * MSY) / (dtarget - dlimit) * (dep - dlimit),
    lambda * MSY))

  # CONTROL
  ctrl <- fwdControl(list(quant="catch", value=c(ca), year=ay + 1),
    # TAC limits
    list(quant="catch", min=dtaclow, max=dtacupp, relYear=ay - 1, year=ay + 1))

	return(list(ctrl=ctrl, tracking=tracking))

} # }}}

# ices.hcr {{{

#' The typical HCR used by ICES
#'
#' The typical HCR used by ICES which sets a target F based on the SSB based on 4 parameters: blim, bsafe, fmin and ftrg.
#' F increases linearly between SSB = blim and SSB = bsafe, from F = fmin to F = ftrg.
#' If:
#' B < Blim, F = Fbycatch;
#' B > trigger, F = Fmsy;
#' B > Blim & B < trigger, F linear between Fbycatch and Fmsy;
#' F = ftrg is the maximum F, F = fmin is the minimum F.
#' F is set in year ay, based on SSB in year ay - ssb_lag.
#' The control argument is a list of parameters used by the HCR.
#'
#' @param stk The perceived FLStock.
#' @param fmin Minimum fishing mortality.
#' @param ftrg [TODO:description]
#' @param blim [TODO:description]
#' @param bsafe [TODO:description]
#' @param args MSE arguments, class *list*.
#' @param tracking Structure for tracking modules outputs.
#'
#' @return A *list* with elements *ctrl*, of class *fwdControl*, and *tracking*.
#' @examples
#' data(ple4om)
#' # Test for year when SSB > bsafe
#' ices.hcr(stock(om), fmin=0.05, ftrg=0.15, blim=200000, bsafe=300000,
#'   args=list(ay=2018, data_lag=1, management_lag=1), tracking=FLQuant())
#' # Test for year when SSB < bsafe
#' ices.hcr(stock(om), fmin=0.05, ftrg=0.15, blim=200000, bsafe=300000,
#'   args=list(ay=1995, data_lag=1, management_lag=1), tracking=FLQuant())

ices.hcr <- function(stk, ftrg, blim, bsafe, fmin=0, args, tracking){

  # args
	ay <- args$ay
	ssb_lag <- args$data_lag
	man_lag <- args$management_lag

  # GET ssb metric
	ssb <- ssb(stk)[, ac(ay - ssb_lag)]

	# APPLY rule

	fout <- FLQuant(fmin, dimnames=list(iter=dimnames(ssb)$iter))
	fout[ssb >= bsafe] <- ftrg
	inbetween <- (ssb < bsafe) & (ssb > blim)
	gradient <- (ftrg - fmin) / (bsafe - blim)
	fout[inbetween] <- (ssb[inbetween] - blim) * gradient + fmin
	
  # CREATE control file
  ctrl <- fwdControl(year=ay + man_lag, quant="fbar", value=c(fout))

	list(ctrl=ctrl, tracking=tracking)
} # }}}

# fixedF.hcr {{{

#' A fixed target f
#'
#' No matter what get F = Ftarget
#' The control argument is a list of parameters used by the HCR.
#' @param stk The perceived FLStock.
#' @param control A list with the element ftrg (numeric).
fixedF.hcr <- function(stk, ftrg, args, tracking){
	ay <- args$ay
	# rule 
	if(!is(ftrg, "FLQuant"))
    ftrg <- FLQuant(ftrg, dimnames=list(iter=dimnames(stk@catch)$iter))

	# create control file
	ctrl <- getCtrl(c(ftrg), "f", ay+args$management_lag, dim(ftrg)[6])
	
	# return
	list(ctrl=ctrl, tracking=tracking)
} # }}}

# movingF.hcr {{{

movingF.hcr <- function(stk, hcrpars, args, tracking){
	ay <- args$ay
	# rule 
	if(!is(hcrpars, "FLQuant"))
    hcrpars <- FLQuant(c(hcrpars), dimnames=list(iter=dimnames(stk@catch)$iter))
	
  # create control file
	ctrl <- getCtrl(c(hcrpars), "f", ay+args$management_lag, dim(hcrpars)[6])
	
  # return
	list(ctrl=ctrl, tracking=tracking)
} # }}}

#' An indicator-based HCR
#'
#' Get indicator to target. The control argument is a list of parameters used by the HCR.
#' @param stk The perceived FLStock.
#' @param itrg The target for the indicator.
#' @param args A list with generic arguments to be used by the function if needed.
#' @param tracking The tracking matrix.
indicator.hcr <- function (stk, hcrpars, args, tracking) {
    ay <- args$ay
    dy <- args$dy
  	#sqy <- args$sqy
  	mlag <- args$management_lag
	if(!is(hcrpars, "FLQuant"))
    hcrpars <- FLQuant(hcrpars, dimnames=list(iter=dimnames(stk@catch)$iter))
	mult <- stk@indicator[,ac(dy)] / hcrpars
	#csq <- yearMeans(catch(stk)[,ac(dy)])
    ctrl <- getCtrl(mult, "f", ay + mlag, dim(hcrpars)[6])
    list(ctrl = ctrl, tracking = tracking)
}


# cpue.hcr {{{
#' cpue.hcr
#'
#' @examples
#' data(ple4)

cpue.hcr <- function(stk, ay, k1, k2, k3, k4, target=1,
  dtaclow=0.85, dtacupp=1.15, tracking){
  
  # RECOVER slope & mean(cpue)
  slope <- tracking["cpue.slope", ac(ay)]
  mcpue <- tracking["cpue.mean", ac(ay)]

  # CALCULATE new tac

  ka <- ifelse(slope > 0, k1, k2)
  kb <- ifelse(mcpue > target, k3, k4)

  # TAC_y-1 ~ TAC_y * 1 + ka * m + kb * (mcpue - target)
  tac <- catch(stk)[, ac(ay-1)] * (1 + ka * slope + kb * (mcpue - target))

  ctrl <- fwdControl(list(quant="catch", value=tac, year=ay + 1),
    # TAC limits
    list(quant="catch", min=dtaclow, max=dtacupp, relYear=ay - 1, year=ay + 1))
  
	return(list(ctrl=ctrl, tracking=tracking))
} # }}}

