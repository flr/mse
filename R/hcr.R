# hcr.R - DESC
# mse/R/hcr.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' Evaluate the chosen HCR function
#'
#' Evaluate the chosen HCR function using the current stock perception and a control
#' @param method Name of the chosen HCR function.
#' @param stk The perceived stock.
#' @param ay The current year. The management control (e.g. F or effort) will be set in ay+1.
#' @param EFF Effort array (if effort management is being used).
#' @param EFF0 Tracking array.
#' @param control The control object for the chosen HCR function. A list of parameters.
#h <- function(...){
#	args <- list(...)
#	method <- args$method
#	args$method <- NULL
#	# Check inputs
#	if(!is(args$stk,"FLStock")) stop("stk argument must be an FLStock")
#	# dispatch
#	ctrl <- do.call(method, args)
#	# check outputs
#	if(!is(ctrl, "fwdControl")) stop("The HCR must return and object of class fwdControl")	
#	# return
#	ctrl  
#}

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
#' @param stk The perceived FLStock.
#' @param control A list with the elements fmin, ftrg, blim, bsafe and ssb_lag, all of which are numeric.
#' @param ay The year for which the target F is set, based on the SSB in year (ay - control$ssb_lag).
ices.hcr <- function(stk, fmin, ftrg, blim, bsafe, args, tracking){
	ay <- args$ay
	ssb_lag <- ifelse(is.null(args$ssb_lag), 1, args$ssb_lag)
	# rule
	ssb <- ssb(stk)[, ac(ay-ssb_lag)]
	fout <- FLQuant(fmin, dimnames=list(iter=dimnames(ssb)$iter))
	fout[ssb >= bsafe] <- ftrg
	inbetween <- (ssb < bsafe) & (ssb > blim)
	gradient <- (ftrg - fmin) / (bsafe - blim)
	fout[inbetween] <- (ssb[inbetween] - blim) * gradient + fmin
	# create control file
	ctrl <- getCtrl(c(fout), "f", ay+args$management_lag, dim(fout)[6])
	# return
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

# catchSSB.hcr {{{

#'
# hcrparams=FLPar(dlimit=0.10, dtarget=0.40, lambda=1.0, dltac=0.15, dhtac=0.15),

#' @param dtarget
#' @param dlimit
#' @param lambda
#' @param MSY
#' @examples
#' data(ple4)
#' # APPLY hcr
#' catchSSB.hcr(ple4[,ac(1957:1996)], dtarget=0.40, dlimit=0.10, lambda=1,
#'   MSY=95000, ay=1995, tracking=FLQuant())
#' # APPLY hcr over a range of dtarget
#' lapply(seq(0.30, 0.90, by=0.1), function(x) {
#'    catchSSB.hcr(ple4[,ac(1957:1996)], dtarget=x, dlimit=0.10, lambda=1,
#'   MSY=95000, ay=1995, tracking=FLQuant())$ctrl})



#' A HCR to set total catch based on SSB
#'
#' @param stk The perceived FLStock.
#' @param dtarget=0.40 Depletion level from which catch is decreased.
#' @param dlimit=0.10 Depletion level at which fishing is stopped.
#' @param lambda=1 Responsiveness of the rule
#' @param MSY [TODO:description]
#' @param dtaclow=0.85 [TODO:description]
#' @param dtacupp=1.15 [TODO:description]
#' @param args [TODO:description]
#' @param tracking [TODO:description]
#'
#' @return [TODO:description]
#' @export
#'
#' @examples
#' [TODO:example]
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

