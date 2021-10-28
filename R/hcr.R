# hcr.R - DESC
# mse/R/hcr.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# ices.hcr {{{

#' The typical HCR used by ICES
#'
#' The typical HCR used by ICES which sets a target F based on the SSB based on 4 parameters: sblim, sbsafe, fmin and ftrg.
#' F increases linearly between SSB = blim and SSB = bsafe, from F = fmin to F = ftrg.
#' If:
#' - B < Blim, F = Fbycatch;
#' - B > trigger, F = Fmsy;
#' - B > Blim & B < trigger, F linear between Fbycatch and Fmsy;
#' - F = ftrg is the maximum F, F = fmin is the minimum F.
#' F is set in year ay, based on SSB in year ay - data_lag
#'
#' @param stk The perceived FLStock.
#' @param fmin Minimum fishing mortality.
#' @param ftrg [TODO:description]
#' @param sblim [TODO:description]
#' @param sbsafe [TODO:description]
#' @param args MSE arguments, class *list*.
#' @param tracking Structure for tracking modules outputs.
#'
#' @return A *list* with elements *ctrl*, of class *fwdControl*, and *tracking*.
#' @examples
#' data(ple4om)
#' # Test for year when SSB > bsafe
#' ices.hcr(stock(om), fmin=0.05, ftrg=0.15, sblim=200000, sbsafe=300000,
#'   args=list(ay=2018, data_lag=1, management_lag=1), tracking=FLQuant())
#' # Test for year when SSB < bsafe
#' ices.hcr(stock(om), fmin=0.05, ftrg=0.15, sblim=200000, sbsafe=300000,
#'   args=list(ay=1995, data_lag=1, management_lag=1), tracking=FLQuant())

ices.hcr <- function(stk, ftrg, sblim, sbsafe, fmin=0, args, tracking){

  # args
	ay <- args$ay
	data_lag <- args$data_lag
	man_lag <- args$management_lag

  # GET ssb metric
	ssb <- unitSums(ssb(stk)[, ac(ay - data_lag)])

	# APPLY rule

	fout <- FLQuant(fmin, dimnames=list(iter=dimnames(ssb)$iter))
	fout[ssb >= sbsafe] <- ftrg
	inbetween <- (ssb < sbsafe) & (ssb > sblim)
	gradient <- (ftrg - fmin) / (sbsafe - sblim)
	fout[inbetween] <- (ssb[inbetween] - sblim) * gradient + fmin
	
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
#' @examples
#' data(ple4om)
#' fixedF.hcr(stock(om), ftrg=0.15, args=list(ay=2017, management_lag=1,
#'   frq=1), tracking=FLQuant())

fixedF.hcr <- function(stk, ftrg, args, tracking){

  # args
	ay <- args$ay
  mlag <- args$management_lag
  frq <- args$frq

	# create control object
  ctrl <- fwdControl(year=seq(ay + mlag, ay + frq), quant="fbar", value=c(ftrg))

	# return
	list(ctrl=ctrl, tracking=tracking)

} # }}}

# fixedC.hcr {{{

#' A fixed catch HCR
#'
#' No matter what get C = ctrg
#' The control argument is a list of parameters used by the HCR.
#' @param stk The perceived FLStock.
#' @param control A list with the element ctrg (numeric).
#' @examples
#' data(ple4om)
#' fixedC.hcr(stock(om), ctrg=50000, args=list(ay=2017, management_lag=1,
#'   frq=1), tracking=FLQuant())

fixedC.hcr <- function(stk, ctrg, args, tracking){

  # args
	ay <- args$ay
  mlag <- args$management_lag
  frq <- args$frq

	# create control object
  ctrl <- fwdControl(year=seq(ay + mlag, ay + frq), quant="catch", value=c(ctrg))

	# return
	list(ctrl=ctrl, tracking=tracking)

} # }}}

# movingF.hcr {{{

#' [TODO:description]
#'
#' @param stk [TODO:description]
#' @param hcrpars [TODO:description]
#' @param args [TODO:description]
#' @param tracking [TODO:description]
#'
#' @return [TODO:description]
#' @export
#'
#' @examples

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
#' catchSSB.hcr(stock(om), MSY=140000, tracking=FLQuant(),
#' args=list(ay=2018, data_lag=1, management_lag=1, frq=1))
#' # APPLY hcr over a range of dtarget values
#' lapply(seq(0.30, 0.80, by=0.1), function(x) {
#'   catchSSB.hcr(stock(om), MSY=140000, dtarget=x,
#'   args=list(ay=2018, data_lag=1, management_lag=1, frq=1),
#'   tracking=FLQuant())$ctrl } )

catchSSB.hcr <- function(stk, dtarget=0.40, dlimit=0.10, lambda=1, MSY,
  dtaclow=0.85, dtacupp=1.15, yrs=1, args, tracking) {
  
  # args
  ay <- args$ay
  data_lag <- args$data_lag
  man_lag <- args$management_lag
  frq <- args$frq

  # SET tac limits if NA
  if(is.na(dtaclow))
    dtaclow <- 1e-8
  if(is.na(dtacupp))
    dtacupp <- 1e8

  # COMPUTE depletion, across units
  dep <- yearMeans(unitSums(window(stock(stk), start=ay - data_lag - yrs,
    end=ay - data_lag))) / unitSums(stock(stk)[, 1])

  # RULE
  ca <- ifelse(dep <= dlimit, 1e-8,
    ifelse(dep < dtarget, (lambda * MSY) / (dtarget - dlimit) * (dep - dlimit),
    lambda * MSY))

  # IF NA, set to previous TAC
  if(any(is.na(ca)))
    ca[is.na(ca)] <- tracking[[1]]['hcr', ac(ay - 1)][is.na(ca)]

  # CONTROL
  ctrl <- fwdControl(
    # TAC for frq years
    c(lapply(seq(ay + man_lag, ay + frq), function(x)
      list(quant="catch", value=c(ca), year=x)),
    # TAC change limits
    lapply(seq(ay + man_lag, ay + frq), function(x)
      list(quant="catch", min=rep(dtaclow, dim(ca)[6]), max=rep(dtacupp, dim(ca)[6]),
        year=x, relYear=x-1)))
  )

	return(list(ctrl=ctrl, tracking=tracking))

} # }}}

# indicator.hcr {{{

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
# }}}

# cpue.hcr {{{

#' cpue.hcr
#'
#' @examples
#' data(ple4om)
#' cpue.hcr(stock(om), k1=0.1, k2=0.2, k3=0.1, k4=0.1, args=list(ay=1990),
#'  tracking=FLQuants(FLQuant(c(0.5, 0.8), dimnames=list(metric=c("cpue.slope",
#'  "cpue.mean"), year=1990))))

cpue.hcr <- function(stk, k1, k2, k3, k4, target=1,
  dtaclow=0.85, dtacupp=1.15, args, tracking){
  
  # args
  ay <- args$ay

  # RECOVER slope & mean(cpue)

  slope <- tracking[[1]]["cpue.slope", ac(ay)]
  mcpue <- tracking[[1]]["cpue.mean", ac(ay)]

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
