# is.R - DESC
# mse/R/is.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# tac.is {{{


#' TAC implementation function
#'
#' Performs a short term forecast (STF) to hit the target F in year ay+1.
#' The resulting catch in year ay+1 is the TAC, i.e. the TAC that will result in Fbar = Ftarget.
#' The STF uses geometric mean recruitment. Fbar in the intermediate years (i.e. years between last data year and ay+1) are set as the mean of the last nsqy years.
#' The control argument is a list of parameters:
#' nsqy - number of years to average over to get Fbar for STF
#' delta_tac_min - constraint on the TAC
#' delta_tac_max - constraint on the TAC
#' @param stk The perceived FLStock.
#' @param imp_control A list with the elements: nsqy, delta_tac_min, delta_tac_max
#' @param ay The year for which the target F is set, based on the SSB in year (ay - control$ssb_lag).
#' @param EFF0 The tracking array
#' @param EFF Not used by this function but may be used by the other implementation functions

tac.is <- function(stk, ctrl, genArgs, delta_tac_max=NA, delta_tac_min=NA, tracking){
	ay <- genArgs$ay
	nsqy <- genArgs$nsqy
	iy <- genArgs$iy
	mlag <- genArgs$management_lag
	it <- dim(stk)[6]
	# reference value
	#if(ay==iy) refCatch <- tracking["C.om", ac(ay-1)] else refCatch <- tracking["metric.is", ac(ay-1)]
	if(ay==iy) refCatch <- catch(stk)[, ac(ay-1)] else refCatch <- tracking["metric.is", ac(ay-1)]
	# Year range of perceived stock
	yrs <- as.numeric(dimnames(stock.n(stk))$year)
	last_data_yr <- yrs[length(yrs)]
	# Status quo years
	sqy <- (last_data_yr-nsqy+1):last_data_yr
	# Get the Fbar for the intermediate years
	fsq0 <- yearMeans(fbar(stk)[,ac(sqy)])
	# Number of intermediate years (between last data year and ay+1)
	ninter_yrs <- ay - last_data_yr + mlag - 1
	# Control object for the STF
	ctrl <- getCtrl(c(rep(fsq0, times=ninter_yrs), ctrl@trgtArray[,"val",]), "f", (last_data_yr+1):(ay+mlag), it)
	# Number of projection years
	nproj_yrs <- (ay+mlag) - last_data_yr
	stkTmp <- stf(stk, nproj_yrs, wts.nyears=nsqy)
	# Set geomean sr relationship
	gmean_rec <- c(exp(yearMeans(log(rec(stk)[,ac(sqy)]))))
	# Project!
	stkTmp <- fwd(stkTmp, ctrl=ctrl, sr=list(model="mean", params = FLPar(gmean_rec,iter=it)))
	# Get TAC for the management year that results from hitting the F in STF
	TAC <- catch(stkTmp)[,ac(ay+mlag)]
	# catch stabelizers
	upper_limit <- refCatch * delta_tac_max
	lower_limit <- refCatch * delta_tac_min
	TAC <- pmin(c(upper_limit), c(TAC), na.rm=TRUE)
	TAC <- pmax(c(lower_limit), c(TAC), na.rm=TRUE)
	# new control file
	ctrl <- getCtrl(c(TAC), "catch", ay+mlag, it)
	list(ctrl = ctrl, tracking = tracking)
} # }}}

# effort.is {{{

#' effort implementation function
#'
#' @param stk The perceived FLStock.
#' @param imp_control A list with the elements: nsqy, delta_tac_min, delta_tac_max
#' @param ay The year for which the target F is set, based on the SSB in year (ay - control$ssb_lag).

effort.is <- function(stk, ctrl, genArgs, tracking){
	ay <- genArgs$ay
	it <- dims(stk)$iter
	iy <- genArgs$iy
	mlag <- genArgs$management_lag
	# reference value
	if(ay==iy) fay <- fbar(stk)[,ac(ay-1)] else fay <- tracking["metric.is",ac(ay-1)]*tracking["F.est",ac(ay)]	
	# target to reach defined by HCR (in f units)
	trgt <- ctrl@trgtArray[,"val",]
	# multiplier
	mult <- trgt/fay
	# new control file, in relative terms
	ctrl <- getCtrl(mult, "f", ay+mlag, it, rel.year=ay)
	list(ctrl = ctrl, tracking = tracking)
} # }}}











