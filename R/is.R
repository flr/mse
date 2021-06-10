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
#' @examples
#' data(ple4om)
#' res <- catchSSB.hcr(stock(om), MSY=140000, args=list(ay=2018, data_lag=1),
#'   tracking=FLQuant())
#' 
# args <- list(ay=2010, iy=2010, management_lag=1, nsqy=3, dy=2009)
# stk <- window(stock(om), end=args$dy)

# ADD initial TAC, initac=catch(stk[, ac(iy)])

tac.is <- function(stk, ctrl, args, dtaclow=NA, dtacupp=NA, recyrs=1,
  fmin=0, initac=catch(stk[, ac(iy - 1)]), tracking) {

  # EXTRACT args

  ay <- args$ay
  dy <- args$dy
  iy <- args$iy
  mlag <- args$management_lag
  dlag <- args$data_lag
  nsqy <- args$nsqy

  # PREPARE stk until ay + mlag, biology as in last nsqy years

  fut <- stf(stk, end=ay + mlag, wts.nyears=nsqy)

  # SET recruitment

	# Set geomean sr relationship
  gmnrec <- exp(yearMeans(log(window(rec(stk), end=-(recyrs)))))
  srr <- predictModel(model=rec~a, params=FLPar(a=gmnrec))

  # GET TAC dy / ay - 1

  if(ay == iy)
    prev_tac <- initac
  else
    prev_tac <- c(tracking[[1]]["isys", ac(dy)])

  # FORECAST for ay - dlag + 1
  # DEBUG TAC max in year 1?
  fctrl <- fwdControl(
    list(year=seq(ay - dlag + 1, length=mlag), quant="fbar",
    value=rep(c(fbar(stk)[, ac(dy)]), mlag)),
    list(year=ay + mlag, quant="fbar", value=ctrl$value))

  fut <- fwd(fut, sr=srr, control=fctrl)

  # EXTRACT catches
  TAC <- c(catch(fut)[, ac(ay + mlag)])

  # APPLY upper TAC limit
  TAC <- pmin(TAC, c(prev_tac) * dtacupp, na.rm=TRUE)

  # ID iters where F > fmin
  i <- c(fbar(fut)[, ac(ay + mlag)] > fmin)

  # APPLY lower TAC limit, only if F > fmin
  TAC[i] <- pmax(TAC[i], c(prev_tac)[i] * dtaclow, na.rm=TRUE)

  # CONSTRUCT fwdControl
  ctrl <- fwdControl(list(year=ay + mlag, quant="catch", value=TAC))

  return(list(ctrl=ctrl, tracking=tracking))
}

# }}}


# effort.is {{{

#' effort implementation function
#'
#' @param stk The perceived FLStock.
#' @param imp_control A list with the elements: nsqy, delta_tac_min, delta_tac_max
#' @param ay The year for which the target F is set, based on the SSB in year (ay - control$ssb_lag).

effort.is <- function(stk, ctrl, args, tracking){
	ay <- args$ay
	it <- dims(stk)$iter
	iy <- args$iy
	management_lag <- args$management_lag
	data_lag <- args$data_lag
	# reference value
	if(ay==iy) fay <- fbar(stk)[,ac(ay-data_lag)] else fay <- yearMeans(fbar(stk)[,ac(args$sqy)])
	# target to reach defined by HCR (in f units)
	trgt <- ctrl$value
	# multiplier
	mult <- trgt/fay
	# new control file, in relative terms
	ctrl <- getCtrl(mult, "f", ay+management_lag, it, rel.year=ay-data_lag)
	list(ctrl = ctrl, tracking = tracking)
} # }}}

#' indicator implementation function
#'
#' @param stk The perceived FLStock.
#' @param ctrl control file with HCR decision

indicator.is <- function(stk, ctrl, args, tracking, system=c("output", "input"), ...){

  	sqy <- args$sqy
  	if(system=='input'){
  		vsq <- yearMeans(fbar(stk)[,ac(sqy)]) 
  		quantity <- 'f'
  	} else {
  		vsq <- yearMeans(catch(stk)[,ac(sqy)]) 
  		quantity <- 'catch'
	}  	
	# new control file
	ctrl@target[,'quantity'] <- quantity
	ctrl$value <- ctrl$value*vsq

	# return
	list(ctrl = ctrl, tracking = tracking)
} # }}}


































