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

# ADD initial TAC, initac=catch(stk[, ac(iy)])

tac.is <- function(stk, ctrl, args, dtaclow=NA, dtacupp=NA, recyrs=10,
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
  gmnrec <- exp(yearMeans(log(rec(stk)[, rev(dimnames(stk)$year)[2:recyrs+1]])))
  srr <- predictModel(model=rec~a, params=FLPar(a=gmnrec))
  
  # GET TAC dy / ay - 1

  if(ay == iy)
    prev_tac <- rep(c(initac), length=args$it)
  else
    prev_tac <- c(tracking[[1]]["isys", ac(dy)])

  # FORECAST for iyrs and my IF mlag > 0, else only my
  # DEBUG TAC vs Fsquo in year 1?

  if(mlag > 0) {

    fctrl <- fwdControl(
      list(year=seq(ay - dlag + 1, length=mlag), quant="fbar",
        value=rep(c(fbar(stk)[, ac(dy)]), mlag)),
      list(year=ay + mlag, quant="fbar", value=ctrl$value))
  } else {

    fctrl <- fwdControl(
      list(year=ay + mlag, quant="fbar", value=ctrl$value))
  }

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

# indicator.is {{{

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

# seasonal.is {{{

seasonal.is <- function(stk, ctrl, args, ratio=rep(1/args$ns, args$ns),
  tracking, ...){

  nseas <- args$ns

  res <- fwdControl(
    # LAPPLY over ctrl rows
    Reduce(c, lapply(seq(dim(iters(ctrl))[1]), function(i)
      # LAPPLY over seasons
      lapply(seq(nseas), function(s)
        c(as.list(target(ctrl)[i, -3]),
          list(season=s, value=iters(ctrl)[i, 'value',] * ratio[s]))
      )
    ))
  )

  return(list(ctrl=res, tracking=tracking))
}

# }}}

# allocate.is

# splitTAC.is {{{

#' @examples
#' data(ple4)

splitTAC.is <- function(stk, ctrl, allocation, args, tracking) {

  # DIMS
  frq <- args$frq
  yrs <- ctrl$year

  ctrl <- fwdControl(
    Map(function(yr, fi) {
        list(year=yr, fishery=fi, catch=1, biol=an(NA),
    quant="catch", value=ctrl[ctrl$year==yr,]$value * allocation[fi])
        },
    yr=rep(yrs, length(allocation)),
    fi=rep(seq(length(allocation)), each=length(yrs)))
  )


  return(list(ctrl=ctrl, tracking=tracking))
}

# }}}


