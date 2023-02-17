# is.R - DESC
# mse/R/is.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# tac.is {{{


#' TAC implementation system module
#'
#' Performs a short term forecast (STF) for the target fishing mortality to
#' obtain the corresponding catch.
#'
#' A `fwdControl` object obtained from the 'hcr' step is applied in the
#' management year (`ay + mlag`) or years (`seq(ay + mlag, ay + mlag + freq`).
#' An assumption is made on the mortality in the assessment year (`ay`), which
#' becomes the intermediate year in this projection. By default this is set
#' to Fbar = Fstatusquo, that is, the same fishing mortality estimated in the 
#' last data year (`ay - data_lag`).
#'
#' - recruitment
#'

#' The projection uses geometric mean recruitment. Fbar in the intermediate years (i.e. years between last data year and ay+1) are set as the mean of the last nsqy years.
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

# ADD initial TAC or F, initac=catch(stk[, ac(iy)])

# all years - 2
# -25 

tac.is <- function(stk, ctrl, args, dtaclow=NA, dtacupp=NA, recyrs=30,
  fmin=0, initac=catch(stk[, ac(iy - 1)]), Fdevs=fbar(stk) %=% 1, tracking) {

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
  gmnrec <- exp(yearMeans(log(rec(stk)[, rev(dimnames(stk)$year)[2:recyrs+1]])))
  srr <- predictModel(model=rec~a, params=FLPar(a=gmnrec))
  
  # ADD F deviances
  ftar <- ctrl$value * Fdevs[, ac(dy)]

  # FORECAST for iyrs and my IF mlag > 0, else only my
  if(mlag > 0) {
 
    # SET F for intermediate year
    fsq <- fbar(stk)[, ac(dy)]

    # TODO: ADD catch option

    # CONSTRUCT fwd control
    fctrl <- fwdControl(
      # ay as intermediate
      list(year=seq(ay - dlag + 1, length=mlag), quant="fbar",
        value=rep(c(fsq), mlag)),
      # target
      list(year=ay + mlag, quant="fbar", value=ftar))

  } else {

    fctrl <- fwdControl(
      list(year=ay + mlag, quant="fbar", value=ftar))
  }

  # RUN STF fwd
  fut <- ffwd(fut, sr=srr, control=fctrl)

  # EXTRACT catches
  TAC <- c(catch(fut)[, ac(ay + mlag)])

  # ID iters where hcr set met trigger and F > fmin
  id <- c(tracking[[1]]["decision.hcr", ac(ay)] > 2) &
    c(fbar(fut)[, ac(ay + mlag)] > fmin)

  # GET TAC dy / ay - 1
  if(ay == iy)
    prev_tac <- rep(c(initac), length=args$it)
  else
    prev_tac <- c(tracking[[1]]["isys", ac(ay)])

  # APPLY upper and lower TAC limit, only for id
  TAC[id] <- pmin(TAC[id], c(prev_tac)[id] * dtacupp)
  TAC[id] <- pmax(TAC[id], c(prev_tac)[id] * dtaclow)

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

seasonal.is <- function(stk, ctrl, args, ratio=rep(1 / args$ns, args$ns),
  tracking, ...) {

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

# TODO: TURN splitTAC into allocate.is

# splitTAC.is {{{

#' @examples
#' data(ple4)

splitTAC.is <- function(stk, ctrl, allocation, args, tracking) {

  # DIMS
  frq <- args$frq
  yrs <- ctrl$year

  # CHECK quant in fwdControl is catch
  if(ctrl$quant != "catch")
    stop("splitTAc.is expects a catch-based fwdControl")

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
