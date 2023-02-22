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
#' to Fbar = Fsq, that is, the same fishing mortality estimated in the 
#' last data year (`ay - data_lag`).
#'
#' The projection applies a constant recruitment, equal to the geometric mean
#' over an specified number of years. By default all years minus the last two
#' are included in the calculation. An specific set of years can be employed,
#' by specifying a character vector of year names, or two values can be given
#' for the number of years to be inlcuded, counting from the last, and how many
#' years to exclude at the end. For example, `c(30, 2)` will use the last 30
#' years but excluding the last two, usually worst estimated.
#'
#' @param stk The perceived FLStock.
#' @param ctrl The fwdControl output by the *hcr* step, target must be 'fbar'.
#' @param args The MSE run arguments.
#' @param recyrs Years to use for geometric mean recruitment if projection. Defaults to all years minus the last two.
#' @param Fdevs Deviances on the fbar input to incorporate error and bias when MP is run using the pseudo-estimators 'perfect.sa' or 'shortcut.sa'.
#' @param dtaclow Limit to decreases in output catch, as a proportional change (0.85 for 15%). Applied only when metric > lim, as set by 'hcr' step.
#' @param dtacupp Limit to increases in output catch, as a proportional change (1.15 for 15%). Applied only when metric > lim, as set by 'hcr' step.
#' @param fmin Minimum fbar to apply when catch change limits are use.
#' @param initac Initial catch from which to compute catch change limits. Defaults to previous observed catch.
#' @param tracking The tracking object.
#' @examples
#' data(sol274)
#' # Setup control with tac.is
#' control <- mpCtrl(list(est=mseCtrl(method=perfect.sa),
#'   hcr=mseCtrl(method=hockeystick.hcr,
#'     args=list(lim=0, trigger=4.3e5, target=0.21)),
#'   isys=mseCtrl(method=tac.is)))
#' # Run MP until 2025
#' run <- mp(om, oem, ctrl=control, args=list(iy=2021, fy=2025))
#' # Plot run time series
#' plot(om, TAC.IS=run)

tac.is <- function(stk, ctrl, args,
  recyrs=c(dims(stk)$year, 2), Fdevs=fbar(stk) %=% 1,
  dtaclow=NA, dtacupp=NA, fmin=0, initac=catch(stk[, ac(iy - 1)]),
  tracking) {

  # EXTRACT args
  spread(args)

  # PREPARE stk until ay + mlag, biology as in last nsqy years
  fut <- fwdWindow(stk, end=ay + management_lag, nsq=nsqy)
  
  # PARSE recyrs
  if(is.numeric(recyrs)) {
    if(length(recyrs) == 1) {
      if(recyrs < 0) {
        recyrs <- c(dims(stk)$year, -recyrs)
      } else {
        recyrs <- c(recyrs, 0)
      }
    }
    id <- seq(dim(stk)[2] - recyrs[1] + 1, length=recyrs[1] - recyrs[2])
    recyrs <- dimnames(stk)$year[id]
  }

  # CHECK recyrs
  if(!all(recyrs %in% dimnames(stk)$year))
    stop("'recyrs' cannot be found in input stk")

  # SET GM recruitment
  
  gmnrec <- exp(yearMeans(log(rec(stk)[, recyrs])))

  srr <- predictModel(model=rec~a, params=FLPar(a=gmnrec))

  # ADD F deviances
  ftar <- ctrl$value * Fdevs[, ac(dy)]

  # FORECAST for iyrs and my IF mlag > 0,
  if(management_lag > 0) {
 
    # SET F for intermediate year
    fsq <- fbar(stk)[, ac(dy)]

    # TODO: ADD catch option

    # CONSTRUCT fwd control
    fctrl <- fwdControl(
      # ay as intermediate with Fsq
      list(year=seq(ay - data_lag + 1, length=management_lag), quant="fbar",
        value=rep(c(fsq), management_lag)),
      # target
      list(year=ay + management_lag, quant="fbar", value=ftar))

  # else only for my
  } else {
    fctrl <- fwdControl(
      list(year=ay + management_lag, quant="fbar", value=ftar))
  }

  # RUN STF ffwd
  fut <- ffwd(fut, sr=srr, control=fctrl)

  # EXTRACT catches
  TAC <- c(catch(fut)[, ac(ay + management_lag)])

  # ID iters where hcr set met trigger and F > fmin
  id <- c(tracking[[1]]["decision.hcr", ac(ay)] > 2) &
    c(fbar(fut)[, ac(ay + management_lag)] > fmin)

  # GET TAC dy / ay - 1
  if(ay == iy)
    prev_tac <- rep(c(initac), length=args$it)
  else
    prev_tac <- c(tracking[[1]]["isys", ac(ay)])

  # APPLY upper and lower TAC limit, if not NA and only for id iters
  if(!is.na(dtacupp)) {
    TAC[id] <- pmin(TAC[id], c(prev_tac)[id] * dtacupp)
  }
  if(!is.na(dtaclow)) {
    TAC[id] <- pmax(TAC[id], c(prev_tac)[id] * dtaclow)
  }

  # CONSTRUCT fwdControl
  ctrl <- fwdControl(list(year=ay + management_lag, quant="catch", value=TAC))

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
