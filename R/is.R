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
#'   isys=mseCtrl(method=tac.is, args=list(recyrs=-3, output='landings'))))
#' # Run MP until 2025
#' run <- mp(om, oem, ctrl=control, args=list(iy=2021, fy=2027))
#' # Plot run time series
#' plot(om, TAC.IS=run)

tac.is <- function(stk, ctrl, args, output="catch", recyrs=-2,
  Fdevs=fbar(fut) %=% 1, dtaclow=NA, dtacupp=NA, fmin=0, reuse=TRUE,
  initac=metrics(stk, output)[, ac(iy - 1)], tracking) {

  # EXTRACT args
  spread(args)

  # SET control years
  cys <- seq(ay + management_lag, ay + management_lag + frq - 1)

  # PREPARE stk for cys, biology as in last nsqy years
  if(management_lag == 0) {
    fut <- stk
  } else {
    fut <- fwdWindow(stk, end=cys[length(cys)], nsq=nsqy)
  }

  # PARSE recyrs if numeric
  id <- dimnames(stk)$year

  # COERCE to list
  if(!is.list(recyrs)) {
    recyrs <- list(recyrs)
  }
  
  # PARSE list
  for(i in recyrs) {
    #
    if(is(i, 'character')) {
      id <- id[!id %in% i]
    } else if(all(i < 0)) {
      if(length(i) == 1)
        id <- rev(rev(id)[-seq(abs(i))])
      else
        id <- rev(rev(id)[i])
    } else if(all(i > 0)) {
      id <- rev(rev(id)[seq(abs(i))])
    }
  }

  # SET years to use
  recyrs <- id

  # CHECK recyrs
  if(!all(recyrs %in% dimnames(stk)$year)) {
    stop("'recyrs' cannot be found in input stk")
  }

  # TODO: OTHER rec options
  
  # SET GM recruitment from past
  gmnrec <- unitSums(exp(yearMeans(log(rec(stk)[, recyrs]))))

  # SETUP SRR
  srr <- predictModel(model=rec~a, params=FLPar(a=gmnrec))

  # STORE geomeanrec value
  track(tracking, "gmrec.isys", ay + management_lag) <- gmnrec

  # ADD F deviances for 1 year

  # reuse = TRUE
  if(isTRUE(reuse) | toupper(reuse) == 'F') {
    ftar <- rep(c(ctrl[1,]$value * Fdevs[, ac(cys[1])]), length(cys))
  # reuse = FALSE
  } else {
    ftar <- c(ctrl$value * Fdevs[, ac(cys)])
  }

  # TRACK Ftarget
  track(tracking, "fbar.isys", cys) <- ftar

  # FORECAST for iyrs and my IF mlag > 0,
  if(management_lag > 0) {
 
    # SET F for intermediate year
    fsq <- fbar(stk)[, ac(dy)]

    # TODO: ADD TAC option

    # CONSTRUCT fwd control
    if(data_lag == 0) {
      fctrl <- fwdControl(
        # target
        list(year=cys, quant="fbar", value=c(ftar)))
    } else {
      fctrl <- fwdControl(
        # ay as intermediate with Fsq TODO: Other options
        list(year=seq(ay - data_lag + 1, length=management_lag),
          quant="fbar", value=rep(c(fsq), management_lag)),
        # target
        list(year=cys, quant="fbar", value=c(ftar)))
    }

  # else only for my
  } else {
    fctrl <- fwdControl(
      list(year=ay + management_lag, quant="fbar", value=ftar))
  }

  # RUN STF ffwd
  fut <- ffwd(fut, sr=srr, control=fctrl)

  # ID iters where hcr set met trigger and F > fmin
  id <- c(tracking[[1]]["decision.hcr", ac(ay)] > 2) &
    c(fbar(fut)[, ac(ay + management_lag)] > fmin)

  # EXTRACT catches
  if(isTRUE(reuse) | toupper(reuse) == "C") {
    TAC <- expand(catch(fut)[, ac(cys)[1]], year=seq(length(cys)))
  } else {
    TAC <- catch(fut)[, ac(cys)]
  }

  # GET TAC dy / ay - 1
  if(ay == iy)
    prev_tac <- rep(c(initac), length=args$it)
  else
    prev_tac <- c(tracking[[1]]["isys", ac(ay)])

  # APPLY upper and lower TAC limit, if not NA and only for id iters
  if(!is.na(dtacupp)) {
    iter(TAC, id) <- pmin(c(iter(TAC, id)), prev_tac[id] * dtacupp)
  }
  if(!is.na(dtaclow)) {
    iter(TAC, id) <- pmax(c(iter(TAC, id)), prev_tac[id] * dtaclow)
  }

  # CONSTRUCT fwdControl  TODO: USE frq here
  ctrl <- fwdControl(lapply(seq(length(cys)), function(x)
    list(year=cys[x], quant=output, value=TAC[,x])))

  # PROJECT survivors if man_lag = 0
  if(management_lag == 0) {
    ctrl <- merge(ctrl,
      fwdControl(list(year=cys[length(cys)] + 1, quant="catch", value=0)))
  }
    
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

# sp.is {{{

sp.is <- function(stk, ctrl, Ftarget=refpts(stk)$Ftarget,
  metric="stock", output="catch", args, tracking) {
  
  ctrl$quant <- output
  ctrl$value <- ctrl$value * do.call(metric, list(stk))[, ac(args$dy)] *
    Ftarget
  if(!output %in% c('f', 'fbar'))
    ctrl$minAge <- ctrl$maxAge <- NA

  return(list(ctrl=ctrl, tracking=tracking))
}


# }}}

# split.is {{{

#' Split a biol target across fleets by setting relative 'quant' proportions
#'
#' @examples
#' data(ple4)

split.is <- function(stk, ctrl, split, quant=unique(ctrl$quant)[1],
  args, tracking) {

  # DIMS
  frq <- args$frq
  yrs <- ctrl$year

  # SET as proportions
  split <- unlist(split) / sum(unlist(split))

  # SPLIT
  ctrl <- fwdControl(lapply(seq(split), function(f)
    list(fishery=f, catch=1, year=ctrl$year, quant=ac(ctrl$quant),
      value=ctrl$value * split[f])))

  if(quant %in% c("f", "fbar")) {
    ctrl$biol <- args$stock
    ctrl$minAge <- range(stk, 'minfbar')
    ctrl$maxAge <- range(stk, 'maxfbar')
  }

  return(list(ctrl=ctrl, tracking=tracking))
}

# }}}
