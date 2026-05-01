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
#' @param dlow Limit to decreases in output catch, as a proportional change (0.85 for 15%). Applied only when metric > lim, as set by 'hcr' step.
#' @param dupp Limit to increases in output catch, as a proportional change (1.15 for 15%). Applied only when metric > lim, as set by 'hcr' step.
#' @param fmin Minimum fbar to apply when catch change limits are use.
#' @param initac Initial catch from which to compute catch change limits. Defaults to previous observed catch.
#' @param tracking The tracking object.
#' @examples
#' data(plesim)
#' # Setup control with tac.is
#' control <- mpCtrl(list(est=mseCtrl(method=perfect.sa),
#'   hcr=mseCtrl(method=hockeystick.hcr,
#'     args=list(lim=0, trigger=14000, target=0.18)),
#'   isys=mseCtrl(method=tac.is, args=list(recyrs=-3, fnsqy=3, output='landings'))))
#' # Run MP until 2025
#' run <- mp(om, oem, ctrl=control, args=list(iy=2021, fy=2027))
#' # Plot run time series
#' plot(om, TAC.IS=run)

tac.is <- function(stk, ctrl, args, output="catch", recyrs=-2, fsqyrs=1,
  Fdevs=unitMeans(fbar(fut)) %=% 1, dlow=NA, dupp=NA, fmin=0, reuse=TRUE,
  initac=unitSums(metrics(stk, output)[, ac(iy - data_lag)]), tracking) {

  # EXTRACT args
  spread(args)

  # PREPARE stk for mys, biology as in last nsqy years
  fut <- fwdWindow(stk, end=mys[length(mys)], nsq=nsqy)

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
  gmnrec <- exp(yearMeans(log(unitSums(rec(stk))[, recyrs])))

  # SETUP SRR
  srr <- predictModel(model=rec~a, params=FLPar(a=gmnrec))

  # TRACK geomeanrec value
  track(tracking, "gmrec.isys", ay) <- gmnrec

  # ADD F deviances for 1 year
  # reuse = TRUE
  if(isTRUE(reuse) | toupper(reuse) == 'F') {
    ftar <- ctrl[1,]$value * Fdevs[, ac(mys[1])]
  # reuse = FALSE
  } else {
    ftar <- ctrl$value * Fdevs[, ac(mys)]
  }

  # TRACK Ftarget
  track(tracking, "fbar.isys", ay) <- unitMeans(ftar)

  # FORECAST for iyrs and my IF mlag > 0,
  if(management_lag > 0) {

    # TODO: ADD TAC option
 
    # SET F for intermediate year, mean dy - fsqyrs
    fsq <- yearMeans(unitMeans(fbar(stk)[, ac(seq(dy - fsqyrs + 1, dy))]))

    # CONSTRUCT fwd control
    if(data_lag == 0) {
      fctrl <- fwdControl(
        # target
        list(year=mys, quant="fbar", value=ftar))
    } else {
      fctrl <- fwdControl(c(
        # SET F for intermediate year(s)
        lapply(seq(dy + 1, mys[1] - 1), function(y) list(year=y,
          quant="fbar", value=c(fsq))),
        # ... and for management years
        lapply(mys, function(y) list(year=y, quant="fbar", value=ftar))))
    }
  # else only for my
  } else {
    fctrl <- fwdControl(
      list(year=ay, quant="fbar", value=ftar))
  }

  # RUN STF ffwd
  fut <- fwd(fut, sr=srr, control=fctrl)

  # ID iters where hcr set met trigger and F > fmin
  id <- tracking[metric  == "rule.hcr" & year == ay, data > 2] &
    c(unitMeans(fbar(fut)[, ac(ay + management_lag)]) > fmin)

  # EXTRACT catches
  if(isTRUE(reuse) | toupper(reuse) == "C") {
    TAC <- areaSums(unitSums(expand(catch(fut)[, ac(mys)[1]], year=seq(length(mys)))))
  } else {
    TAC <- areaSums(unitSums(catch(fut)[, ac(mys)]))
  }

  # TRACK initla TAC
  track(tracking, "catch.isys", ay) <- unitMeans(TAC)

  # GET TAC dy / ay - 1
  if(ay == iy)
    prev_tac <- rep(c(initac), length=args$it)
  else
    prev_tac <- c(tracking[metric == "isys" & year == ay - frq, data])

  # APPLY upper and lower TAC limit, if not NA and only for id iters
  if(!is.na(dupp)) {
    iter(TAC, id) <- pmin(c(iter(TAC, id)), prev_tac[id] * dupp)
  }
  if(!is.na(dlow)) {
    iter(TAC, id) <- pmax(c(iter(TAC, id)), prev_tac[id] * dlow)
  }

  # CONSTRUCT fwdControl  TODO: USE frq here
  ctrl <- fwdControl(lapply(seq(length(mys)), function(x)
    list(year=mys[x], quant=output, value=TAC[,x])))

  # PROJECT survivors if man_lag = 0
  if(management_lag == 0) {
    ctrl <- merge(ctrl,
      fwdControl(list(year=mys[length(mys)] + 1, quant="catch", value=0)))
  }
    
  return(list(ctrl=ctrl, tracking=tracking))
}

# }}}

# indicator.is {{{

#' Indicator Implementation System Module
#'
#' Applies the harvest control rule (HCR) decision to scale the target catch or
#' fishing mortality based on a reference quantity (status-quo fishing mortality or catch).
#'
#' This implementation system (IS) function adjusts the management decision from the HCR
#' step by multiplying it with a reference quantity computed from the stock-at-age data
#' in the perceived stock, allowing for indicator-based management approaches. The reference
#' quantity can be either the status-quo fishing mortality (input system) or catch (output system).
#'
#' @param stk The perceived FLStock object returned by the OEM module.
#' @param ctrl The fwdControl object output by the *hcr* step, containing the HCR decision.
#' @param args The MSE run arguments, including `sqy` (status-quo years).
#' @param tracking The tracking object for recording module decisions and outputs.
#' @param system Character, either "output" (default) or "input". If "output", catch is used
#'   as reference; if "input", fishing mortality is used.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing:
#'   \item{ctrl}{The modified fwdControl with scaled target values.}
#'   \item{tracking}{The updated tracking object.}
#'
#' @details
#' When `system="output"`, the function scales the HCR target by the mean catch from the
#' status-quo years. When `system="input"`, it scales by the mean fishing mortality.
#' This approach allows MPs to work with relative rather than absolute targets.
#'
#' @examples
#' data(plesim)
#' # Setup control with indicator.is using output system (catch-based)
#' control <- mpCtrl(list(est=mseCtrl(method=perfect.sa),
#'   hcr=mseCtrl(method=hockeystick.hcr,
#'     args=list(lim=0, trigger=0.5, target=1.2)),
#'   isys=mseCtrl(method=indicator.is, args=list(system="output"))))
#' # Run MP
#' run <- mp(om, oem, control=control, args=list(iy=2021, fy=2027))
#'
#' @author Ernesto Jardim, Iago Mosqueira
#' @seealso \code{\link{tac.is}}, \code{\link{sp.is}}, \code{\link{seasonal.is}}
#' @keywords manip

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
	ctrl$value <- ctrl$value * vsq

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
