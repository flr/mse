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

ices.hcr <- function(stk, ftrg, sblim, sbsafe, fmin=0,
  minfbar=range(stk, "minfbar"), maxfbar=range(stk, "maxfbar"), args, tracking){

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
  ctrl <- fwdControl(year=ay + man_lag, quant="fbar", value=c(fout),
    minAge=minfbar, maxAge=maxfbar)

	list(ctrl=ctrl, tracking=tracking)
} # }}}

# hockeystick.hcr {{{

#' @param stk
#' @param lim Value of metric at which output is set to 'min'.
#' @param trigger Value of metric below which output is linearly reduced towards 'min'.
#' @param target Output value when metric is greater or equal to 'trigger'.
#' @param min Minimum output value, applied if metric is below 'min'
#' @param metric Quantity computed from 'stk', defaults to ssb. Function or character.
#' @param output Quantity employed if forecast, defaults to 'fbar', character.
#' @param dlow
#' @param dupp
#' @param args
#' @param tracking
#' @examples
#' data(ple4)
#' args <- list(ay=2015, data_lag=1, management_lag=1, frq=1)
#' # Set as fbar ~ ssb
#' hockeystick.hcr(ple4, lim=3e5, trigger=4e5, target=0.25, min=0,
#'   metric="ssb", output="fbar", args=args, tracking=FLQuant())
#' # Use for catch ~ depletion, with metric as a new function
#' hockeystick.hcr(ple4, lim=0.10, trigger=0.40, target=140000, min=0,
#'   metric=function(x) ssb(x) %/% ssb(x)[,1],
#'   output="catch", dlow=0.85, dupp=1.15, args=args, tracking=FLQuant())

# TODO USE hcrpars / refpts in metric

hockeystick.hcr <- function(stk, lim, trigger, target, min=0, metric="ssb",
  output="fbar", dlow=NA, dupp=NA, args, tracking) {

  # EXTRACT args
  ay <- args$ay
  data_lag <- args$data_lag
  man_lag <- args$management_lag
  frq <- args$frq

  # SET limits if NA
  if(is.na(dlow))
    dlow <- 1e-8
  if(is.na(dupp))
    dupp <- 1e8

  # COMPUTE metric
  met <- window(do.call(metric, list(stk)), start=ay - data_lag,
    end=ay - data_lag)

  # RULE
    # BELOW lim
  out <- ifelse(met <= lim, min,
    # BETWEEN lim and trigger
    ifelse(met < trigger,
      pmax(c(target * ((met - trigger) / (trigger - lim) + 1)),  min),
    # ABOVE trigger
    target))
    
  # IF NA, set to previous value
  if(any(is.na(out)))
    out[is.na(out)] <- tracking[[1]]['hcr', ac(ay - 1)][is.na(out)]

  # CONTROL
  ctrl <- fwdControl(
    # TARGET for frq years
    c(lapply(seq(ay + man_lag, ay + frq), function(x)
      list(quant=output, value=c(out), year=x)),
    # CHANGE limits
    lapply(seq(ay + man_lag, ay + frq), function(x)
      list(quant=output, min=rep(dlow, dim(out)[6]), max=rep(dupp, dim(out)[6]),
        year=x, relYear=x - 1)))
  )

	list(ctrl=ctrl, tracking=tracking)
}

# }}}

# plot_hockeystick.hcr {{{

#' @examples
#' data(ple4)
#' args <- list(lim=1e5, trigger=4e5, target=0.25, min=0,
#'   metric="ssb", output="fbar")
#' # Plot hockeystick.hcr for given arguments
#' plot_hockeystick.hcr(args)
#' # Add metric and output from FLStock
#' plot_hockeystick.hcr(args, obs=ple4)
#' # Superpose Kobe colours
#' plot_hockeystick.hcr(args, obs=ple4, kobe=TRUE)
#' # Set actual x (e.g. biomass) target.
#' plot_hockeystick.hcr(args, obs=ple4, kobe=TRUE, xtarget=args$trigger * 0.80)

plot_hockeystick.hcr <- function(args, obs="missing", kobe=FALSE,
  xtarget=args$trigger, alpha=0.3) {

  # EXTRACT args from mpCtrl
  if(is(args, "mpCtrl"))
    args <- args$args

  # SET args
  spread(args)
  xlim <- trigger * 1.50
  ylim <- target * 1.50

  # GET observations
  if(!missing(obs)) {
    obs <- model.frame(metrics(obs, list(met=get(metric), out=get(output))))
    xlim <- max(obs$met) * 1.05
    ylim <- max(obs$out) * 1.05
  }
 
  # SET met values
  met <- seq(0, xlim, length=200)

  # BELOW lim
  out <- ifelse(met <= lim, min,
    # BETWEEN lim and trigger
    ifelse(met < trigger,
      pmax(c(target * ((met - trigger) / (trigger - lim) + 1)),  min),
    # ABOVE trigger
    target))

 
  # DATA
  dat <- data.frame(met=met, out=out)
  
  p <- ggplot(dat, aes(x=met, y=out)) +
    coord_cartesian(ylim = c(0, ylim), clip="off") +
    xlab(toupper(metric)) + ylab(toupper(output)) +
    # TARGET
    geom_segment(aes(x=0, xend=trigger * 1.25, y=target, yend=target), linetype=2) +
    annotate("text", x=0, y=target + ylim / 30, label="target", hjust="left") +
    # MIN
    annotate("text", x=0, y=min + ylim / 30, label="min", hjust="left") +
    # TRIGGER
    geom_segment(aes(x=trigger, xend=trigger, y=0, yend=target), linetype=2) +
    annotate("text", x=trigger, y=-ylim / 40, label="trigger", vjust="bottom") +
    # LIMIT
    geom_segment(aes(x=lim, xend=lim, y=0, yend=min), linetype=2) +
    annotate("text", x=lim, y=-ylim / 40, label="limit", vjust="bottom") +
    # HCR line
    geom_line()

  if(!missing(obs)) {
    p <- p + geom_point(data=obs)
  }

  if(kobe) {
  
  # PREDICT for xtarget
  ytarget <- ifelse(xtarget < trigger,
    pmax(c(target * ((xtarget - trigger) / (trigger - lim) + 1)),  min),
    target)

  p <- p +
    # YELLOW
    geom_polygon(data=data.frame(x=c(args$lim, xtarget, xtarget, args$lim),
      y=c(args$min, args$min, ytarget, args$min)),
      aes(x=x, y=y), fill="yellow", alpha=alpha) +
    # GREEN
    geom_polygon(data=data.frame(
      x=c(xtarget, xlim, xlim, args$trigger, xtarget, xtarget),
      y=c(0, 0, rep(args$target, 2), ytarget, ytarget)),
      aes(x=x, y=y), fill="green", alpha=alpha) +
    # RED
    geom_polygon(data=data.frame(
      x=c(0, args$lim, args$trigger, xlim, xlim, 0, 0),
      y=c(args$min, args$min, args$target, args$target, ylim, ylim, args$min)),
      aes(x=x, y=y), fill="red", alpha=alpha)
  }

  return(p)
}

# }}}

# trend.hcr {{{

#' @param k1 Gain parameter
#' @param k2 Gain parameter
#' @param gamma Asymmetry parameter
#' @param nyears Number of years used in regression of log(stock).
#' @examples
#' data(ple4om)
#'  trend.hcr(stock(om), args=list(ay=2003, data_lag=1, management_lag=1, frq=1,
#'  it=1), tracking=FLQuant(), k1=1.5, k2=3, gamma=1, nyears=5, metric=ssb)

trend.hcr <- function(stk, ind, args, tracking, k1=1.5, k2=3, gamma=1, nyears=5,
  metric=stock) {

  # args
  spread(args)
  dy <- ac(ay - data_lag)

  # SELECT metric
  if(is(metric, "function")) {
    met <- do.call(metric, list(stk))
  } else if (is(metric, "character")) {
    if(metric %in% names(ind))
      met <- ind[[metric]]
    else
      met <- do.call(metric, list(stk))
  }

  # CREATE data.table
  dat <- data.table(as.data.frame(met[, ac(seq(ay - data_lag - (nyears - 1),
    length=nyears))]))
  
  # FIND iters with NAs
  nas <- dat[, .(nas=sum(is.na(data))), by=iter]
  lnas <- nas$nas == 0
  rnas <- nas[nas == 0, (iter)]

  # CALCULATE slope if not in nas
  slope <- rep(NA, it)
  slope[lnas] <- dat[iter %in% rnas, .(slope=coef(lm(log(data) ~ year))[2]),
    by=iter][, (slope)]

  # TAC TODO GET TAC from tracking['hcr',]
  tac <- catch(stk)[, dy]

  # FIND iters with negative slope
  id <- slope < 0

  # slope < 0
  tac[,,,,, id & lnas] <- catch(stk)[, dy,,,, id & lnas] *
    (1 - k1 * abs(slope[id & lnas]) ^ gamma) 

  # slope >= 0
  tac[,,,,, !id & lnas] <- catch(stk)[, dy,,,, !id & lnas] *
    (1 + k2 * slope[!id & lnas]) 

  # CONTROL
  ctrl <- fwdControl(
    # TAC for frq years
    lapply(seq(ay + management_lag, ay + frq), function(x)
    list(quant="catch", value=c(tac), year=x))
  )

  return(list(ctrl=ctrl, tracking=tracking))
}

# }}}

# target.hcr {{{

#' @param stk
#' @param lim Lower limit for value of metric.
#' @param target Desired value for metric.
#' @param metric Quantity computed from 'stk', defaults to ssb. Function or character.
#' @param output Quantity employed in forecast (fwd's 'quant'), defaults to 'fbar', character.
#' @param args
#' @param tracking
#' @examples
#' data(ple4)
#' args <- list(ay=2015, data_lag=1, management_lag=1, frq=1)
#' # Set as fbar ~ ssb
#' hockeystick.hcr(ple4, lim=3e5, trigger=4e5, target=0.25, min=0,
#'   metric="ssb", output="fbar", args=args, tracking=FLQuant())
#' # Use for catch ~ depletion, with metric as a new function
#' hockeystick.hcr(ple4, lim=0.10, trigger=0.40, target=140000, min=0,
#'   metric=function(x) ssb(x) %/% ssb(x)[,1],
#'   output="catch", dlow=0.85, dupp=1.15, args=args, tracking=FLQuant())

target.hcr <- function(ind, lim, target, r=1, metric="mlc", output="fbar",
  nyears=3, args, tracking) {

  # EXTRACT args
  ay <- args$ay
  data_lag <- args$data_lag
  man_lag <- args$management_lag
  frq <- args$frq

  # COMPUTE metric multiplier

  met <- yearMeans(window(ind[[metric]], start=ay - data_lag - nyears + 1,
    end=ay - data_lag))

  # RULE
    # BELOW lim, E_[y+1] = E_y / 2 * (I_recent / I_lim) ^ 2 * r
  out <- ifelse(met < lim, 1 / 2 * (met / lim) ^ 2 * r,
    # ABOVE lim, E_[y+1] = E_y * ((1 + (I_recent - I_lim) / (I_tar - I_lim)) / 2) ^ r
    ((1 + (met - lim) / (target - lim)) / 2) ^ r
    )

  # IF NA, set to previous value
  # if(any(is.na(out)))
  #   out[is.na(out)] <- tracking[[1]]['hcr', ac(ay - 1)][is.na(out)]

  # CONTROL
  ctrl <- fwdControl(
    # TARGET for frq years
    lapply(seq(ay + man_lag, ay + frq), function(x)
      list(quant=output, value=c(out), year=x, relYear=x-1)))

	list(ctrl=ctrl, tracking=tracking)
}
# }}}

# cpue.hcr {{{

#' cpue.hcr
#'
#' @examples
#' data(ple4om)
#' ind <- cpue.ind(stock(om), FLIndices(CPUE=FLIndexBiomass(index=ssb(om))),
#'   args=list(ay=2000, data_lag=1), tracking=FLQuant())
#' cpue.hcr(stk=stock(om), ind=ind$ind, k1=0.1, k2=0.2, k3=0.1, k4=0.1, args=list(ay=1990),
#'  tracking=FLQuants(FLQuant(c(0.5, 0.8), dimnames=list(metric=c("cpue.slope",
#'  "cpue.mean"), year=1990))))

cpue.hcr <- function(stk, ind, k1, k2, k3, k4, target=1,
  dtaclow=0.85, dtacupp=1.15, args, tracking){
  
  # args
  ay <- args$ay

  # RECOVER slope & mean(cpue)
  slope <- ind$slope
  mcpue <- ind$mean

  # CALCULATE new tac
  ka <- ifelse(slope > 0, k1, k2)
  kb <- ifelse(mcpue > target, k3, k4)

  # TAC_y-1 ~ TAC_y * 1 + ka * m + kb * (mcpue - target)
  # TODO TAC: catch(om) / catch(stk) / TAC(y-1)
  tac <- catch(stk)[, ac(ay-1)] * (1 + ka * slope + kb * (mcpue - target))

  ctrl <- fwdControl(list(quant="catch", value=tac, year=ay + 1),
    # TAC limits
    list(quant="catch", min=dtaclow, max=dtacupp, relYear=ay - 1, year=ay + 1))
  
	return(list(ctrl=ctrl, tracking=tracking))
} # }}}


# mix.hcr: COMBINE 2 hcr modules
#   mlc.hcr + cpue.hcr

# ---

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
  dtaclow=0.85, dtacupp=1.15, yrs=1, metric="ssb", args, tracking) {

  # TODO
  
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
  met <- do.call(metric, list(stk))
  dep <- yearMeans(unitSums(window(met, start=ay - data_lag - yrs,
    end=ay - data_lag))) / unitSums(met[, 1])

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


