# hcr.R - DESC
# mse/R/hcr.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


globalVariables(c("ay", "bufflow", "buffup", "data_lag", "dy", "frq", "fy", "lim",
  "management_lag", "min", "sloperatio"))

# hockeystick.hcr {{{

#' Hockey-stick Harvest Control Rule
#'
#' A hockey-stick harvest control rule that sets the value of an output between a 
#' minimum and a target, based on that of a metric, compared with a limit and a trigger.
#'
#' @details 
#' This function implements a hockey-stick shaped harvest control rule (HCR). It is
#' commonly used to set F or effort, and sometimes catch, from a measure of stock
#' status, such as estimated SSB or depletion. The function can take any input,
#' 'metric', if it can computed from an FLStock or is returned by the function run
#' in the 'est' step. The 'output' argument can be set to any quantity that the OM
#' projection method can understand, e.g. fbar, effort or catch for 'fwd.om'.
#' The HCR increases the control variable linearly between a lower threshold (`lim`)
#' and a trigger point (`trigger`), and sets it to a target level (`target`) when the 
#' metric is above the trigger. The decreasing line can be cut at any point ('drop'),
#' where output values fall to the 'min' level set. See examples below.
#' The function can also apply optional upper (`dupp`) and lower (`dlow`) constraints
#' to changes in the control variable. These constraints can be applied either
#' unconditionally (`all = TRUE`) or only on the stock being above the trigger.
#'
#' @param stk The OEM observation or SA estimation, an FLStock object.
#' @param ind Possible indicators returned by the 'est' step, FLQuants.
#' @param lim The lower threshold of the stock metric, below which the control rule
#' applies the minimum allowable value (`min`), numeric or FLQuant.
#' @param trigger The stock status or metric value threshold above which the control variable reaches the target value, numeric or FLQuant.
#' @param target The output level applied when the stock metric is at or above the `trigger` value, numeric or FLQuant.
#' @param min Numeric. The minimum allowable control value (e.g., minimum F or catch).
#' @param drop Numeric. A stock metric threshold below which the control variable is forced to `min` to prevent over-exploitation of severely depleted stocks, numeric  or FLQuant
#' @param metric The stock metric to use for the HCR (e.g., `"ssb"` for spawning-stock biomass), character or function.
#' @param output Character. The output control variable (e.g., `"fbar"` for fishing mortality or `"catch"` for quotas), character.
#' @param dlow A limit for the decrease in the output variable, e.g. 0.85 for a maximum decrease of 15%, numeric.
#' @param dupp A limit for the increase in the output variable, e.g. 1.15 for a maximum increase of 15%, numeric.
#' @param all If `TRUE`, upper and lower limits (`dupp` and `dlow`) are applied unconditionally, otherwise only when metric > trigger, logical.
#' @param args A list containing dimensionality arguments, passed on by mp().
#' @param tracking An FLQuant used for tracking indicators, intermediate values, and decisions during MP evaluation.
#'
#' @return A list containing elements 'ctrl', a fwdControl object, and 'tracking'.
#' @examples
# Load an FLStock as example input
#' data(ple4)
#' 
#' # Define arguments, done automatically when called by mp()
#' args <- list(ay = 2015, iy = 2015, dy=2014, management_lag = 1, mys=2016)
#' track <- FLQuant(dimnames=list(metric="fbar.hcr", year=2014:2016))
#' 
#' # Apply the control rule
#' ctrl <- hockeystick.hcr(stk = ple4, ind = FLQuants(), lim = 7e5, trigger = 1e6, 
#'   target = 0.24, metric = "ssb", output = "fbar", args = args, tracking = track)
#' 
#' # Inspect the control object
#' print(ctrl)
#' 
#' # Add change limits
#' ctrl2 <- hockeystick.hcr(stk = ple4, ind = FLQuants(), lim = 7e5, trigger = 1e6, 
#'   target = 0.24, dlow=0.85, dupp=1.15, metric = "ssb", output = "fbar",
#'   args = args, tracking = track)
#' 
#' # Inspect the control object
#' print(ctrl2)

hockeystick.hcr <- function(stk, ind, lim, trigger, target, min=0, drop=0,
  metric="ssb", output="fbar", dlow=NA, dupp=NA, all=TRUE, args, tracking) {

  # EXTRACT args
  spread(args[c('ay', 'iy', 'dy', 'mys', 'management_lag')])

  # COMPUTE metric
  met <- window(selectMetric(metric, stk, ind), start=dy, end=dy)

  # TRACK metric
  track(tracking, "met.hcr", dy) <- met
  
  # - APPLY rule

  # BELOW lim
  out <- c(ifelse(met <= lim, min,
    # BETWEEN lim and trigger
    ifelse(met < trigger,
      # diff(met - lim) * gradient + min
      (met - lim) * ((target - min) / (trigger - lim)) + min,
    # ABOVE trigger
    target)))

  # APPLY drop to min
  out[c(met < drop)] <- min

  # TRACK initial target
  track(tracking, paste0(output, ".hcr"), mys) <- out

  # TRACK decision: met <= lim, 1; lim < met < trigger, 2; met >= trigger, 3
  track(tracking, "decision.hcr", ay) <- ifelse(met < drop, 0,
    ifelse(met <= lim, 1, ifelse(met < trigger, 2, 3)))

  # GET TAC dy / ay - 1
  if(ay == iy) {
    pre <- areaSums(unitSums(seasonSums(window(do.call(output, list(stk)),
      start=ay - management_lag, end=ay - management_lag))))
  } else {
    pre <- c(tracking[[1]]["hcr", ac(ay)])
  }

  # IF NA, set to previous value
  if(any(is.na(out))) {
    out[is.na(out)] <- pre[is.na(out)]
  }

  # APPLY limits, always or if met < trigger
  if(!is.na(dupp)) {
    if(all) {
    out[out > pre * dupp] <- pre[out > pre * dupp] * dupp
    } else {
    out[out > pre * dupp & met < trigger] <- pre[out > pre * dupp & met <
      trigger] * dupp
    }
  }

  if(!is.na(dlow)) {
    if(all) {
    out[out < pre * dlow] <- pre[out < pre * dlow] * dlow
    } else {
    out[out < pre * dlow & met < trigger] <- pre[out < pre * dlow & met <
      trigger] * dlow
    }
  }

  # CONTROL
  ctrl <- fwdControl(
    # TARGET for mys years
    c(lapply(mys, function(x) list(quant=output, value=c(out), year=x)))
  )

  # SET fbar ages
  if(output %in% c("f", "fbar")) {
    ctrl$minAge <- range(stk, "minfbar")
    ctrl$maxAge <- range(stk, "maxfbar")
  }

	list(ctrl=ctrl, tracking=tracking)
}
# }}}

# plot_hockeystick.hcr {{{

#' @examples
#' data(ple4)
#' Set example HCR arguments
#' args <- list(lim=1e5, trigger=4e5, target=0.25, min=0,
#'   metric="ssb", output="fbar")
#' # Plot hockeystick.hcr for given arguments
#' plot_hockeystick.hcr(args)
#' # Plot wheh 'drop' has been set
#' args <- list(lim=0, trigger=4e5, target=0.25, min=0, drop=2e5,
#'   metric="ssb", output="fbar")
#' plot_hockeystick.hcr(args)
#' # Add metric and output from FLStock
#' plot_hockeystick.hcr(args, obs=ple4)
#' # Superpose Kobe colours
#' plot_hockeystick.hcr(args, kobe=TRUE)
#' data(ple4)
#' # Change labels
#' plot_hockeystick.hcr(args, obs=ple4, kobe=TRUE,
#'   labels=c(limit="Blim", trigger="Btrigger", target="Ftarget"))
#' # Set actual x (e.g. biomass) target.
#' plot_hockeystick.hcr(args, obs=ple4, kobe=TRUE, xtarget=args$trigger * 0.80)
#' #' Add line and label for Btarget
#' plot_hockeystick.hcr(args, obs=ple4, kobe=TRUE, xtarget=args$trigger * 0.80) +
#' geom_vline(xintercept=args$trigger * 0.80, linetype=3) +
#' geom_label(x=args$trigger * 0.80, y=0.7, label="SBtarget")
#' # ADD a time line and decade labels
#' plot_hockeystick.hcr(args, obs=ple4, kobe=TRUE) +
#'   geom_line(data=model.frame(metrics(ple4, list(met=ssb, out=fbar)))) +
#'   geom_label(data=model.frame(metrics(ple4[, ac(seq(1957,2017, by=10))],
#'   list(met=ssb, out=fbar))), aes(label=year),
#'   fill=c("white", rep("gray", 5), "orange"))
#' # Example on relative terms where trigger < xtarget
#' args <- list(lim=0., trigger=0.9, target=1, min=0,
#'   metric="ssb", output="fbar")  
#' plot_hockeystick.hcr(args, kobe=TRUE, xtarget=1) +
#' geom_vline(xintercept=1)
#' plot_hockeystick.hcr(args, obs=3e5)

plot_hockeystick.hcr <- function(args, obs="missing",
  kobe=FALSE, xtarget=args$trigger, alpha=0.3,
  labels=c(lim="limit", trigger="trigger", min="min", target="target")) {
  
  # EXTRACT args from mpCtrl
  if(is(args, "mseCtrl"))
    args <- args(args)
  else if(is(args, "mpCtrl"))
    args <- args(args$hcr)

  # ASSIGN args if missing
  if(!"min" %in% names(args))
    args$min <- 0
  if(!"drop" %in% names(args))
    args$drop <- 0
  if(!"metric" %in% names(args))
    metric <- "ssb"
  if(!"output" %in% names(args))
    output <- "fbar"

  # SET args
  spread(lapply(args, c))
  xlim <- trigger * 1.50
  ylim <- target * 1.50
  
  # SET met values
  met <- seq(0, xlim, length=200)

  # BELOW lim
  # TODO: APPLY over args sets, 'set'
  out <- ifelse(met <= lim, min,
    # BETWEEN lim and trigger
    ifelse(met < trigger,
      # diff(met - lim) * gradient + min
      (met - lim) * ((target - min) / (trigger - lim)) + min,
    # ABOVE trigger
    target)
  )

  # APPLY drop to min
  out[c(met < c(args$drop))] <- min

  # LABELS as list
  labels <- as.list(labels)
 
  # DATA
  # TODO: ADD 'set'
  dat <- data.frame(metric=met, output=out)
  
  # TODO: ADD aes(group='set')
  p <- ggplot(dat, aes(x=metric, y=output)) +
    coord_cartesian(ylim = c(0, ylim), clip="off") +
    # DROP xlab(toupper(metric)) + ylab(toupper(output)) +
    # TARGET
    annotate("segment", x=0, xend=trigger * 1.25, y=target, yend=target,
      linetype=2) +
    annotate("text", x=0, y=target + ylim / 30, label=labels$target, 
      hjust="left") +
    # MIN
    annotate("text", x=0, y=min + ylim / 30, label=labels$min, hjust="left") +
    # TRIGGER
    annotate("segment", x=trigger, xend=trigger, y=0, yend=target,
      linetype=2) +
    annotate("text", x=trigger, y=-ylim / 40, label=labels$trigger, 
      vjust="bottom") +
    # LIMIT
    annotate("segment", x=lim, xend=lim, y=0, yend=min, linetype=2) +
    annotate("text", x=lim, y=-ylim / 40, label=labels$lim, vjust="bottom") +
    # HCR line
    geom_line()

  # KOBE
  # TODO: ONLY if set = 1

  if(kobe) {
  
  # YELLOW inflection point
  if(xtarget <= trigger) {
    yinf <- ifelse(xtarget < trigger,
      pmax(c(target * ((xtarget - trigger) / (trigger - lim) + 1)),  min),
      target)
    yell <- geom_polygon(data=data.frame(
      x=c(args$lim, xtarget, xtarget, args$lim),
      y=c(args$min, args$min, yinf, args$min)),
      aes(x=x, y=y), fill="yellow", alpha=alpha)
  } else {
    yinf <- target

    yell <- geom_polygon(data=data.frame(
      x=c(args$lim, xtarget, xtarget, trigger, args$lim),
      y=c(args$min, args$min, yinf, yinf, args$min)),
      aes(x=x, y=y), fill="yellow", alpha=alpha)
  }
  
    # YELLOW
    p <- p + yell +
    # GREEN
    geom_polygon(data=data.frame(
      x=c(xtarget, xlim, xlim, args$trigger, xtarget, xtarget),
      y=c(0, 0, rep(args$target, 2), yinf, yinf)),
      aes(x=x, y=y), fill="green", alpha=alpha) +
    # RED
    geom_polygon(data=data.frame(
      x=c(0, args$lim, args$trigger, xlim, xlim, 0, 0),
      y=c(args$min, args$min, args$target, args$target, ylim, ylim, args$min)),
      aes(x=x, y=y), fill="red", alpha=alpha)
  }

  # OBS
  if(!missing(obs)) {
    # FLStock
    if(is.FLStock(obs)) {
      obs <- model.frame(metrics(obs, list(metric=get(metric), output=get(output))))
      xlim <- max(obs$met, na.rm=TRUE) * 1.05
      ylim <- max(obs$out, na.rm=TRUE) * 1.05

      # PLOT line if 1 iter
      if(length(unique(obs$iter)) == 1)
        p <- p + geom_point(data=obs, alpha=alpha) +
          geom_path(data=obs, alpha=alpha) +
          geom_label(data=subset(obs, year %in% c(min(year), max(year))),
            aes(label=year), fill=c('gray', 'white'), alpha=1)
      # PLOT with alpha if multiple
      else
        p <- p + geom_point(data=obs, alpha=alpha)
    }
    # NUMERIC
    else if(is.numeric(obs)) {
      obs <- data.frame(met=obs, out=out[which.min(abs(met - obs))])
      p <- p + geom_point(data=obs, colour="red", size=3)
    }

  }
  return(p)
}

# }}}

# fixedC.hcr {{{

#' A fixed target C
#'
#' No matter what get C = Ctrg
#' The control argument is a list of parameters used by the HCR.
#' @param stk The perceived FLStock.
#' @param control A list with the element ftrg (numeric).
#' @examples
#' data(sol274)
#' fixedC.hcr(stock(om), ctrg=50000, args=list(ay=2017, management_lag=1,
#'   frq=1, it=100), tracking=FLQuant())

fixedC.hcr <- function(stk, ctrg, args, tracking){
  
  # args
	ay <- args$ay
  mlag <- args$management_lag
  frq <- args$frq

  # PARSE FLQuant
  if(is(ctrg, 'FLQuant')) {
    # EXPAND iters
    ctrg <- propagate(ctrg, args$it)
    # TODO: EXPAND years, if needed
    ctrg <- c(ctrg[, ac(seq(ay + mlag, ay + frq))])
  } else {
    # REPLICATE
    ctrg <- rep(ctrg, args$it)[seq(args$it)]
  }

	# create control object
  ctrl <- fwdControl(year=seq(ay + mlag, ay + frq), quant="catch", value=c(ctrg))

	# return
	list(ctrl=ctrl, tracking=tracking)

} # }}}

# fixedF.hcr {{{

#' A fixed target F
#'
#' No matter what get F = ftrg
#' The control argument is a list of parameters used by the HCR.
#' @param stk The perceived FLStock.
#' @param control A list with the element ftrg (numeric).
#' @examples
#' data(sol274)
#' fixedF.hcr(stock(om), ftrg=0.13, args=list(ay=2017, management_lag=1,
#'   frq=1), tracking=FLQuant())

fixedF.hcr <- function(stk, ftrg, args, tracking){
  
  # args
	ay <- args$ay
  mlag <- args$management_lag
  frq <- args$frq

  # PARSE FLQuant
  if(is(ftrg, 'FLQuant')) {
    # EXPAND iters
    ftrg <- propagate(ftrg, args$it)
    # TODO: EXPAND years, if needed
    ftrg <- c(ftrg[, ac(seq(ay + mlag, ay + frq))])
  }

	# create control object
  ctrl <- fwdControl(year=seq(ay + mlag, ay + frq), quant="fbar", value=c(ftrg))

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
  ctrl <- as(FLQuants(fbar=hcrpars), 'fwdControl')
	
  # return
	list(ctrl=ctrl, tracking=tracking)

} # }}}

# buffer.hcr {{{

buffer.hcr <- function(stk, ind, metric='wmean',
  target=1, width=0.5, lim=max(target * 0.10, target - 2 * width), 
  bufflow=max(lim * 1.50, target - width), buffupp=target + width,
  sloperatio=0.15, scale=lastcatch,
  initac=NULL, dupp=NULL, dlow=NULL, all=TRUE, ..., args, tracking) {

  # EXTRACT args
  ay <- args$ay
  iy <- args$iy
  data_lag <- args$data_lag
  man_lag <- args$management_lag
  frq <- args$frq

  # SET data year
  dy <- ay - data_lag
  # SET control years
  cys <- seq(ay + man_lag, ay + man_lag + frq - 1)

  # COMPUE and window metric
  met <- mse::selectMetric(metric, stk, ind)
  met <- window(met, start=dy, end=dy)

  # COMPUTE gradient of decrease
  dgradient <- (1 - 2^(-1)) / (bufflow - lim)
  
  # COMPUTE HCR multiplier if ...
  # BELOW lim
  hcrm <- ifelse(met <= lim, ((met / lim) ^ 2) / 2,
    # BETWEEN lim and bufflow
    ifelse(met <= bufflow,
      (0.5 * (1 + (met - lim) / (bufflow - lim))),
    # BETWEEN bufflow and buffupp
    ifelse(met < buffupp, 1, 
    # ABOVE buffupp, as proportion of dgradient
      1 + sloperatio * dgradient * (met - buffupp))))

  # TRACK decision
  dec <- cut(met, c(0, lim, bufflow, buffupp, Inf), labels=seq(1,4))
  
  track(tracking, "decision.hcr", ay) <- as.numeric(dec)

  # GET initial TAC,
  if(!is.null(initac) & ay == iy) {
    lastcatch <- FLQuant(initac, iter=args$it)
  # previous TAC, or
  } else {
    # DEBUG: tracking has copies by season, TURN annual?
    lastcatch <- tracking[[1]]['hcr', ac(ay),,1]
    # previous catch
    if(all(is.na(lastcatch)))
      lastcatch <- unitSums(areaSums(seasonSums(catch(stk)[, ac(ay - args$data_lag)])))
  }

  # SET TAC
  out <- scale * hcrm

  # TRACK first decision
  track(tracking, "rule.hcr", cys) <- out

  # APPLY limits, always or if met < trigger
  if(!is.null(dupp)) {
    if(all) {
      out[out > lastcatch * dupp] <-
        lastcatch[out > lastcatch * dupp] * dupp
    } else {
      out[out > lastcatch * dupp & met > bufflow] <-
        lastcatch[out > lastcatch * dupp & met > bufflow] * dupp
    }
  }

  if(!is.null(dlow)) {
    if(all) {
      out[out < lastcatch * dlow] <-
        lastcatch[out < lastcatch * dlow] * dlow
    } else {
      out[out < lastcatch * dlow & met > bufflow] <-
        lastcatch[out < lastcatch * dlow & met > bufflow] * dlow
    }
  }

  # TRACK TAC limited
  # sum(old > out)
  # sum(old < out)

  # CONTROL
  ctrl <- fwdControl(
    # TARGET for frq years
    c(lapply(cys, function(x) list(quant="catch", value=c(out), year=x))))
	
  list(ctrl=ctrl, tracking=tracking)
}
# }}}

# plot_buffer.hcr {{{

plot_buffer.hcr <- function(args, obs="missing", alpha=0.3,
  labels=c(lim="limit", bufflow="Lower~buffer", buffupp="Upper~buffer",
    metric=metric, output=output), metric=args$metric, output='multiplier',
    xlim=buffupp * 1.50, ylim=scale * 1.50) {

  # EXTRACT args from mpCtrl
  if(is(args, "mseCtrl"))
    args <- args(args)

  # GET args
  spread(lapply(args, c), FORCE=TRUE)

  # PARSE labels
  alllabels <- formals()$labels
  alllabels[names(labels)] <- labels
  labels <- as.list(alllabels)

  # SET met values
  met <- seq(0, xlim, length=200)

  # COMPUTE gradient of decrease
  dgradient <- (1 - 2^(-1))/(bufflow - lim)

  # COMPUTE HCR multiplier if ...

  # BELOW lim
  out <- ifelse(met <= lim, ((met / lim) ^ 2) / 2,
    # BETWEEN lim and bufflow
    ifelse(met <= bufflow,
      (0.5 * (1 + (met - lim) / (bufflow - lim))),
    # BETWEEN bufflow and buffupp
    ifelse(met < buffupp, 1, 
    # ABOVE buffupp, as proportion of dgradient
      1 + sloperatio * dgradient * (met - buffupp))))

  # DATA
  # TODO: ADD 'set'
  dat <- data.frame(metric=met, output=out)
  
  # TODO:
  scale <- 1
  
  # TODO: ADD aes(group='set')
  p <- ggplot(dat, aes(x=metric, y=output)) +
    coord_cartesian(ylim = c(0, ylim), clip="off") +
    # HCR line
    geom_line() +
    # TODO: TARGET & WIDTH
    # BUFFER UPP
    annotate("segment", x=buffupp, xend=buffupp, y=0, yend=scale, linetype=2) +
    annotate("point", x=buffupp, y=scale, size=3) +
    annotate("text", x=buffupp, y=-ylim / 40, label=labels$buffupp,
      vjust="bottom", parse=TRUE) +
    # BUFFER LOW
    annotate("segment", x=bufflow, xend=bufflow, y=0, yend=scale, linetype=2) +
    annotate("point", x=bufflow, y=scale, size=3) +
    annotate("text", x=bufflow, y=-ylim / 40, label=labels$bufflow,
      vjust="bottom", parse=TRUE) +
    # LIMIT
    annotate("segment", x=lim, xend=lim, y=0, yend=out[which.min(abs(met - lim))], 
      linetype=2) +
    annotate("point", x=lim, y=out[which.min(abs(met - lim))], size=3) +
    annotate("text", x=lim, y=-ylim / 40, label=labels$lim, vjust="bottom",
      parse=TRUE) +
    # SLOPE
    annotate("segment", x=buffupp, xend=xlim, y=1, yend=1, linetype=2) +
    annotate("text", x=buffupp + (xlim - buffupp) / 3, y=1, label="slope", 
      vjust="bottom", parse=TRUE)

  # AXIS labels
  if(!is.null(labels$metric))
    p <- p + xlab(parse(text=labels$metric))
  if(!is.null(labels$output))
    p <- p + ylab(parse(text=labels$output))

  # OBS
  if(!missing(obs)) {
    # FLStock
    if(is.FLStock(obs)) {
      obs <- model.frame(metrics(obs, list(metric=get(metric), output=get(output))))
      xlim <- max(obs$metric, na.rm=TRUE) * 1.05
      ylim <- max(obs$output, na.rm=TRUE) * 1.05

      # PLOT line if 1 iter
      if(length(unique(obs$iter)) == 1)
        p <- p + geom_point(data=obs, alpha=alpha) +
          geom_path(data=obs, alpha=alpha) +
          geom_label(data=subset(obs, year %in% c(min(year), max(year))),
            aes(label=year), fill=c('gray', 'white'), alpha=1)
      # PLOT with alpha if multiple
      else
        p <- p + geom_point(data=obs, alpha=alpha)
    }
    # NUMERIC
    else if(is.numeric(obs)) {
      obs <- data.frame(metric=obs, output=output[which.min(abs(metric - obs))])
      p <- p + geom_point(data=obs, colour="red", size=3)
    }

  }
  return(p)
}

# args <- list(lim=0.4, bufflow=1, buffupp=2,
#   sloperatio=0.2)

# plot_buffer.hcr(args, labels=list(metric='CPUE', output='C~mult'))

# }}}

# depletion.hcr {{{

depletion.hcr <- function(stk, ind, metric='ssb', mult=1, hrmsy, K,
  trigger=0.4, lim=0.1, min=0.00001, dupp=NULL, dlow=NULL, all=TRUE,
  ..., args, tracking) {

  # EXTRACT args
  ay <- args$ay
  iy <- args$iy
  data_lag <- args$data_lag
  man_lag <- args$management_lag
  frq <- args$frq

  # SET data year
  dy <- ay - data_lag
  # SET control years
  cys <- seq(ay + man_lag, ay + man_lag + frq - 1)

  # COMPUTE and window metric
  met <- mse::selectMetric(metric, stk, ind)
  met <- window(met, start=dy, end=dy)

  # COMPUTE HCR multiplier if ...
  hcrm <- ifelse((met / K) >= trigger, 1,
    # metric betwween lim and trigger, and above
    ifelse((met / K) >= lim,      
      ((met / K) - lim) / (trigger - lim), min))

  # TRACK decision
  dec <- cut((met / K), c(0, lim, trigger, Inf), labels=seq(1, 3))
  # track(tracking, "decision.hcr", ay) <- as.numeric(dec)

  # SET HR target
  hrtarget <- hrmsy * hcrm * mult

  # SET TAC
  out <- met * hrtarget

  # TRACK first decision
  track(tracking, "rule.hcr", cys) <- out

  # TODO: ADD initac and TAC from tracking
  lastcatch <- unitSums(seasonSums(window(catch(stk), start=dy, end=dy)))

  # APPLY limits, always or if met < trigger
  if(!is.null(dupp)) {
    if(all) {
      out[out > lastcatch * dupp] <-
        lastcatch[out > lastcatch * dupp] * dupp
    } else {
      out[out > lastcatch * dupp & met < trigger] <-
        lastcatch[out > lastcatch * dupp & met < trigger] * dupp
    }
  }

  if(!is.null(dlow)) {
    if(all) {
      out[out < lastcatch * dlow] <-
        lastcatch[out < lastcatch * dlow] * dlow
    } else {
      out[out < lastcatch * dlow & met < trigger] <-
        lastcatch[out < lastcatch * dlow & met < trigger] * dlow
    }
  }

  # TRACK TAC limited
  # sum(old > out)
  # sum(old < out)

  # CONTROL
  ctrl <- fwdControl(
    # TARGET for frq years
    c(lapply(cys, function(x) list(quant="catch", value=c(out), year=x))))
	
  list(ctrl=ctrl, tracking=tracking)
}
# }}}
