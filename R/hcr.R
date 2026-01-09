# hcr.R - DESC
# mse/R/hcr.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# hockeystick.hcr {{{

#' Hockey-stick Harvest Control Rule
#'
#' A hockey-stick harvest control rule that sets the value of an output between a 
#' minimum and a target, according to that of a metric, compared with a limit and
#' a trigger.
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
#' @param stk The 'oem' observation or SA estimation, an FLStock object.
#' @param ind Possible indicators returned by the 'est' step, FLQuants.
#' @param lim The lower threshold of the stock metric, below which the control rule
#' applies the minimum allowable value (`min`), numeric or FLQuant.
#' @param trigger The stock status or metric value threshold above which the control variable reaches the target value, numeric or FLQuant.
#' @param target The output level applied when the stock metric is at or above the `trigger` value, numeric or FLQuant.
#' @param min Numeric. The minimum allowable control value (e.g., minimum F or catch).
#' @param drop Numeric. A stock metric threshold below which the control variable is forced to `min` to prevent over-exploitation of severely depleted stocks, numeric  or FLQuant
#' @param metric The stock metric to use for the HCR (e.g., `"ssb"` for spawning-stock biomass), character or function.
#' @param output Character. The output control variable (e.g., `"fbar"` for fishing mortality or `"catch"` for quotas), character.
#' @param dlow A limit for the decrease in the output variable, e.g. 0.85 for a maximum decrease of 15%, numeric. No limit is applied if NULL.
#' @param dupp A limit for the increase in the output variable, e.g. 1.15 for a maximum increase of 15%, numeric. No limit is applied if NULL.
#' @param all If `TRUE`, upper and lower limits (`dupp` and `dlow`) are applied unconditionally, otherwise only when metric > trigger, logical.
#' @param initial. Initial value of 'output' to use when applying 'dlow' and 'dupp' limits, numeric of FLQuant.
#' @param args A list containing dimensionality arguments, passed on by mp().
#' @param tracking An FLQuant used for tracking indicators, intermediate values, and decisions during MP evaluation.
#'
#' @return A list containing elements 'ctrl', a fwdControl object, and 'tracking'.
#' Three elements in *tracking* report on the steps inside *hockeystick.hcr*:
#' - decision.hcr
#' @examples
#' # Example dataset
#' data(sol274)
#' 
#' # Sets up an mpCtrl using hockeystick(fbar~ssb)
#' ctrl <- mpCtrl(est = mseCtrl(method=perfect.sa),
#'   hcr = mseCtrl(method=hockeystick.hcr, args=list(metric="ssb", trigger=45000, 
#'     output="fbar", target=0.27)))
#' 
#' plot_hockeystick.hcr(ctrl)
#' 
#' # Sets up a 40:10 HCR mpCtrl using hockeystick(fbar~depletion)
#' ctrl <- mpCtrl(est = mseCtrl(method=perfect.sa),
#'   hcr = mseCtrl(method=hockeystick.hcr, args=list(
#'     metric="depletion", trigger=0.40, lim=0.10,
#'     output="fbar", target=0.27, min=0.02)))
#' 
#' plot_hockeystick.hcr(ctrl)
#' 
#' # Runs mp between 2021 and 2035
#' run <- mp(om, control=ctrl, args=list(iy=2021, fy=2035))
#' 
#' # Plots results
#' plot(om, run)

hockeystick.hcr <- function(stk, ind, target, trigger, lim=0, min=0, drop=0,
  metric="ssb", output="fbar", dlow=NULL, dupp=NULL, all=TRUE, initial=NULL,
  args, tracking, ...) {

  # EXTRACT args
  spread(args[c('ay', 'iy', 'dy', 'mys', 'management_lag', 'it')])

  # COMPUTE metric
  met <- window(selectMetric(metric, stk, ind, ...), start=dy, end=dy)

  # TRACK metric
  track(tracking, "metric.hcr", ay) <- met
  
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

  # TRACK initial output
  track(tracking, "decision.hcr", ay) <- out

  # TRACK rule: met <= lim, 1; lim < met < trigger, 2; met >= trigger, 3
  track(tracking, "rule.hcr", ay) <- ifelse(met < drop, 0,
    ifelse(met <= lim, 1, ifelse(met < trigger, 2, 3)))

  # GET previous output value if change limited
  if(!is.null(dupp) | !is.null(dlow)) {
    # GET initial value at start if set,
    if(ay == iy) {
      # STOP if initial is NULL
      if(is.null(initial))
        stop("To apply 'dlow' and 'dupp' limits, 'initial' is required")
      pre <- FLQuant(initial, iter=args$it)
    # OR previous decision,
    } else {
      pre <- tracking[metric == 'hcr' & year == ay - 1, data]
    }
  }
  
  # APPLY limits, always or if met < trigger
  if(!is.null(dupp)) {
    if(all) {
    out[out > pre * dupp] <- pre[out > pre * dupp] * dupp
    } else {
    out[out > pre * dupp & met < trigger] <- pre[out > pre * dupp & met <
      trigger] * dupp
    }
  }

  if(!is.null(dlow)) {
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

#' @rdname hochestick.hcr
#' @details
#'
#' @params
#' @examples
#' data(ple4)
#' # Set example HCR arguments
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
#' # ADD decade labels
#' plot_hockeystick.hcr(args, obs=ple4, kobe=TRUE) +
#'   geom_label(data=model.frame(metrics(ple4[, ac(seq(1957,2017, by=10))],
#'   list(metric=ssb, output=fbar))), aes(label=year),
#'   fill=c("white", rep("gray", 5), "orange"))
#' # Example on relative terms where trigger < xtarget
#' args <- list(lim=0., trigger=0.9, target=1, min=0,
#'   metric="ssb", output="fbar")  
#' plot_hockeystick.hcr(args, kobe=TRUE, xtarget=1) +
#' geom_vline(xintercept=1)

plot_hockeystick.hcr <- function(args, obs=NULL,
  kobe=FALSE, xtarget=args$trigger, alpha=0.3,
  labels=c(lim="limit", trigger="trigger", min="min", target="target", drop="drop")) {
  
  # EXTRACT args from mpCtrl
  if(is(args, "mseCtrl"))
    args <- args(args)
  else if(is(args, "mpCtrl"))
    args <- args(args$hcr)

  # ASSIGN args if missing
  if(!"lim" %in% names(args))
    args$lim <- 0
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

  # GET plot limits
  xlim <- max(trigger) * 1.50
  ylim <- max(target) * 1.50

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
    # TARGET
    annotate("segment", x=0, xend=trigger * 1.25, y=target, yend=target,
      linetype=2) +
    annotate("label", x=0, y=target + ylim / 40, label=labels$target, 
      hjust="left", vjust="bottom", parse=TRUE, fill=flpalette[2], alpha=0.5) +
    # MIN
    annotate("label", x=0, y=min + ylim / 40, label=labels$min, hjust="left", 
      vjust="bottom", parse=TRUE, fill=flpalette[2], alpha=0.5) +
    # LIMIT
    annotate("segment", x=lim, xend=lim, y=min + ylim / 10, yend=min, linetype=2) +
    annotate("label", x=lim, y=min + ylim / 10, label=labels$lim, vjust="bottom", 
      parse=TRUE, fill=flpalette[2], alpha=0.5) +
    # TRIGGER
    annotate("segment", x=trigger, xend=trigger, y=0, yend=target,
      linetype=2) +
    annotate("label", x=trigger, y=min + ylim / 10, label=labels$trigger, 
      vjust="bottom", parse=TRUE, fill=flpalette[2], alpha=0.5) +
    # HCR line
    geom_line(size=1)

  # ADD drop
  if(!is.null(args$drop) & args$drop != 0) {

    ydrop <- dat$output[which.min(abs(dat$metric - drop)) + 1]

    p <- p + annotate("label", x=drop, y=ydrop + ylim / 30, label=labels$drop, 
      vjust="bottom", parse=TRUE, fill=flpalette[2], alpha=0.5)
  }

  # KOBE
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
  if(!is.null(obs)) {
    # FLStock
    if(is.FLStock(obs)) {
      obs <- model.frame(metrics(obs, list(metric=get(metric), output=get(output))))

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
    else if(is(obs, 'FLQuants')) {
      obs <- data.frame(metric=obs$metric, output=obs$output)
      p <- p + geom_point(data=obs, colour="red", size=3)
    } else if(is(obs, 'FLQuant')) {
      obs <- data.frame(metric=obs, output=0)
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
#' # Example dataset
#' data(sol274)
#' 
#' # Sets up an mpCtrl for catch ~ MSY
#' ctrl <- mpCtrl(est = mseCtrl(method=perfect.sa),
#'   hcr = mseCtrl(method=fixedC.hcr, args=list(ctrg=11400)))
#' 
#' # Runs mp between 2021 and 2035
#' run <- mp(om, control=ctrl, args=list(iy=2021, fy=2035))

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
    # REPLICATE for iters and years
    ctrg <- rep(rep(ctrg, args$it)[seq(args$it)], length(args$mys))
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
    # REPLICATE for iters and years
    ftrg <- rep(rep(ftrg, args$it)[seq(args$it)], length(args$mys))
  }

	# create control object
  ctrl <- fwdControl(year=seq(ay + mlag, ay + frq), quant="fbar", value=c(ftrg))

	# return
	list(ctrl=ctrl, tracking=tracking)

} # }}}
