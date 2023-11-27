# hcr.R - DESC
# mse/R/hcr.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# -- ABSOLUTE

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
#' data(ple4)
#' # Test for year when SSB > bsafe
#' ices.hcr(ple4, fmin=0.05, ftrg=0.15, sblim=200000, sbsafe=300000,
#'   args=list(ay=2018, data_lag=1, management_lag=1), tracking=FLQuant())
#' # Test for year when SSB < bsafe
#' ices.hcr(ple4, fmin=0.05, ftrg=0.15, sblim=200000, sbsafe=300000,
#'   args=list(ay=1995, data_lag=1, management_lag=1), tracking=FLQuant())
#' #
#' hockeystick.hcr(ple4, ind=FLQuant(), lim=2e5, trigger=3e5, target=0.15,
#'   min=0.05, metric="ssb", output="fbar",
#'   args=list(ay=2018, data_lag=1, management_lag=1, frq=1),
#'   tracking=FLQuant(dimnames=list(metric="fbar.hcr", year=2017:2019)))

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
#' hockeystick.hcr(ple4, ind=FLQuant(), lim=3e5, trigger=4e5, target=0.25,
#'   min=0, metric="ssb", output="fbar", args=args,
#'   tracking=FLQuant(dimnames=list(metric="fbar.hcr", year=2014:2016)))
#' # Use for catch ~ depletion, with metric as a new function
#' hockeystick.hcr(ple4, ind=FLQuant(), lim=0.10, trigger=0.40, target=140000,
#'   min=0, metric=function(x) ssb(x) %/% ssb(x)[,1],
#'   output="catch", dlow=0.85, dupp=1.15, args=args,
#'   tracking=FLQuant(dimnames=list(metric="catch.hcr", year=2014:2016)))

hockeystick.hcr <- function(stk, ind, lim, trigger, target, min=0, drop=0,
  metric="ssb", output="fbar", dlow=NA, dupp=NA, all=TRUE, args, tracking) {
  
  # EXTRACT args
  ay <- args$ay
  data_lag <- args$data_lag
  man_lag <- args$management_lag
  frq <- args$frq

  # SET data year
  dy <- ay - data_lag
  # SET control years
  cys <- seq(ay + man_lag, ay + man_lag + frq - 1)

  # COMPUTE metric
  met <- window(selectMetric(metric, stk, ind), start=dy, end=dy)

  # TRACK metric
  track(tracking, "met.hcr", dy) <- c(met)
  
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

  # TRACK decision
  # - 1 if met <= lim
  # - 2 if lim < met < trigger
  # - 3 if met >= trigger 
  track(tracking, "decision.hcr", ay) <- ifelse(met < drop, 0,
    ifelse(met <= lim, 1, ifelse(met < trigger, 2, 3)))

  # LIMITS over previous output
  pre <- unitSums(seasonSums(window(do.call(output, list(stk)),
    start=ay - man_lag, end=ay - man_lag)))
  
  # IF NA, set to previous value
  if(any(is.na(out))) {
    out[is.na(out)] <- pre[is.na(out)]
    out[is.na(out)] <- 1
    pre[is.na(pre)] <- 1
  }

  # TRACK initial target
  track(tracking, paste0(output, ".hcr"), cys) <- c(out)

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
    # TARGET for frq years
    c(lapply(cys, function(x) list(quant=output, value=c(out), year=x)))
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

  # ASSIGN min if missing
  if(!"min" %in% names(args))
    args$min <- 0
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
  out[c(met < drop)] <- min

  # LABELS as list
  labels <- as.list(labels)
 
  # DATA
  # TODO: ADD 'set'
  dat <- data.frame(met=met, out=out)
  
  # TODO: ADD aes(group='set')
  p <- ggplot(dat, aes(x=met, y=out)) +
    coord_cartesian(ylim = c(0, ylim), clip="off") +
    # DROP xlab(toupper(metric)) + ylab(toupper(output)) +
    # TARGET
    geom_segment(aes(x=0, xend=trigger * 1.25, y=target, yend=target), 
      linetype=2) +
    annotate("text", x=0, y=target + ylim / 30, label=labels$target, 
      hjust="left") +
    # MIN
    annotate("text", x=0, y=min + ylim / 30, label=labels$min, hjust="left") +
    # TRIGGER
    geom_segment(aes(x=trigger, xend=trigger, y=0, yend=target), linetype=2) +
    annotate("text", x=trigger, y=-ylim / 40, label=labels$trigger, 
      vjust="bottom") +
    # LIMIT
    geom_segment(aes(x=lim, xend=lim, y=0, yend=min), linetype=2) +
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
      obs <- model.frame(metrics(obs, list(met=get(metric), out=get(output))))
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

# TODO: fixed.hcr

# fixedF.hcr {{{

#' A fixed target f
#'
#' No matter what get F = Ftarget
#' The control argument is a list of parameters used by the HCR.
#' @param stk The perceived FLStock.
#' @param control A list with the element ftrg (numeric).
#' @examples
#' data(sol274)
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
#' data(sol274)
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

# -- RELATIVE

# trend.hcr {{{

# T_{y+1} = T_{y} * 1 - k1 * |lambda| ^ gamma, lambda < 0
#                   1 + k2 * lambda, lambda >= 0

#' @param k1 Gain parameter
#' @param k2 Gain parameter
#' @param gamma Asymmetry parameter
#' @param nyears Number of years used in regression of log(stock).
#' @examples
#' data(sol274)
#' trend.hcr(stock(om), ind=FLQuants(), args=list(ay=2003, data_lag=1,
#'   management_lag=1, frq=1, it=1),
#'   tracking=FLQuant(dimnames=list(metric='tac.hcr', year=2003)),
#'   k1=1.5, k2=3, gamma=1, nyears=5, metric=ssb)

trend.hcr <- function(stk, ind, k1=1.5, k2=3, gamma=1, nyears=5, metric=ssb,
  dlow=NA, dupp=NA, initac=seasonSums(unitSums(catch(stk)[, dy])),
  args, tracking) {

  # EXTRACT args
  spread(args)
  dy <- ac(ay - data_lag)

  # SELECT metric
  met <- selectMetric(metric, stk, ind)

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
  
  # GET previous TAC from tracking['hcr',]
  pre <- unitSums(seasonSums(window(do.call("catch", list(stk)),
    start=ay - data_lag, end=ay - data_lag)))

  # pre <- tracking[[1]]['tac.hcr', dy]

  # OR from previous catch
  if(all(is.na(pre)) & ay == iy)
    pre <- pre %=% c(initac)

  # FIND iters with negative slope
  id <- slope < 0

  # CREATE tac object
  tac <- pre

  # slope < 0
  tac[,,,,, id & lnas] <- tac[, dy,,,, id & lnas] *
    (1 - k1 * abs(slope[id & lnas]) ^ gamma) 

  # slope >= 0
  tac[,,,,, !id & lnas] <- tac[, dy,,,, !id & lnas] *
    (1 + k2 * slope[!id & lnas]) 

  # TRACK initial TAC
  track(tracking, "tac.hcr", seq(ay + management_lag, ay + frq - 1)) <- tac

  # LIMITS over previous output
  if(!is.na(dupp))
    tac[tac > pre * dupp] <- pre[tac > pre * dupp] * dupp
  if(!is.na(dlow))
    tac[tac < pre * dlow] <- pre[tac < pre * dlow] * dlow

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

#' Target-based harvest control rule to adjust input or output from CPUE
#'
#' Short description
#' 
#' \deqn{}{}
#' 
#' @references Hoshino, E., Hillary, R., Davies, C., Satria, F., Sadiyah, L., Ernawati, T., and Proctor, C. 2020.  Development of pilot empirical harvest strategies for tropical tuna in indonesian archipelagic waters: case studies of skipjack and yellowfin tuna. *Fisheries Research*, 227:105539, doi:10.1016/j.fishres.2020.105539.
#' @param ind
#' @param lim
#' @param target
#' @param r
#' @param metric
#' @param output
#' @param nyears
#' @param args
#' @param tracking
#' @return A list containing *ctrl*, a `fwdControl` object, and *tracking*, an `FLQuant`.
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords utilities
#' @examples
#' data(sol274)
#' #
#' est <- cpue.ind(stock(om), FLIndices(CPUE=FLIndexBiomass(index=ssb(om))),
#'   args=list(ay=2000, data_lag=1),
#'   tracking=FLQuant(dimnames=list(metric="ind", year=2000, iter=1:100)))
#' #
#' target.hcr(ind=est$ind, lim=28000, target=40000,
#'   metric="wmean", output="catch",
#'   args=list(ay=2000, frq=1, data_lag=1, management_lag=1),
#'   tracking=FLQuants(sol174=FLQuant(1000, dimnames=list(metric="hcr",
#'   year=2000))))

target.hcr <- function(ind, lim, target, r=1, metric="mlc", output="catch",
  nyears=3, args, tracking) {

  # EXTRACT args
  ay <- args$ay
  data_lag <- args$data_lag
  man_lag <- args$management_lag
  frq <- args$frq

  # COMPUTE metric
  met <- yearMeans(window(ind[[metric]], start=ay - data_lag - nyears + 1,
    end=ay - data_lag))

  # APPLY rule
    # BELOW lim, E_[y+1] = E_y / 2 * (I_recent / I_lim) ^ 2 * r
  out <- ifelse(met < lim, 1 / 2 * (met / lim) ^ (2 * r),
    # ABOVE lim, E_[y+1] = E_y * ((1 + (I_recent - I_lim) / (I_tar - I_lim)) / 2) ^ r
    ((1 + (met - lim) / (target - lim)) / 2) ^ r)

  # IF NA, set to previous value
  # if(any(is.na(out)))
  #   out[is.na(out)] <- tracking[[1]]['hcr', ac(ay - 1)][is.na(out)]

  ctrl <- fwdControl(lapply(seq(ay + man_lag, ay + frq), function(x) {
    # target and year
    c(list(quant=output, value=c(out), year=x, relYear=x - 1),
      # FCB
      setFCB(output, relative=TRUE))
    }
  ))

  list(ctrl=ctrl, tracking=tracking)
}
# }}}

# cpue.hcr {{{

#' cpue.hcr
#'
#' @examples
#' data(sol274)
#' ind <- cpue.ind(stock(om), FLIndices(CPUE=FLIndexBiomass(index=ssb(om))),
#'   args=list(ay=2000, data_lag=1),
#'   tracking=FLQuant(dimnames=list(metric="ind", year=2000, iter=1:100)))
#' cpue.hcr(stk=stock(om), ind=ind$ind, k1=0.1, k2=0.2, k3=0.1, k4=0.1,
#'   args=list(ay=2000, frq=1, management_lag=1),
#'   tracking=FLQuants(sol174=FLQuant(1000, dimnames=list(metric="hcr",
#'   year=2000))))

cpue.hcr <- function(stk, ind, k1, k2, k3, k4, target=1,
  dtaclow=0.85, dtacupp=1.15, slope="slope", mean="mean", args, tracking) {

  # args
  ay <- args$ay
  frq <- args$frq
  man_lag <- args$management_lag

  # RECOVER slope & mean(cpue)
  slope <- ind[[slope]]
  mcpue <- ind[[mean]]

  # CALCULATE new tac
  ka <- ifelse(slope > 0, k1, k2)
  kb <- ifelse(mcpue > target, k3, k4)

  # GET previous TAC nfrom last hcr ...
  tac <- tracking[[1]]['hcr', ac(ay)]
  # ... OR catch
  if(all(is.na(tac)))
    tac <- seasonSums(catch(stk)[, ac(ay - args$data_lag)])

  # TAC_y-1 ~ TAC_y * 1 + ka * m + kb * (mcpue - target)
  tac <- tac * (1 + ka * slope + kb * (mcpue - target))
  
  # TAC limits, not on 1st year

  # CONTROL
  ctrl <- fwdControl(
    # TARGET for frq years
    c(lapply(seq(ay + man_lag, ay + frq), function(x)
      list(quant="catch", value=c(tac), year=x)))
  )

	return(list(ctrl=ctrl, tracking=tracking))
} # }}}

# pid.hcr {{{

# T_{y+1} = T_{y} * 1 - k1 * |lambda| ^ gamma, lambda < 0
#                   1 + k2 * lambda, lambda >= 0

#' @param stk
#' @param ind
#' @param kp
#' @param ki
#' @param kd
#' @param nyears Number of years used in regression of log(stock).
#' @param metric
#' @param ref
#' @examples
#' data(sol274)
#' track <- FLQuants(FLQuant(dimnames=list(metric='hcr', year=2000:2005,
#'   iter=seq(dims(om)$iter))))
#'  args <- list(ay=2003, data_lag=1, management_lag=1, frq=1, it=1)
#' #
#'  pid.hcr(stock(om), ind=FLQuant(), tracking=track, args=args,
#'  nyears=5, metric=ssb, ref=yearMeans(ssb(om)), kp=0.5, ki=0.01, kd=0.7)
#' # 
#' control <- mpCtrl(list(
#'   est = mseCtrl(method=perfect.sa),
#'   hcr = mseCtrl(method=pid.hcr,
#'     args=list(metric=ssb, ref=yearMeans(ssb(om)), kp=0.5, ki=0.01, kd=0.7))))
#' tes <- mp(om, oem=oem, ctrl=control, args=list(iy=2017))
#' plot(om, PID=tes)

pid.hcr <- function(stk, ind, kp=0, ki=0, kd=0, nyears=5,
  metric=ssb, ref, dlow=NA, dupp=NA, args, tracking) {
  
  # args
  spread(args)
  dy <- ac(ay - data_lag)
  dy1 <- ac(ay - data_lag - 1)

  # SELECT metric

  # ind, if only one,
  if(missing(metric) & length(ind) == 1) {
    metric <- ind[[1]]
  } else if (is(metric, "character")) {
    # or EXTRACT from ind
    if(metric %in% names(ind))
      met <- ind[[metric]]
    # or COMPUTE from stk,
    else
      met <- do.call(metric, list(stk))
  } else if(is(metric, "function")) {
    met <- do.call(metric, list(stk))
  }

  # WINDOW metric
  met <- window(met, start=ay - data_lag - (nyears - 1), end=ay - data_lag)
  
  # FIND iters with NAs

  # GET TAC from tracking['hcr',]
  pre <- tracking[[1]]['hcr', dy]

  # OR from previous catch
  if(all(is.na(pre)))
    pre <- seasonSums(unitSums(catch(stk)[, dy]))

  # CALCULATE divergence
  e <- log(met %/% ref)

  # COMPUTE control signal
  u <- kp * e[, dy] + ki * yearSums(e) + kd * (e[, dy] - e[,dy1])

  # COMPUTE factor
  fac <- min(max(exp(u^1), exp(u^2)), exp(u^3))

  # TAC
  tac <- fac * pre

  # TRACK initial TAC
  track(tracking, "tac.hcr", seq(ay + management_lag, ay + frq)) <- tac

  # LIMITS over previous output
  if(!is.na(dupp))
    tac[tac > pre * dupp] <- pre[tac > pre * dupp] * dupp
  if(!is.na(dlow))
    tac[tac < pre * dlow] <- pre[tac < pre * dlow] * dlow

  # CONTROL
  ctrl <- fwdControl(
    # TAC for frq years
    lapply(seq(ay + management_lag, ay + frq), function(x)
    list(quant="catch", value=c(tac), year=x))
  )

  return(list(ctrl=ctrl, tracking=tracking))
}

# }}}

# hr.hcr {{{

hr.hcr <- function(stk, ind, metric="mean", target, trigger,
  lambda=1, dtaclow=NA, dtacupp=NA, args, tracking) {

  # args
  sty<- args$iy
  ay <- args$ay
  frq <- args$frq
  data_lag <- args$data_lag
  man_lag <- args$management_lag
  dys <- ac(ay - data_lag)
  mys <- ac(seq(ay + man_lag, length.out=frq))

  # SELECT metric
  met <- ind[[metric]]
  
  # CALCULATE biomass safeguard, min(1, I_{ay-dlag} / Itrigger)
  bsg <- qmin(met / trigger, 1)
  
  # SET new TAC
  tac <- met * target * bsg * lambda

  # TRACK initial tac
  track(tracking, paste0("tac.hcr"), seq(ay + man_lag, ay + frq)) <- c(tac)

  # GET previous TAC | catch
  pre <- tracking[[1]]["hcr", dys]
  nas <- c(is.na(pre))
  if(any(nas)) {
    iter(pre, nas) <- iter(catch(stk)[, ac(ay - frq)], nas)
  }

  # TODO: PA buffer

  # APPLY limits if advice years is not the first simulation year
  if (ay != (sty + 1))
  {
      if(!is.na(dtacupp))
        tac[tac > pre * dtacupp] <- pre[tac > pre * dtacupp] * dtacupp
      if(!is.na(dtaclow))
        tac[tac < pre * dtaclow] <- pre[tac < pre * dtaclow] * dtaclow
  }
  # CONSTRUCT fwd control
  ctrl <- as(FLQuants(catch=expand(tac, year=mys)), "fwdControl")

  list(ctrl=ctrl, tracking=tracking)
}

# }}}

# hr.target {{{

hr.target <- function(len, catch, index, lc, linf) {

  # COMPUTE reference length
  reflen <- 0.75 * lc + 0.25 * linf

  # IDENTIFY years where mean length >= reflen
  iyr <- dimnames(len)$year[len >= reflen]

  # CALCULATE harvest rate for selected years only
  hr <- catch[, iyr] / index[, iyr]

  return(yearMeans(hr))
} 
# }}}

# -- COMBINE

# meta.hcr {{{

# TODO: USE for sequential: pchr(CHOOSE hcr)

#' @examples
#' data(sol274)
#' # Compute length-based indicators: leman and lbar
#' control <- mpCtrl(list(
#'   est = mseCtrl(method=len.ind, args=list(indicator=c('lmean', 'lbar'),
#'    params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2, nyears=5)),
#' # Apply trend.hcr to each indicator, return mean advice value
#' hcr = mseCtrl(method=meta.hcr,
#'    args=list(list(method="trend.hcr", k1=1, k2=2, metric="lmean"),
#'    list(method="trend.hcr", k1=2, k2=3, metric="lbar")))))
#' run01 <- mp(om, oem=oem, ctrl=control, args=list(iy=2020, fy=2030))
#' #
#' control <- mpCtrl(list(
#'   est = mseCtrl(method=len.ind, args=list(indicator=c('lmean', 'lbar'),
#'    params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2, nyears=5)),
#' #
#'   hcr = mseCtrl(method=meta.hcr,
#'    args=list(list(method="trend.hcr", k1=1, k2=2, metric="lmean"),
#'    list(method="trend.hcr", k1=1, k2=1, metric="lbar"),
#'    combination=function(x, y) x + 0.40 * y))))
#' #
#' run02 <- mp(om, oem=oem, ctrl=control, args=list(iy=2020, fy=2030))
#' #
#' plot(window(om, start=2000), list(R01=run01, R02=run02))

meta.hcr <- function(stk, ind, ..., args, tracking,
  combination=function(x, y) (x + y) / 2) {

  # HCRs arguments
  rargs <- list(...)

  # CHECK rargs have 'method'
  if(!all(unlist(lapply(rargs, function(x) "method" %in% names(x)))))
    stop("args must contain list with an element called 'method'")
  
  # CHECK list has list w/ method + matching args
  if(!all(unlist(lapply(rargs, function(x) all(names(x)[!grepl("method",
    names(x))] %in% names(formals(x$method)))))))
    stop("elements in each args list must match arguments in hcr function")

  # APPLY each hcr
  decs <- lapply(rargs, function(x)
    do.call(x$method, c(list(ind=ind, stk=stk, args=args, tracking=tracking),
      x[!grepl("method", names(x))]))
  )

  # CHECK methods use the same currency
  if(!Reduce(all.equal, lapply(decs, function(x) as.character(x$ctrl$quant))))
    stop("Individual hcrs must output the same quant (e.g. 'catch')")

  # COMBINE ctrls
  ctrl <- decs[[1]]$ctrl
  iters(ctrl) <- Reduce(combination, lapply(decs, function(i) iters(i$ctrl)))

  # TODO: MERGE tracking
  # BUG: tracking <- Reduce(merge, lapply(decs, function(x) x$tracking))

	# return
	list(ctrl=ctrl, tracking=tracking)
}
# }}}

# -- FUNCTIONS

# setFCB {{{
setFCB <- function(output=c("catch", "landings", "discards", "fbar", "f",
  "effort"), relative=FALSE, element=1) {

  # SELECT output from possible values
  output <- match.arg(output)

  # EXTRACT valid FCB for output
  targets <- subset(FLasher:::.vfcb, quant == output)[1,]

  # BUILD FCB list
  fcb <- ifelse(unlist(targets[c("fishery", "catch", "biol")]),
    element, as.numeric(NA))

  # ADD relative if needed
  if(relative)
    fcb <- c(fcb, setNames(fcb, nm=c("relFishery", "relCatch", "relBiol")))

  fcb
}
# }}}

# selecMetric {{{

selectMetric <- function(metric="missing", stk, ind) {

    # MISSING metric? ind
    if(missing(metric)) {
      if(length(ind) == 1) {
        met <- ind[[1]]
      } else {
        met <- ind
      }
    # CHARACTER?
    } else if (is(metric, "character")) {
      # EXTRACT from ind,
      if(metric %in% names(ind))
        met <- ind[[metric]]
      # or COMPUTE from stk
      else
        met <- do.call(metric, list(stk))
    # FUNCTION?
    } else if(is(metric, "function")) {
      met <- do.call(metric, list(stk))
    }
    return(met)
}
# }}}

# ---

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
#' data(sol274)
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
# 
