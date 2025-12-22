# tune.R - DESC
# /tune.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# tunebisect: BISECTION tuning {{{

#' 
#' @examples
#' # dataset contains both OM (FLom) and OEM (FLoem)
#' data(sol274)
#' # choose sa and hcr
#' control <- mpCtrl(list(
#'   est = mseCtrl(method=perfect.sa),
#'   hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
#'   trigger=41500, target=0.27))))
#' # load statistics
#' data(statistics)
#' tun <- tunebisect(om, oem=oem, control=control, args=list(iy=2021, fy=2035),
#' tune=list(target=c(0.15, 0.35)),
#' metrics=list(SB=ssb), statistic=statistics['PSBMSY'], years=2025:2034)
#' # Plot tuned MP
#' plot(om, tun)

tunebisect <- function(om, oem=NULL, control, statistic, metrics=NULL, args,
  tune, prob=0.5, tol=0.01, maxit=12, years=ac(seq(args$iy + 1, args$fy - 1)),
  verbose=TRUE, ...) {

  # CALL recursively if multiple prob
  if(length(prob) > 1) {

    res <- (lapply(setNames(prob, nm=paste0("prob_", prob)), function(p) {
      message(paste0("Tuning for prob=", p))
      tunebisect(om=om, oem=oem, control=control, statistic=statistic, metrics=metrics,
      args=args, tune=tune, prob=p, tol=tol, maxit=maxit, years=years, verbose=verbose, ...)
    }))

    # WARN if not tuned
    if(any(unlist(lapply(res, is, 'FLmses')))) {
      warning("Some tuning(s) did not converge, check individual results.")

      return(res)

    } else {
      return(FLmses(res))
    }
  }
  
  # args
  args$fy <- if(is.null(args$fy)) dims(om)$maxyear else args$fy

  # CHECK years
  if(!all(unique(unlist(years)) %in% seq(args$iy, args$fy)))
    stop("Years for statistic computation 'years' outside of 'args' year range (iy:fy).")

  # CHECK that tune names match args(control$hcr)
  if(!names(tune) %in% formalArgs(method(control$hcr)))
    stop("Element in tune must be a argument of method(control$hcr).")

  # CHECK length(tune)  == 1
  if(length(tune) > 1)
    stop(paste0("'tunebisect' can only tune for a single HCR argument, got ",
      names(tune), "."))

  # CHECK length(tune[[1]]) == 2
  if(length(tune[[1]]) != 2)
    stop("Range of argument for tuning has more than 2 values, need min and max.")

  # CHECK statistic: single statistic as length 1 list
  if(length(statistic) != 1) {
    stop("'statistic' must be a named list of length 1 with formula, 'name', and 'desc'")
  }

  # CHECK 0 < prob < 1
  if(!(prob >=0 & prob <=1))
    stop("prob must be a value between 0 and 1.")

  # --- RUN at min

  cmin <- control
  cmin$hcr@args[names(tune)] <- lapply(tune, '[', 1)

  # PRINT at top
  if(verbose)
    message(paste0("[1] ", names(tune), ": ",
      unlist(cmin$hcr@args[names(tune)])))

  rmin <- mp(om, oem=oem, ctrl=cmin, args=args, scenario=paste0("min"),
    verbose=FALSE, ...)

  pmin <- performance(rmin, metrics=metrics, 
    statistics=statistic, refpts=refpts(om), probs=NULL, years=years)
  obmin <- mean(pmin$data, na.rm=TRUE) - prob

  # PRINT result
  if(verbose)
    message(paste0("[1] diff: ", format(obmin, digits=2), ", prob: ",
      format(mean(pmin$data, na.rm=TRUE), digits=2)))

  # CHECK cmin result
  if(isTRUE(all.equal(obmin, 0, tolerance=tol)))
    return(rmin)
  
  # --- RUN at max

  cmax <- control
  cmax$hcr@args[names(tune)] <- lapply(tune, '[', 2)

  # PRINT at top
  if(verbose)
    message(paste0("[2] ", names(tune), ": ",
      unlist(cmax$hcr@args[names(tune)])))

  rmax <- mp(om, oem=oem, ctrl=cmax, args=args, scenario=paste0("max"),
    verbose=FALSE, ...)
  
  pmax <- performance(rmax, metrics=metrics,
    statistic=statistic, refpts=refpts(om), probs=NULL, years=years)
  obmax <- mean(pmax$data, na.rm=TRUE) - prob

  # PRINT result
  if(verbose)
    message(paste0("[2] diff: ", format(obmax, digits=2), ", prob: ",
      format(mean(pmax$data, na.rm=TRUE), digits=2)))
  
  # CHECK cmax result
  if(isTRUE(all.equal(obmax, 0, tolerance=tol)))
    return(rmax)

  # CHECK range includes 0
  # TODO: PROPOSE wider range, min * 0.5, max * 1.5, and restart (recursive?).
  if((obmin * obmax) > 0) {
    warning(paste("Range of hcr param(s) cannot achieve requested tuning objective probability: min =", round(obmin, 3), ", max=", round(obmax, 3)))
    return(FLmses(list(min=rmin, max=rmax)))
  }

  # --- LOOP bisecting

  count <- 1
  while(count <= maxit) {

    # RUN at mid
    cmid <- control
    cmid$hcr@args[[names(tune)]] <-
      (cmin$hcr@args[[names(tune)]] + cmax$hcr@args[[names(tune)]]) / 2

    # PRINT at top
    if(verbose)
      message(paste0("[", count + 2, "] ", names(tune), ": ",
        format(unlist(cmid$hcr@args[names(tune)]), digits=3)))

    rmid <- mp(om, oem=oem, ctrl=cmid, args=args, scenario=paste0("mid"),
      verbose=FALSE, ...)

    pmid <- performance(rmid, metrics=metrics, 
      statistics=statistic, refpts=refpts(om), probs=NULL, years=years)
    obmid <- mean(pmid$data, na.rm=TRUE) - prob

    # PRINT result
    if(verbose)
      message(paste0("[", count + 2, "] diff: ", format(obmid, digits=2),
        ", prob: ", format(mean(pmid$data, na.rm=TRUE), digits=2)))
  
    # CHECK and RETURN cmid result
    if(isTRUE(all.equal(obmid, 0, tolerance=tol))) {
      return(rmid)
    }

    # TEST LEFT
    if((obmin * obmid) < 0) {

      # SET max as new mid
      cmax <- cmid
      obmax <- obmid

    } else {
      
      # SET min as new mid
      cmin <- cmid
      obmin <- obmid
    }

    count <- count + 1
  }

  stop("Solution not found within 'maxit', check 'range', 'maxit' or 'tol'.")

} # }}}
