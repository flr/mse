# tune.R - DESC
# /tune.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# tunebisect: BISECTION tuning {{{

#' 
#' @examples

tunebisect <- function(om, oem="missing", control, metrics, indicator, tune, args,
  prob=0.5, tol=0.01, maxit=12, verbose=TRUE, pyears=ac(seq(args$iy+1, args$fy)),
  refpts=refpts(om), ...) {
  
  # TODO CHECKS

  # indicator is of length 1
  # 0 < prob < 1
  # tune is of length 1
  # element in tune is of length 2

  # RUN at min
  cmin <- control
  cmin$hcr@args[names(tune)] <- lapply(tune, '[', 1)

  rmin <- mp(om, oem=oem, ctrl=cmin, args=args, scenario=paste0("min"), ...)
  
  pmin <- performance(metrics(stock(rmin), metrics=metrics), indicator=indicator,
    refpts=refpts, probs=NULL, years=pyears)
  obmin <- mean(pmin$data) - prob
  
  if(verbose)
    print(paste0("[1] diff: ", format(obmin, digits=2), "; ", names(tune), ": ",
      unlist(cmin$hcr@args[names(tune)])))

  # CHECK cmin result
  if(isTRUE(all.equal(obmin, 0, tolerance=tol)))
    return(rmin)
  
  # RUN at max
  cmax <- control
  cmax$hcr@args[names(tune)] <- lapply(tune, '[', 2)

  rmax <- mp(om, oem=oem, ctrl=cmax, args=args, scenario=paste0("max"), ...)
  
  pmax <- performance(metrics(stock(rmax), metrics=metrics), indicator=indicator,
    refpts=refpts, probs=NULL, years=pyears)
  obmax <- mean(pmax$data) - prob
  
  if(verbose)
    print(paste0("[2] diff: ", format(obmax, digits=2), "; ", names(tune), ": ",
      unlist(cmax$hcr@args[names(tune)])))
  
  # CHECK cmax result
  if(isTRUE(all.equal(obmax, 0, tolerance=tol)))
    return(rmax)
  
  # CHECK range includes 0
  if((obmin * obmax) > 0) {
    stop("Range of hcr param(s) cannot achieve requested tuning objective probability")
    return(list(min=rmin, max=rmax))
  }

  # LOOP bisecting
  count <- 0
  while(count <= maxit) {

    # RUN at mid
    cmid <- control
    cmid$hcr@args[[names(tune)]] <-
      (cmin$hcr@args[[names(tune)]] + cmax$hcr@args[[names(tune)]]) / 2

    rmid <- mp(om, oem=oem, ctrl=cmid, args=args, scenario=paste0("mid"), ...)
    pmid <- performance(metrics(stock(rmid), metrics=metrics), indicator=indicator,
      refpts=refpts, probs=NULL, years=pyears)
    obmid <- mean(pmid$data) - prob

    if(verbose)
      print(paste0("[", count + 2, "] diff: ", format(obmid, digits=2), "; ",
        names(tune), ": ", unlist(cmid$hcr@args[names(tune)])))
  
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

  warning("Solution not found within 'maxit', check 'range', 'maxit' or 'tol'.")

  return(rmid)

} # }}}
