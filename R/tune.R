# tune.R - DESC
# /tune.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# tunebisect: BISECTION tuning {{{

#' 
#' @examples

tunebisect <- function(om, oem="missing", control, metrics, statistic, tune,
  args, prob=0.5, tol=0.01, maxit=12, verbose=TRUE,
  years=ac(seq(args$iy+1, args$fy)), ...) {
  
  # args
  args$fy <- if(is.null(args$fy)) dims(om)$maxyear else args$fy

  # CHECK years
  if(!all(unique(unlist(years)) %in% seq(args$iy, args$fy)))
    stop("Years for statistic computation 'years' outside of 'args' year range (iy:fy).")

  # CHECK that tune names match args(control$hcr)
  if(!names(tune) %in% names(args(control$hcr)))
    stop("Element in tune must be a argument in args(control$hcr).")

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
    print(paste0("[1] ", names(tune), ": ",
      unlist(cmin$hcr@args[names(tune)])))

  rmin <- mp(om, oem=oem, ctrl=cmin, args=args, scenario=paste0("min"),
    verbose=FALSE, ...)
  
  pmin <- performance(rmin, metrics=metrics, 
    statistic=statistic, refpts=refpts(om), probs=NULL, years=years)
  obmin <- mean(pmin$data, na.rm=TRUE) - prob
  
  # PRINT result
  if(verbose)
    print(paste0("[1] diff: ", format(obmin, digits=2), "; ", names(tune), ": ",
      unlist(cmin$hcr@args[names(tune)])))

  # CHECK cmin result
  if(isTRUE(all.equal(obmin, 0, tolerance=tol)))
    return(rmin)
  
  # --- RUN at max

  cmax <- control
  cmax$hcr@args[names(tune)] <- lapply(tune, '[', 2)

  # PRINT at top
  if(verbose)
    print(paste0("[2] ", names(tune), ": ",
      unlist(cmax$hcr@args[names(tune)])))

  rmax <- mp(om, oem=oem, ctrl=cmax, args=args, scenario=paste0("max"),
    verbose=FALSE, ...)
  
  pmax <- performance(rmax, metrics=metrics,
    statistic=statistic, refpts=refpts(om), probs=NULL, years=years)
  obmax <- mean(pmax$data, na.rm=TRUE) - prob
  
  # PRINT result
  if(verbose)
    print(paste0("[2] diff: ", format(obmax, digits=2)))
  
  # CHECK cmax result
  if(isTRUE(all.equal(obmax, 0, tolerance=tol)))
    return(rmax)

  # CHECK range includes 0
  if((obmin * obmax) > 0) {
    warning("Range of hcr param(s) cannot achieve requested tuning objective probability")
    return(list(min=rmin, max=rmax))
  }

  # --- LOOP bisecting

  count <- 0
  while(count <= maxit) {

    # RUN at mid
    cmid <- control
    cmid$hcr@args[[names(tune)]] <-
      (cmin$hcr@args[[names(tune)]] + cmax$hcr@args[[names(tune)]]) / 2

    # PRINT at top
    if(verbose)
      print(paste0("[", count + 2, "] ", names(tune), ": ",
        unlist(cmid$hcr@args[names(tune)])))

    rmid <- mp(om, oem=oem, ctrl=cmid, args=args, scenario=paste0("mid"),
      verbose=FALSE, ...)
    pmid <- performance(rmid, metrics=metrics, 
      statistics=statistic, refpts=refpts(om), probs=NULL, years=years)
    obmid <- mean(pmid$data, na.rm=TRUE) - prob

    # PRINT result
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

# tunegrid: GRID tuning {{{

#' @examples
#' data(ple4om)
#' tes <- tunegrid(om, oem, control, metric=list(SB=ssb), statistic=stats['S8'],
#'   grid=grid, args=mseargs, years=list(2030:2039), iters=1:100)

tunegrid <- function(om, oem="missing", control, metric, statistic, grid, args,
  years=ac(seq(args$iy+1, args$fy)), parallel=FALSE, verbose=FALSE, 
  iters=dims(om)$iter, ...) {

  if(!missing(iters)) {
    om <- iter(om, iters)
    oem <- iter(oem, iters)
  }

  fixed <- as.data.frame(args(control$hcr)[!names(args(control$hcr)) %in% names(grid)])

  # dopar LOOP over grid

  res <- foreach(i=seq(dim(grid)[1]), .errorhandling = "remove") %dopar% {

    # TODO ADD progressr

    cat("[", i, "]\n")

    # ASSIGN new control$hcr@args values
    args(control$hcr)[names(grid)] <- grid[i,]

    # RUN mp
    run <- mp(om=om, oem=oem, ctrl=control,
      args=args, parallel=parallel, verbose=verbose, ...)

    # COMPUTE performance & BIND to grid
    cbind(performance(run, metric=metric, statistic=statistic,
      years=years), grid[i,, drop=FALSE], fixed)
  }

  # TODO ADD empty row for missing or use try()and .combine

  res <- rbindlist(res, idcol="run")

  return(res)
}

# }}}

# bisect {{{

#' Bisection search for a forecast target providing a given performance statistic value.
#'
#' The plain bisection algorithm (Burden & Douglas, 1985) is employed here to
#' find the value of a given forecast target quantity (e.g. `fbar`) for which
#' a selected value of a performance statistic is obtained over a chosen period.
#' @references {Burden, Richard L.; Faires, J. Douglas (1985), "2.1 The Bisection Algorithm", Numerical Analysis (3rd ed.), PWS Publishers, ISBN 0-87150-857-5}
#' @param stock
#' @param sr
#' @param metrics
#' @param statistic
#' @param years
#' @param tune
#' @param prob
#' @param tol
#' @param maxit
#' @examples
#' data(ple4)
#' stock <- propagate(stf(ple4, end=2118), 100)
#' srr <- predictModel(model=rec~a*ssb*exp(-b*ssb), params=FLPar(a=5.20, b=1.65e-6))
#' # GENERATE SRR deviances
#' devs <- ar1rlnorm(rho=0.4, 2019:2118, iters=100, meanlog=0, sdlog=0.5)
#' # DEFINE Fp05 statistic
#' statistic <- list(FP05=list(~yearMeans((SB/SBlim) < 1), name="P.05",
#'   desc="ICES P.05"))
#' # CALL bisect over 100 years
#' fp05fwd <- bisect(stock, sr=srr, deviances=devs, metrics=list(SB=ssb), 
#' refpts=FLPar(SBlim=150000), statistic=statistic, years=2018:2118,
#' pyears=2069:2118, tune=list(fbar=c(0.1, 1)), prob=0.05)

bisect <- function(stock, sr, deviances=rec(stock) %=% 1, metrics, refpts,
  statistic, years, pyears=years, tune, prob, tol=0.01, maxit=15, verbose=TRUE) {

  # --- RUN at min

  cmin <- fwdControl(year=years, quant=names(tune)[1], value=unlist(tune)[1])

  # PRINT at top
  if(verbose)
    cat(paste0("[1] ", names(tune), ": ", unlist(tune)[1]))

  rmin <- fwd(stock, sr=sr, control=cmin, deviances=deviances)
  
  pmin <- performance(rmin, metrics=metrics, 
    statistics=statistic, refpts=refpts, probs=NULL, years=pyears)

  obmin <- mean(pmin$data, na.rm=TRUE) - prob
  
  if(verbose)
    cat(" - prob:", mean(pmin$data, na.rm=TRUE), " - diff: ", obmin, "\n")
  
  # CHECK cmin result
  if(isTRUE(all.equal(obmin, 0, tolerance=tol)))
    return(rmin)
  
  # --- RUN at max

  cmax <- fwdControl(year=years, quant=names(tune)[1], value=unlist(tune)[2])

  # PRINT at top
  if(verbose)
    cat(paste0("[2] ", names(tune), ": ", unlist(tune)[2]))

  rmax <- fwd(stock, sr=sr, control=cmax, deviances=deviances)
  
  pmax <- performance(rmax, metrics=metrics, 
    statistics=statistic, refpts=refpts, probs=NULL, years=pyears)
  obmax <- mean(pmax$data, na.rm=TRUE) - prob

  if(verbose)
    cat(" - prob:", mean(pmax$data, na.rm=TRUE), " - diff: ", obmax, "\n")
  
  # CHECK cmax result
  if(isTRUE(all.equal(obmax, 0, tolerance=tol)))
    return(rmax)
 
  # --- CHECK range includes 0
  if((obmin * obmax) > 0) {
    warning("Range of hcr param(s) cannot achieve requested tuning objective probability")
    return(list(min=rmin, max=rmax))
  } 

  # --- LOOP bisecting

  count <- 0
  while(count <= maxit) {

    # RUN at mid
    cmid <- control
    cmid <- fwdControl(year=years, quant=names(tune)[1],
      value=(cmin$value + cmax$value) / 2)

    # PRINT at mid
    if(verbose)
      cat(paste0("[", count + 3, "] ", names(tune), ": ", cmid$value[1]))

    rmid <- fwd(stock, sr=sr, control=cmid, deviances=deviances)

    pmid <- performance(rmid, metrics=metrics, 
      statistics=statistic, refpts=refpts, probs=NULL, years=pyears)
    obmid <- mean(pmid$data, na.rm=TRUE) - prob

    if(verbose)
      cat(" - prob:", mean(pmid$data, na.rm=TRUE), " - diff: ", obmid, "\n")

    # CHECK and RETURN cmid result
    if(isTRUE(all.equal(obmid, 0, tolerance=tol))) {
      return(rmid)
    }

    # TEST LEFT
    if((obmin * obmid) < 0) {

      # SET max as new mid
      cmax <- cmid
      obmax <- obmid
      if(isTRUE(all.equal(cmin$value[1], cmid$value[1], tolerance=tol))) {
        return(rmid)
      }
    } else {

      # SET min as new mid
      cmin <- cmid
      obmin <- obmid
      if(isTRUE(all.equal(cmid$value[1], cmax$value[1], tolerance=tol))) {
        return(rmid)
      }
    }
    count <- count + 1
  }

  warning("Solution not found within 'maxit', check 'range', 'maxit' or 'tol'.")

  return(rmid)

} # }}}

# computeFp05 {{{

#' Calculates the Fbar value giving a maximum probability of ssb being Blim of 5%
#'
#' @param stock An FLStock over which the calculation is carried out.
#' @param sr The stock-recruits relationship tpo use in fwd.
#' @param SBlim
#' @param range
#' @param nyears
#' @param sigmaR
#' @param rho
#' @param its Number of iterations
#' @param verbose Should progress be shown, TRUE.
#' @examples
#' data(ple4)
#' sr <- predictModel(model=bevholt, params=FLPar(a=1.4e6, b=1.5e5))
#' fp05 <- computeFp05(ple4, sr, SBlim=150000, its=300, range=c(0.10,0.40))
#' # RUN projection for obtained Fp.05 value
#' proj <- fwd(propagate(stf(ple4, nyears=100), 300), sr=sr,
#'   fbar=FLQuant(fp05, dimnames=list(year=2018:2117)),
#'   deviances=ar1rlnorm(rho=0.43, years=2018:2117, iters=300, meanlog=0,
#'   sdlog=0.5))
#' plot(ssb(proj), prob=c(0.01, 0.25, 0.50, 0.75, 0.99)) +
#'   geom_hline(yintercept=150000)

computeFp05 <- function(stock, sr, SBlim, range=c(0.01, 0.75), nyears=3,
  sigmaR=0.5, rho=0.43, its=500, verbose=TRUE) {

  years <- seq(dims(stock)$maxyear + 1, length=100)
  pyears <- years[51:100]

  # GENERATE SRR deviances
  devs <- ar1rlnorm(rho=rho, years=years, iters=its, meanlog=0, sdlog=sigmaR)

  # EXTEND & PROPAGATE
  
  stock <- stf(stock, end=years[100], wts.nyears=nyears)
  stock <- propagate(stock, its)

  statistic <- list(FP05=list(~apply(iterMeans((SB/SBlim) < 1), c(1, 3:6), max),
    name="P.05", desc="ICES P.05"))

  res <- bisect(stock, sr=sr, refpts=FLPar(SBlim=SBlim), deviances=devs,
    metrics=list(SB=ssb), statistic=statistic, years=years, pyears=pyears, 
    tune=list(fbar=range), prob=0.05, tol=0.01, verbose=verbose)

  fp05 <- mean(fbar(res)[,100])

  return(c(Fp05=fp05))
}
# }}}
