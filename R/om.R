# om.R - DESC
# mse/R/om.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# runOM {{{

runOM <- function(lhs, history, deviances, ...) {

  # VARIABLES
  its <- dims(deviances)$iter

  # CREATE equilibrium population
  eq <- lhEql(lhs, ...)

  # ADJUST fbar to history
  fbar(eq) <- history %*% fbar(eq)[catch(eq) == max(catch(eq))]

  # BUT limited by 90% Fcrash
  fbar(eq) <- qmin(fbar(eq), refpts(eq)["crash", "harvest"] * 0.9)

  # COERCE into om FLStock
  om <- as(eq, "FLStock")

  # SET catch >= 0
  stock.n(om)[stock.n(om) < 0] <- 0.0001
  catch.n(om)[catch.n(om) < 0] <- 0.0001
  landings.n(om)[landings.n(om) < 0] <- 0.0001

  # SET fbar range
  if(!"range" %in% names(list(...)))
    range(om, c("minfbar", "maxfbar")) <- 
      mean(ages(m(om))[harvest(om) == expand(fapex(om), age=dimnames(om)$age)])

  # PROJECT for history
  om <- ffwd(propagate(om, its), sr=eq, fbar=fbar(eq)[, -1],
    deviances=deviances)

  # EXTRACT refpts
  rps <- FLPar(FMSY=fmsy(eq), SBMSY=sbmsy(eq), MSY=msy(eq))

  # OUTPUT FLStockR
  res <- FLStockR(om, sr=as(eq, "predictModel"), refpts=rps)

  return(res)
}
# }}}

# initiate {{{

#' Initializes a population for a given virgin biomass.
#'
#' Abundances at age for a population at virgin conditions at age. An `FLBiol`
#' object is initiated by providing a target total biomass (`B0`) and a value
#' for the stock-recruit steepness (`s`). The object requires slots to be
#' already filled up for the mean weight-at-age (`wt`), natural mortality
#' (`m`), time of spawning (`spwn`) and maturity at age (`mat`).
#'
#' @param biol An `FLBiol` object to nbe initiated.
#' @param B0 Initial or virgin biomass.
#'
#' @return An updated `FLBiol` with abundances set in teh first year to match
#' the requested biomas.
#' @export
#'
#' @examples
#' data(ple4.biol)
#' initiate(ple4.biol, B0=450000)
#' #
#' initiate(propagate(ple4.biol, 100), B0=runif(100, 3e5, 5e5))

initiate <- function(biol, B0, h=0.75) {

  # SET iters
  dms <- dims(biol)
  its <- dms$iter
  na <- dms$age
  B0 <- rep(B0, length=its)

  sr(biol) <- predictModel(model=bevholtss3()$model,
    params=propagate(FLPar(s=NA, R0=NA, v=NA), its))

  # SOLVE R0 for B0, WT, M + F
  foo <- function(R0, n, m, wt, b0) {

    n[1] <- R0

    for(a in seq(2, length(n)))
      n[a] <- n[a - 1] * exp(-m[a - 1])
    # plusgroup
    n[a] <- n[a] / (1 - exp(-m[a]))

    return(sum((c(b0) - sum(wt * n)) ^ 2))
  }
 
  # TODO: DEAL with iters in biol, B0 and h

  init <- 1000

  # RUN for iters
  #for(i in seq(its)) {
  res <- foreach(i=seq(its), .combine=c) %dopar% {

    # EXTRACT to vectors
    m <- c(iter(m(biol)[, 1],i))
    wt <- c(iter(wt(biol)[, 1],i))
    n <- c(n(biol)[, 1,,,,i])

    res <- optim(init, foo, method="Brent", lower=1, upper=1e12,
      n=n, m=m, wt=wt, b0=B0[i])

    res$par
  }

  # RECONSTRUCT initial population

  # rec
  n(biol)[1, 1] <- res
    
  # n
  for(a in seq(2, na))
    n(biol)[a, 1] <- n(biol)[a - 1, 1] * exp(-m(biol)[a - 1, 1])

  # pg
  n(biol)[na, 1] <- n(biol)[na, 1] / (1 - exp(-m(biol)[na, 1]))
  
  # ADD bevholtss3 SRR
  browser()
  params(sr(biol)) <- FLPar(s=h, R0=res, v=ssb(biol)[,1])

  # RETURN FLBiol
  return(biol)
}
# }}}

# deplete {{{

#' @examples
#' data(ple4)
#' ini <- initiate(propagate(ple4.biol, 100), B0=runif(100, 3e5, 5e5))
#' dep <- deplete(ini, sel=catch.sel(ple4)[, 10], dep=runif(100, 0.10, 0.90))

deplete <- function(biol, sel, dep) {

  # GET dims
  dm <- dim(n(biol))
  myr <- dims(biol)$minyear
  mag <- dims(biol)$max

  # EXTRACT slots
  waa <- wt(biol)[, 1]
  maa <- m(biol)[, 1]
  mat <- mat(biol)[, 1]
  msp <- spwn(biol)[, 1]
  sr <- sr(biol)

  # USE only first year of sel
  sel <- sel[, 1]

  # TODO: FLBRP(FLBiol, sel)
  # FLBRP
  brp <- FLBRP(stock.wt=waa, landings.wt=waa, discards.wt=waa, bycatch.wt=waa,
    mat=mat, landings.sel=sel, m=maa,
    discards.sel=sel%=%0, bycatch.harvest=sel%=%0,
    harvest.spwn=maa %=% 0, m.spwn=maa %=% 0,
    availability=maa %=% 1,
    range=c(minfbar=0, maxfbar=mag, plusgroup=mag))
 
  # TODO: sr bevholtss3 -> bevholt
  # ADD sr
  psr <- params(sr)
  npsr <- abPars("bevholt", spr0=psr$v / psr$R0, s=psr$s, v=psr$v)
  model(brp) <- bevholt()$model
  params(brp) <- FLPar(a=npsr$a, b=npsr$b)

  # FIT
  brp <- brp(brp)

  # SET finer fbar range on Fcrash
  fmax <- max(refpts(brp)['crash', 'harvest'])
  # or fmax
  if(is.na(fmax))
    fmax <- max(refpts(brp)['fmax', 'harvest']) * 1.25
  fbar(brp) <- FLQuant(seq(0, fmax, length=301))

  # ADD Btgt
  refpts(brp, "target", "biomass") <- c(tb(biol)[,1] ) * dep

  # FIND refpts$target fbars in fbar(brp)

  ftarget <- c(refpts(brp)["target", "harvest",])
  fbars <- c(fbar(brp))
  idx <- seq(301)[max.col(-abs(outer(ftarget, fbars,"-")))]

  # COMPUTE stock.n
  stn <- stock.n(brp)

  # CONVERT to vector
  vstn <- c(stn)

  # GET dimensions and iters
  dmn <- dim(stn)
  its <- seq(dmn[6])

  # EXTRACT years matching idx by iter
  ii <- unlist(lapply(seq(length(idx)), function(x)
    seq((its[x] - 1) * (dmn[1] * dmn[2]) + (dmn[1] * (idx[x] - 1)) + 1,
      (its[x] - 1) * (dmn[1] * dmn[2]) + (dmn[1] * (idx[x] - 1)) + dmn[1])
    ))

  # ASSIGN via c()
  n(biol)[,1] <- vstn[ii]

  attr(biol, "refpts") <- refpts(brp)

  return(biol)
}
# }}}

# simulator {{{

#' @param biol
#' @param fisheries
#' @param v Virgin total biomass or carrying capacity (K).
#' @param h Steepness of the Beverton & Holt stock-recruitment relationship.
#' @param d Initial depletion level, as proportion of v.
#' @param sigmaR Variance of the recruitment deviances.
#' @param rho Autocorrelation in recruitment, only used if no deviances are provided.
#' @param control Past history for forward projection, tipically a catch series.
#' @param deviances Deviances over the stock-recruits relationship
#' @param invalk Inverse Age-Length Key to generate length samples from the generated catch-at-age.
#' @examples
#' fq <- FLQuant(NA, dimnames=list(age=1:6, year=10:30))
#' # BIOL
#' bio <- FLBiol(
#' wt=fq %=% seq(0.03, 8, length=6),
#' m=fq %=% c(0.6, 0.3, rep(0.2, 4)),
#' spwn=fq[1,] %=% 0.5,
#' rec=predictModel(model=rec~a, params=FLPar(a=1765)),
#' mat=predictModel(model=~mat, FLQuants(mat=fq %=% c(0, 0.2, 0.5, rep(1,3)))))
#' # FISHERY
#' fis <- FLFishery(
#'   effort=FLQuant(25/10000, dimnames=list(year=10:30)),
#' A=FLCatch(landings.wt=wt(bio), discards.wt=wt(bio),
#'   landings.n=fq %=% 1, discards.n=fq %=% 0,
#'   catch.sel=fq %=% c(0.1, 0.3, 0.4, 0.8, 0.9, 1)))
#' # Project for 20 year catch trend: c(50, 100-800, 1050-400)
#' sim <- simulator(bio, FLFisheries(A=fis), v=13000, d=0.01,
#'   history=FLQuants(catch=FLQuant(c(50, seq(100, 800, length=9),
#'   seq(1050, 400, length=10)), dimnames=list(year=11:30))))
#' plot(sim$biol)

simulator <- function(biol, fisheries, B0, h, dep=0, sigmaR=0,
  history, deviances=ar1rlnorm(rho=0, years=dimnames(biol)$year, iter=1,
    meanlog=0, sdlog=sigmaR), invalk="missing") {
  
  # INITIATE N0
  nbiol <- initiate(biol, B0=B0, h=h)

  # COMBINED selectivity for depletion, catch 1
  if (length(fis) > 1)
    sel <- Reduce("+", lapply(fis, function(x) catch.sel(x[[1]])) *
      lapply(fis, function(x) catch.n(x[[1]]))) /
      Reduce("+", lapply(fis, function(x) catch.n(x[[1]])))
  else
    sel <- catch.sel(fis[[1]][[1]])

  # DEPLETE to dep
  nbiol <- deplete(nbiol, sel=sel[,1], dep=dep)

  # ADD age devs, no bias correction needed
  lage <- dims(biol)$age
  n(nbiol)[-lage, 1] <- n(nbiol)[-lage, 1] * rlnorm(lage - 1, 0, sigmaR)

  # CONVERT history
  if(!is(history, "fwdControl"))
    history <- as(history, "fwdControl")
  
  # SET deviances
  if(!missing(deviances))
    deviances <- rlnorm(1, log(deviances), sigmaR)

  # FWD w/history
  res <- fwd(nbiol, fisheries, control=history, deviances=deviances,
    effort_max=1e6)

  # LEN samples
  if(!missing(invalk)) {
    cafs <- lapply(res$fisheries, function(x) catch.n(x[[1]]) + 1e-6)
    res$lengths <- lapply(cafs, lenSamples, invALK=invalk, n=250)
  }

  return(res)
}

# }}}
