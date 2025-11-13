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

  # SETUP initial sr
  sr(biol) <- predictModel(model=bevholtss3()$model,
    params=propagate(FLPar(s=h, R0=NA, v=NA), its))

  # SOLVE R0 for B0, WT, M + F
  foo <- function(R0, n, m, wt, b0) {

    n[1] <- R0

    for(a in seq(2, length(n)))
      n[a] <- n[a - 1] * exp(-m[a - 1])
    # plusgroup
    n[a] <- n[a] / (1 - exp(-m[a]))

    return((c(b0) - sum(wt * n)))
  }
 
  # TODO: DEAL with iters only in biol, B0 and h

  # INITIAL value, assumes 1,000 rec per t SSB.
  init <- B0

  # RUN for iters
  res <- foreach(i=seq(its), .combine=c) %do% {

    # EXTRACT to vectors
    m <- c(iter(m(biol)[, 1], i))
    wt <- c(iter(wt(biol)[, 1], i))
    n <- c(iter(n(biol)[, 1], i))

    res <- uniroot(foo, c(1, 1e12), n=n, m=m, wt=wt, b0=B0[i])
    print(i)
    res$root
  }

  # RECONSTRUCT initial population

  # rec
  n(biol)[1, 1] <- res
    
  # n
  for(a in seq(2, na))
    n(biol)[a, 1] <- n(biol)[a - 1, 1] * exp(-m(biol)[a - 1, 1])

  # pg
  n(biol)[na, 1] <- n(biol)[na, 1] / (1 - exp(-m(biol)[na, 1]))
  
  # ADD R0 param
  params(sr(biol))$R0 <- res
  params(sr(biol))$v <- ssb(biol)[,1]

  # RETURN FLBiol
  return(biol)
}
# }}}

# deplete {{{

#' @examples
#' data(ple4)
#' ini <- initiate(propagate(ple4.biol, 100), B0=runif(100, 3e5, 5e5))
#' dep <- deplete(ini, sel=catch.sel(ple4)[, 10], dep=runif(100, 0.10, 0.90))

deplete <- function(biol, sel, dep, minfbar=dims(biol)$min, 
  maxfbar=dims(biol)$max) {

  # GET dims
  dm <- dim(n(biol))
  myr <- dims(biol)$minyear
  mag <- dims(biol)$max

  # CHECK inputs
  # dep ~ [0,1]
  if(!all(dep <= 1 & dep >= 0))
    stop("depletion (dep) must fall between 0 and 1.")
  # maxfbar <= mag
  if(maxfbar > mag)
    maxfbar <- mag

  # EXTRACT slots
  waa <- wt(biol)[, 1]
  maa <- m(biol)[, 1]
  mat <- mat(biol)[, 1]
  msp <- spwn(biol)[, 1]
  sr <- sr(biol)

  # USE only first year of sel
  sel <- sel[, 1]

  # FLBRP
  brp <- FLBRP(stock.wt=waa, landings.wt=waa, discards.wt=waa, bycatch.wt=waa,
    mat=mat, landings.sel=sel, m=maa,
    discards.sel=sel %=% 0, bycatch.harvest=sel %=% 0,
    harvest.spwn=maa %=% 0, m.spwn=maa %=% 0,
    availability=maa %=% 1,
    range=c('minfbar'=minfbar, 'maxfbar'=maxfbar, 'plusgroup'=mag))

  # ADD sr as bevholt(a,b)
  psr <- params(sr)
  npsr <- abPars("bevholt", spr0=psr$v / psr$R0, s=psr$s, v=psr$v)
  model(brp) <- bevholt()$model
  params(brp) <- FLPar(a=npsr$a, b=npsr$b)
  
  # ADD Btgt
  brp <- brp(brp)
  refpts(brp, "target", "biomass") <- c(refpts(brp)['virgin', 'biomass',]) * dep
  
  # SET fbar as ftarget by iter
  fbar(brp) <- FLQuant(c(refpts(brp)['target', 'harvest',]),
    dim=c(1, 1, 1, 1, 1, dm[6]))

  # COMPUTE & ASSIGN stock.n
  n(biol)[,1] <- stock.n(brp)

  # TEST: abs(range(c(tb(biol)[,1]) / (B0 * dep)))

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
#' NULL

simulator <- function(biol, fisheries, history, B0, h, dep=0,
  iter=dims(biol)$iter, sigmaR=0, rho=0, B0change=NULL, invalk=NULL,
  deviances=ar1rlnorm(rho=rho, years=dimnames(biol)$year,
    meanlog=0, sdlog=sigmaR),
  minfbar=dims(biol)$min, maxfbar=dims(biol)$max) {

  # DIMS
  nage <- dims(biol)$age
  nyr <- dims(biol)$year
  nfs <- length(fisheries)
  its <- length(c(B0))
  bls <- split(seq(its), ceiling(seq_along(seq(its)) / 250))

  # CHECK match history and first year with N
  
  # SET progresssor
  p <- progressor(length(bls))

  # PROPAGATE objects, if needed

  if(dims(biol)$iter == 1)
    biols <- lapply(bls, function(x) propagate(biol, length(x)))
  else if(dims(biol)$iter == its)
    biols <- lapply(bls, iter, obj=biol)
  else
    biols <- list(`1`=biol)

  if(dims(fisheries[[1]])$iter == 1)
    fisheries <- lapply(bls, function(x) lapply(fisheries,
      function(y) propagate(y, iter=length(x))))
  else if(dims(fisheries[[1]])$iter == its)
    fisheries <- lapply(bls, iter, obj=fisheries)
  else
    fisheries <- list(`1`=fisheries)

  # LOOP over iter blocks

  sim <- foreach(i=names(bls), .combine=.lcombine,
    .multicombine=TRUE) %dofuture% {

    # SET iters
    it <- bls[[i]]
    ni <- length(it)

    # GET objects by iter group from lists
    bio <- biols[[i]]
    fis <- fisheries[[i]]
    
    # INITIATE N0
    nbio <- initiate(bio, B0=B0[it], h=h[it])

    # COMBINED selectivity for depletion, catch 1 across fisheries
    if (nfs > 1)
    sel <- Reduce("+", lapply(fis, function(x) catch.sel(x[[1]])) *
      lapply(fis, function(x) catch.n(x[[1]]))) /
      Reduce("+", lapply(fis, function(x) catch.n(x[[1]])))
    else
      sel <- catch.sel(fis[[1]][[1]])

    # RESCALE selex to 1
    sel <- sel %/% apply(sel, 2:6, max)

    # DEPLETE to dep level by iter
    nbio <- deplete(nbio, sel=sel, dep=dep[it], minfbar=minfbar, 
      maxfbar=maxfbar)

    # ADD initial age devs, no bias correction needed
    n(nbio)[-nage, 1] <- n(nbio)[-nage, 1] * rlnorm(nage - 1, 0, sigmaR)

    # MAP refpts & keep target F
    rps <- remap(nbio@refpts, MSY=c("msy", "yield"))
    targetf <- c(nbio@refpts["target", "harvest"])

    # ALTER SRR if B0(K) changes in time
  
    if(!is.null(B0change)) {

      # EXPAND FLPar
      pas <- params(sr(nbio))
      pay <- FLPar(NA, dimnames=list(params=c("s", "R0", "v"),
       year=dimnames(nbio)$year, iter=it))

      # ASSIGN first year,
      pay[, 1,]<- pas
      # steepness
      pay['s', ,] <- pas['s',]
      # SET B0 and R0 trends
      pay['v', ,] <- apply(pas$v, 2, '*', c(B0change))
      pay['R0', ,] <- apply(pas$R0, 2, '*', c(B0change))

      params(sr(nbio)) <- pay
  
      # RESCALE refpts by year
      rpy <- FLPar(NA, dimnames=list(param=dimnames(rps)$param, 
        year=dimnames(nbio)$year, iter=it))
      rpy[,1,] <- rps

      # F is constant
      rpy[c('FMSY'),]  <- rps[c('FMSY'),] 

      # BIOMASS is scaled
      for(i in c('SBMSY', 'BMSY', 'B0', 'SB0', 'MSY'))
        rpy[i,] <- apply(rps[i, ], 2, '*', c(B0change))
    }

    # SET effort to match F target
    for(i in seq(fis)) {
      effort(fis[[i]])[] <- abs(targetf)
    }

    # CONVERT history
    # TODO: DEAL w/ iters in history
    if(!is(history, "fwdControl")) {
      history <- as(history, "fwdControl")
    }
    
    # FWD w/history
    res <- suppressWarnings(fwd(nbio, fis, control=iter(history, it),
      deviances=iter(deviances, it), effort_max=1e6))

    # LEN samples
    if(!is.null(invalk)) {
      cafs <- lapply(res$fisheries, function(x) catch.n(x[[1]]) + 1e-6)
      res$lengths <- lapply(cafs, lenSamples, invALK=invalk, n=250)
    }

    # OUTPUT
    res$priors <- data.table(B0=B0, dep=dep, h=h)
    deviances(res$biol) <- iter(deviances, it)
    res$deviances <- iter(deviances, it)
    res$refpts <- rps

    # UPDATE progressr report
    p()

    return(res)
  }

  return(sim)
}

# }}}

# .bcombine including refpts attr {{{
.bcombine <- function(x, y, ...) {

  args <- c(list(x, y), list(...))
  res <- do.call(combine, args)

  attr(res, "refpts") <- Reduce(combine, lapply(args, slot, "refpts"))

  return(res)
}
# }}}

# .lcombine simulator list {{{
.lcombine <- function(x, y, ...) {

  args <- c(list(x, y), list(...))

  out <- list(biol=do.call(.bcombine, lapply(args, '[[', 'biol')),
    fisheries=do.call(combine, lapply(args, '[[', 'fisheries')),
    priors=do.call(rbind, lapply(args, '[[', 'priors')),
    deviances=do.call(combine, lapply(args, '[[', 'deviances')),
    refpts=do.call(combine, lapply(args, '[[', 'refpts'))
  )

  if("lengths" %in% names(x))
    out$lengths <- do.call(combine, lapply(args, '[[', 'lengths'))

  return(out)
}
# }}}
