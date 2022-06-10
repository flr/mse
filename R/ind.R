# ind.R - DESC
# /ind.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# mlc.ind {{{
mlc.ind <- function (stk, idx, args, vbPars=c(linf=120, k=0.2, t0=0), ...) {
  
  args0 <- list(...)
  tracking <- args0$tracking

	vbObj <- FLa4a:::a4aGr(
	  grMod=~linf*(1 - exp(-k * (t - t0))),      
	  grInvMod=~t0 - 1/k * log(1 - len / linf),      
	  params=FLPar(linf=vbPars["linf"], k=vbPars["k"], t0=vbPars["t0"],
      units=c('cm','year-1','year')))
	
  flq <- flc <- catch.n(stk)
	flq[] <- predict(vbObj, t=range(stk)["min"]:range(stk)["max"] + 0.5)
	flq <- quantSums(flc * flq) / quantSums(flc)
  
  ind <- FLQuants(mlc=flq)
  
  list(stk = stk, ind = ind, tracking = tracking)
}
# }}}

# cpue.ind {{{

#' @examples
#' data(ple4om)
#' cpue.ind(stock(om), FLIndices(CPUE=FLIndexBiomass(index=ssb(om))),
#'   args=list(ay=2000, data_lag=1), tracking=FLQuant())

cpue.ind <- function(stk, idx, nyears=5, ayears=3, index=1, args, tracking) {
  
  # ARGS
  ay <- args$ay
  dlag <- args$data_lag
  
  # SUBSET last nyears from ay - mlag
  ind <- index(idx[[index]])[1, ac(seq(ay - dlag - (nyears - 1) ,
    length=nyears))]

  # SLOPE by iter
  dat <- data.table(as.data.frame(ind))
  slope <- dat[, .(slope=coef(lm(log(data)~year))[2]), by=iter]

  # WEIGHTED average index of last ayears
  mind <- yearSums(tail(ind, ayears) * 
   c(0.50 * seq(1, ayears - 1) / sum(seq(1, ayears - 1)), 0.50))
  # LABEL as from last data year
  dimnames(mind) <- list(year=ay - dlag)

  # OUTPUT
  slop <- FLQuant(slope$slope, dimnames=dimnames(mind), units="")
  ind <- FLQuants(mean=mind, slope=slop)

  # TRACK
  track(tracking, "cpue.ind", ac(args$ay)) <- mind
  track(tracking, "slope.ind", ac(args$ay)) <- slop

  list(stk=stk, ind=ind, tracking=tracking)
} # }}}

# len.ind {{{

# TODO

# - SD vs. age, does it increase?
# sd = len * cv
# TODO: arg for slot
# 1. lenSamples(metric(oem))
# 2. metric(lenSamples(perfect.oem), selex)

#' @examples
#' data(ple4)
#' data(ple4.indices)
#' len.ind(ple4, ple4.indices, args=list(ay=2018, data_lag=1),
#'  tracking=FLQuant(), params=FLPar(linf=132, k=0.080, t0=-0.35))

len.ind <- function (stk, idx, args, tracking, indicator="mlc", params,
  nyears=3, cv=0.1, lmax=1.25, bin=1, n=500,
  metric=function(stk) catch.n(stk), ...) {

  # EXTRACT args
  ay <- args$ay
  data_lag <- args$data_lag
  args0 <- list(...)
  
  # COMPUTE inverse ALK (cv, lmax, bin)
  ialk <- invALK(params, age=seq(dims(stk)$min, dims(stk)$max),
    cv=cv, lmax=lmax, bin=bin)

  # GENERATE length samples from metric
  input <- do.call(metric, list(stk=stk, idx=idx)[names(formals(metric))])
  samps <- lenSamples(window(input, start=ay - data_lag - nyears + 1,
    end=ay - data_lag), ialk, n=n)

  # CALL indicator
  ind <- lapply(indicator, do.call, args=c(list(samps), args0))
  names(ind) <- indicator

  # TODO: ADD to tracking
  track(tracking, "len.ind", ac(ay)) <- ind[[1]][, ac(ay)]

  list(stk = stk, ind = FLQuants(ind), tracking = tracking)
}
# }}}
