# ind.R - DESC
# mse/R/ind.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# cpue.ind {{{

#' @examples
#' data(sol274)
#' ind <- cpue.ind(stock(om), FLIndices(CPUE=FLIndexBiomass(index=ssb(om))),
#'   args=list(ay=2000, data_lag=1),
#'   tracking=FLQuant(dimnames=list(metric="ind", year=2000, iter=1:100)))

cpue.ind <- function(stk, idx, nyears=5, ayears=3, index=1, args, tracking) {
  
  # ARGS
  ay <- args$ay
  dlag <- args$data_lag
  
  # SUBSET last nyears from ay - mlag
  met <- index(idx[[index]])[1, ac(seq(ay - dlag - (nyears - 1) ,
    length=nyears))]

  # SLOPE by iter
  dat <- data.table(as.data.frame(met))
  slope <- dat[, .(data=coef(lm(log(data)~year))[2]), by=iter]

  # WEIGHTED average index of last ayears
  mean <- yearSums(tail(met, ayears) * 
   c(0.50 * seq(1, ayears - 1) / sum(seq(1, ayears - 1)), 0.50))
  # LABEL as from last data year
  dimnames(mean) <- list(year=ay - dlag)

  # OUTPUT
  slope <- FLQuant(slope$data, dimnames=dimnames(mean), units="")
  ind <- FLQuants(mean=mean, slope=slope)

  # TRACK
  track(tracking, "mean.ind", ac(ay)) <- mean
  track(tracking, "slope.ind", ac(ay)) <- slope

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
#'  tracking=FLQuant(dimnames=list(year=2018, metric='lend.ind')),
#'  params=FLPar(linf=132, k=0.080, t0=-0.35))
#' #
#' len.ind(ple4, ple4.indices, args=list(ay=2018, data_lag=1),
#'  indicators=c(lbar=lbar, lmean),
#'  tracking=FLQuant(dimnames=list(year=2018, metric='len.ind')),
#'  params=FLPar(linf=132, k=0.080, t0=-0.35))

# TODO: LOOP over metric

len.ind <- function (stk, idx, args, tracking, indicators="lbar", params,
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

  # CONVERT params
  pars <- as(params, "list")
  
  # OBTAIN names from functions
  nms <- unlist(lapply(indicators, function(x)
      if(is(x, "function"))
        find.original.name(x)
      else
        x
    ))

  # SORT OUT names
  if(is.null(names(indicators)))
    names(indicators) <- nms
  else
    names(indicators)[names(indicators) == 0] <- nms[names(indicators) == 0]

  # COMPUTE indicator(s)
  ind <- lapply(setNames(indicators, nm=nms), function(x) {

    # SUBSET indicator arguments in params
    if(is.character(x)) {
      pars <- pars[names(pars) %in% names(formals(get(x)))]
    } else if(is.function(x)) {
      pars <- pars[names(pars) %in% names(formals(x))]
    }

    return(do.call(x, args=c(list(samps), args0, pars)))
  })

  # ADD to tracking on 'ay' as mean across years
  track(tracking, "len.ind", ac(ay)) <- yearMeans(ind[[1]])

  list(stk = stk, ind = FLQuants(ind), tracking = tracking)
}
# }}}

# meta.est {{{

#' @examples
#' data(sol274)
#' library(FLXSA)
#' control <- mpCtrl(list(
#'   est = mseCtrl(method=meta.est, args=list(
#'   list(method='len.ind', indicators=c('lmean', 'lbar'),
#'     params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2, nyears=5),
#'   list(method='xsa.sa'))),
#' hcr = mseCtrl(method=meta.hcr,
#'    args=list(list(method="trend.hcr", k1=1, k2=2, metric="lmean"),
#'    list(method="trend.hcr", k1=2, k2=3, metric="lbar")))))
#' run <- mp(om, oem=oem, ctrl=control, args=list(iy=2020, fy=2023))
#' #
#' control <- mpCtrl(list(
#'   est = mseCtrl(method=meta.est, args=list(
#'   list(method='len.ind', indicators=c('lmean', 'lbar'),
#'     params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2, nyears=5),
#'   list(method='cpue.ind'))),
#' hcr = mseCtrl(method=meta.hcr,
#'    args=list(list(method="trend.hcr", k1=1, k2=2, metric="lmean"),
#'    list(method="cpue.hcr", k1=2, k2=3, k3=1, k4=1)))))
#' run <- mp(om, oem=oem, ctrl=control, args=list(iy=2020, fy=2023))

meta.est <- function(stk, idx, ..., args, tracking) {
 
  # GET est args list
  eargs <- list(...)
  
  # CHECK eargs have 'method'
  if(!all(unlist(lapply(eargs, function(x) "method" %in% names(x)))))
    stop("args must contain list with an element called 'method'")
  
  # CHECK list has list w/ method + matching args
  if(!all(unlist(lapply(eargs, function(x) all(names(x)[!grepl("method",
    names(x))] %in% names(formals(x$method)))))))
    stop("elements in each args list must match arguments in hcr function")
  
  # APPLY each estimator
  ests <- lapply(eargs, function(x)
    do.call(x$method, c(list(stk=stk, idx=idx, args=args, tracking=tracking),
      x[!grepl("method", names(x))]))
  )

  # MERGE ind
  ind <- FLQuants(Reduce("c", lapply(ests, "[[", "ind")))

  # RETURN a single stk (?)
  istks <- which(unlist(lapply(ests, function(x) !"ind" %in% names(x))))

  # ONE method without ind ?
  if(length(istks) == 1)
    stk <- ests[[istks]]$stk
  # MORE than one (?)
  else if(length(istks) > 1)
    stop("")

  # MERGE tracking
  tracking <- Reduce(merge, lapply(ests, function(x) x$tracking))

  return(list(stk=stk, ind=ind, tracking=tracking))
}

# }}}

# TODO: REMOVE

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
