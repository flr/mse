# ind.R - DESC
# mse/R/ind.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# cpue.ind {{{

#' Computes CPUE-based Indicators of changes in Stock Abundance
#'
#' This function computes four abundance indicators from one CPUE or biomass
#' index of abundance: the index itself, an average over a number of years, a
#' weighted mean over those same years and the slope of the trend over the same period.
#'
#' @param stk An object representing the stock returned by the `oem` modules, FLStock.
#' @param idx An FLIndices containing the indices rturned by the `oem` module.
#' @param index An integer or character, specifying the index to use from `idx`. Default is 1.
#' @param nyears An integer, the number of years to consider for the calculations. Default is 5.
#' @param args A list containing dimensionality arguments, passed on by mp().
#' @param tracking An FLQuant used for tracking indicators, intermediate values, and decisions during MP evaluation.
#' @details
#' The weighted average returned in the 'wmean' element is calculated over the last
#' `nyears`. The last year's weight is set as 50%, and the remaining years share the 
#' other 50% proportionally. The 'slope' metric is computed on the log-transformed data.
#'
#' Three elements are added to the `tracking` table:
#' - mean.ind, with the index average
#' - wmean.ind, with the index weighted average
#' - slope.ind, with the index slope
#' @return A list containing 'stk', the input FLStock, 'ind, an FLQuants object
#' containing the computed index metrics, and the 'tracking' table.
#' @examples
#' data(plesim)
#' # MP control with CPUE: catch ~ weighted 4-year mean CPUE
#' ctrl <- mpCtrl(est=mseCtrl(method=cpue.ind, args=list(index=1)),
#'   hcr=mseCtrl(method=hockeystick.hcr, args=list(metric="wmean",
#'   trigger=2000, output="catch", target=1.25e5)))
#'
#' # Run the MP
#' run <- mp(om, oem, control=ctrl, args=list(iy=2025, fy=2035))
#'
#' # Plot results
#' plot(om, run)

cpue.ind <- function(stk, idx, index=1, nyears=5, args, tracking) {

  # ARGS
  ay <- args$ay
  dlag <- args$data_lag
  dyrs <- ac(seq(ay - dlag - (nyears - 1) , length=nyears))

  # SUBSET dyrs
  met <- biomass(idx[[index]])[1, dyrs]

  # 1. AVERAGE index
  imean <- expand(yearMeans(seasonMeans(met)), year=ay - dlag)

  # 2. WEIGHTED average index of last nyears: 0.50 for last year, 0.50 others
  ywts <- c(0.50 * seq(1, nyears - 1) / sum(seq(1, nyears - 1)), 0.50)
  wmean <- expand(seasonMeans(yearSums(tail(met, nyears) * ywts)), year=ay - dlag)

  # 3. SLOPE by iter
  dat <- data.table(as.data.frame(met))
  slope <- dat[, .(data=coef(lm(log(data + 1e-22) ~ year))[2]), by=iter]
  slope <- FLQuant(slope$data, dimnames=dimnames(imean)[-4], units="")

  # OUTPUT
  ind <- FLQuants(index=met, mean=imean, wmean=wmean, slope=slope)

  # TRACK
  track(tracking, "mean.ind", ac(ay)) <- imean
  track(tracking, "wmean.ind", ac(ay)) <- wmean
  track(tracking, "slope.ind", ac(ay)) <- slope

  return(list(stk=stk, ind=ind, tracking=tracking))

}
# }}}

# cpues.ind {{{

#' Compute a CPUE-based indicator for use in empirical harvest control rules
#'
#' @description
#' `cpues.ind` computes a relative abundance indicator from one or more CPUE
#' or survey indices (`idx`), to be used by empirical indicator (`ind`)
#' modules in an `mse` Management Strategy Evaluation loop. Indices are first
#' restricted to a recent window of `nyears`, rescaled by dividing by their
#' mean over a set of reference years (`refyrs`), and then combined across
#' indices, using `weights`, with one of three methods: a weighted rescaled
#' mean (`"mean"`), a weighted mean of standardized z-scores on the log scale
#' (`"zscore"`), or a weighted mean of smoothed, rescaled indices
#' (`"smooth"`). For `combine = "mean"`, the function additionally computes
#' the mean level and log-linear slope of the combined indicator over the
#' recent window.
#'
#' @details
#' `cpues.ind` is designed to be called inside an `mse` indicator (`ind`)
#' module, and expects `args` to contain at least the current assessment
#' year (`ay`) and last data year (`dy`), unpacked internally via
#' `mse::spread`. The mean (and, for `combine = "mean"`, the slope) of the
#' indicator is stored in `tracking` under `"mean.ind"` and `"slope.ind"`.
#'
#' Combination across indices is done by the `weighted.mean` method for
#' `FLQuants`, which by default excludes `NA` values from both the weighted
#' sum and the sum of weights, so indices missing in a given year do not
#' bias the result towards zero or dilute the contribution of the indices
#' that do have data.
#'
#' @param stk An `FLStock` object, returned unmodified in the output list.
#' @param idx An `FLIndices` object with the CPUE or survey indices from
#' which the indicator is computed.
#' @param refyrs Reference years used to rescale (`"mean"`, `"smooth"`) or
#' standardize (`"zscore"`) each index.
#' @param nyears Number of recent years, ending in `dy`, over which the
#' indicator is computed. `numeric`, defaults to 4.
#' @param indices Names of the elements of `idx` to use. Defaults to
#' `names(idx)`, i.e. all indices in `idx`.
#' @param combine Method used to combine the rescaled indices into a single
#' indicator, one of `"mean"`, `"zscore"` or `"smooth"`.
#' @param weights Weights applied to each index, used by all three `combine`
#' methods, passed on to `weighted.mean`. Defaults to equal weights,
#' `numeric` of length `length(indices)`.
#' @param enp.mult Smoothing parameter passed to `smooth_index` as
#' `enp.mult`, only used when `combine = "smooth"`. Defaults to 0.2.
#' @param args A `list` of MSE loop arguments, e.g. `ay` (assessment year)
#' and `dy` (last data year), unpacked using `mse::spread`.
#' @param tracking An `mse` tracking object (`FLQuant`) used to store the
#' computed indicator(s) for later inspection.
#'
#' @return A `list` with three elements:
#' \item{stk}{The input `FLStock`, unchanged.}
#' \item{ind}{An `FLQuants` with the combined indicator(s): `ind`, `mean`
#' and `slope` for `combine = "mean"`; `mean` only for `combine = "zscore"`
#' or `combine = "smooth"`.}
#' \item{tracking}{The updated tracking object.}
#'
#' @author Iago Mosqueira (WMR), FLR Team.
#' @seealso \link[mse]{mp}, \link{weighted.mean}, \link{smooth_index},
#' \link{zscore}
#' @keywords methods
#' @examples
#' \dontrun{
#' data(ple4)
#' data(ple4.indices)
#'
#' args <- list(ay=2017, dy=2017)
#' tracking <- FLQuant()
#'
#' out <- cpues.ind(stk=ple4, idx=ple4.indices, refyrs=2005:2010,
#'   nyears=4, combine="mean", weights=c(1, 2), args=args,
#'   tracking=tracking)
#' }

cpues.ind <- function(stk, idx, refyrs, nyears=4, indices=names(idx),
  combine=c("mean", "zscore", "smooth"), weights=rep(1, length(indices)),
  enp.mult=0.2, args, tracking) {

  # ARGS
  spread(args)
  # FIND start
  start <- dy - nyears + 1
  # SUBSET idx
  idx <- idx[indices]
  
  # CHECK refyrs are available in idx
  lapply(idx, function(i) {
    if(!all(refyrs %in% dimnames(index(i))$year))
      stop(paste0("Reference years 'refyrs' must be present in all indices in 'idx'."))
  })

  # COMBINE
  if (combine == "mean") {

    # DIVIDE by ref mean and TRIM
    inds <- window(FLQuants(lapply(idx, function(i) index(i) %/%
      yearMeans(index(i)[, ac(refyrs)]))), start=start, end=dy)
    
    # WEIGHTED average of rescaled indices, NA-aware
    ind <- FLQuant(weighted.mean(inds, weights), units="")
    
    # COMPUTE mean
    mean <- expand(yearMeans(ind), year=dy)
    
    # COMPUTE slope
    dat <- data.table(as.data.frame(ind))
    slope <- dat[, .(data=coef(lm(log(data + 1e-22) ~ year))[2]), by=iter]
    slope <- FLQuant(slope$data, dimnames=dimnames(mean), units="")
    
    # TRACK
    track(tracking, "mean.ind", year=ay, biol=args$stock) <- mean
    track(tracking, "slope.ind", year=ay, biol=args$stock) <- slope

    # ASSEMBLE ind
    ind <- FLQuants(ind=ind, mean=mean, slope=slope)

  # combine = "zscore"
  } else if(combine == "zscore") {
    
    # ZSCORE standardisation of log(indices)
    inds <- window(FLQuants(lapply(idx, zscore, refyrs=refyrs)),
      start=start, end=dy)
    
    # WEIGHTED mean of non-NA indices, back-transformed
    zmean <- expand(yearMeans(exp(weighted.mean(inds, weights))), year=dy)
    
    # TRACK
    track(tracking, "mean.ind", year=ay, biol=stock) <- zmean
    
    ind <- FLQuants(mean=zmean)

  # combine = "smooth"
  } else if (combine == "smooth") {
    
    inds <- window(FLQuants(lapply(idx, function(i) {
     smi <- smooth_index(index(i), enp.mult=enp.mult)
     smi %/% yearMeans(smi[, refyrs])
    })), start=start, end=dy)

    # WEIGHTED average of smoothed, rescaled indices, NA-aware
    ind <- weighted.mean(inds, weights)
    
    # mean
    smooth <- expand(yearMeans(ind), year=dy)

    # TRACK
    track(tracking, "mean.ind", year=ay, biol=stock) <- smooth
    
    ind <- FLQuants(mean=smooth)
  }
  return(list(stk=stk, ind=ind, tracking=tracking))
}
# }}}

# smooth_index {{{
smooth_index <- function(x, enp.mult=0.2) {

  x[x==0]<-1e3
  
  dat <- data.table(as.data.frame(x, drop=TRUE))
  dat <- dat[!is.na(data)]

  dat[, enptarget:=sum(!is.na(data)) * enp.mult, by=iter]

  dat[, data:=exp(predict(loess(log(data)~year,
    enp.target=unique(enptarget)))), by=iter]

  return(as.FLQuant(dat[, .(year, iter, data, age='all')]))
}
# }}}

# zscore {{{

zscore <- function(i, refyrs=dimnames(i)$year, sample_sd=FALSE,
  years_sd) {
  
  # LOG index
  li <- log(window(index(i), start=min(dims(i)$minyear, min(an(refyrs))),
    end=max(dims(i)$maxyear, max(an(refyrs)))))

  # REF years mean
  rmu <- yearMeans(li[, ac(refyrs)])

  # COMPUTE sd
  if(sample_sd)
    lsd <- sqrt(log(1 + yearMeans((index.var(i) / index(i))^2)))
  else
    # lsd <- sqrt(yearVars(li[, refyrs]))
    lsd <- sqrt(yearVars(li))
  
  return(((li %-% rmu) %/% lsd)[, dimnames(i)$year])
}

# }}}
