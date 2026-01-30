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
#' ctrl <- mpCtrl(est=mseCtrl(method=cpue.ind, args=list(index=2)),
#'   hcr=mseCtrl(method=hockeystick.hcr, args=list(metric="wmean",
#'   trigger=2000, output="catch", target=1.25e5)))
#'
#' # Run the MP
#'  run <- mp(om, oem, control=ctrl, args=list(iy=2025, fy=2035))
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

cpues.ind <- function(stk, idx, nyears=5, index=1, args, tracking) {

  # CALL cpue.ind
  mets <- lapply(setNames(index, nm=names(idx[index])), function(x)
    cpue.ind(stk, idx, nyears=nyears, index=x, args, tracking))

  ind <- lapply(mets, function(x) x$ind$index)
  
  return(list(stk=stk, ind=ind, tracking=mets[[1]]$tracking))

} # }}}
