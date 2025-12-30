# ind.R - DESC
# mse/R/ind.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# cpue.ind {{{

cpue.ind <- function(stk, idx, index=1, nyears=5, mean=yearMeans(index(idx)[[index]]),
  sd=sqrt(yearVars(index(idx)[[index]])), args, tracking) {

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

  # 4. EXP zscore
  #score <- expand(yearMeans(seasonMeans(zscore(met, mean=FLQuant(mean),
  #  sd=FLQuant(sd)))), year=ay - dlag)
  
  # OUTPUT
  ind <- FLQuants(index=met, mean=imean, wmean=wmean, slope=slope)#, zscore=score)

  # TRACK
  track(tracking, "mean.ind", ac(ay)) <- mean
  track(tracking, "wmean.ind", ac(ay)) <- wmean
  track(tracking, "slope.ind", ac(ay)) <- slope
  #track(tracking, "zscore.ind", ac(ay)) <- score

  return(list(stk=stk, ind=ind, tracking=tracking, cpue=met))

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
