# test-performance.R - DESC
# /home/mosqu003/Projects/FLR/code/mse/mse/tests/testthat/test-performance.R

# Copyright (c) WMR, 2025.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(plesim)

# LOAD statistics
data(statistics)

# GET variables used in statistics
vars_stats <- unique(unlist(lapply(statistics, function(x) all.vars(x[[1]]))))

# GET variables available in om refpts and metrics
x <- c(dimnames(refpts(om))$params, names(metrics(om)))

# IDENTIFY missing variables
lapply(setNames(nm=vars_stats[!vars_stats %in% x]), exists)

# ADD if needed
refpts(om)$Ftarget <- refpts(om)$FMSY
refpts(om)$SBlim <- refpts(om)$SB0 * 0.15
refpts(om)$MSY <- refpts(om)$SBMSY * refpts(om)$FMSY * 0.50
# DEBUG: refpts(om)['MSY',] <- 1280


# --- FLom

years <- list(one=1990, many=1990:1995, unnamed=list(1990:1995),
  named=list(S=1990:1995))

tes <- lapply(years, function(yr) {
  performance(om, statistics=statistics, years=yr, run='A')
})

tes$many[statistic=='PSBlim', data, by=year]

tes$many[, .N, by=name]

# ANY NA?
eqpect_equal(sum(unlist(lapply(tes, function(x) x[, sum(is.na(data))]))), 0)

# MEAN inside performance same as outside?

tesmany_mean <- tes$many[, .(data=mean(data), year=1995), by=.(statistic, iter, name, desc, om, run, mp)][, .(statistic, year, data, iter, name, desc, om, run, mp)]
setkey(tesmany_mean, statistic)

expect_identical(tesmany_mean[statistic == "green"], tes$unnamed[statistic == "green"])
