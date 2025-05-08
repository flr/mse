# test-performance.R - DESC
# /home/mosqu003/Projects/FLR/code/mse/mse/tests/testthat/test-performance.R

# Copyright (c) WMR, 2025.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# --- performance(FLom)

data(sol274)
x <- window(om, end=2021)

data(statistics)
stats <- statistics[c("SB", "SBMSY", "PSBMSY", "F", "C", "IACC")]

# - performance(FLom, statistics, metrics, refpts, years, probs, om, type, run, mp)

per0 <- performance(x)
per1 <- performance(x, statistics=stats)
per <- performance(x, statistics=stats, om='sol274')
per <- performance(x, statistics=stats, om='sol274', type='none')
per <- performance(x, years=1958:2021)
per <- performance(x, statistics=stats, metrics=)
per <- performance(x, statistics=stats)



# - performance(FLom, statistics, metrics, years, probs, om, type, mp, run, ...)

# - performance(FLom, statistics, metrics, years, probs, om, type, mp, run, ...)

# - performance(FLom, statistics, metrics, years, probs, om, type, mp, run, ...)


#  --- performance(FLombf)
