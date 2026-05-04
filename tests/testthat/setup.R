# setup.R - DESC
# /home/mosqu003/Active/mse_FLR/mse/tests/testthat/setup.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# LOAD pkg datasets
load(system.file("data/plesim.rda", package = "mse"))
load(system.file("data/statistics.rda", package = "mse"))

# LOAD inputs.R objects
load("inputs.rda")

# ncoresrun, number of cores used in a run from tracking$pid
ncoresrun <- function(x) {
  length(unique(unlist(lapply(x, function(x)
    tracking(x)[metric == 'pid', unique(data)]))))
}
