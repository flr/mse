# cjm.R - DESC
# /home/mosqu003/Projects/FLR/code/mse/mse/data-raw/cjm.R

# Copyright (c) WMR, 2025.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# XX {{{
# }}}

library(FLjjm)

# H1

om <- readFLomjjm('h1_1.07', 'single_stock')

oem <- readFLoemjjm('h1_1.07', path = 'single_stock', method=sampling.oem,
  args=list(byfishery=FALSE)

observations(oem)$dat <- observations(oem)$ctl <- NULL

save(om, oem, file='../data/cjm_one.rda', compress='xz')

# H2

om <- readFLomjjm('h2_1.07', 'two_stocks')

oem <- readFLoemjjm('h2_1.07', path = 'two_stocks', method=sampling.oem,
  args=list(byfishery=FALSE))

observations(oem)$dat <- observations(oem)$ctl <- NULL

save(om, oem, file='../data/cjm_two.rda', compress='xz')
