# cjm.R - DESC
# /cjm.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(FLjjm)

# --- CJM 2 stocks

# READ FLomBF, 2 stocks

om <- readFLomjjm("mod1.00_2stk", path="cjm_2stk")

# READ FLoem

oem <- readFLoemjjm(name="mod1.00_2stk", path="cjm_2stk")

# EXPAND to future

om <- fwdWindow(om, end=2035)

# SETUP future stocks

observations(oem, "stk") <- lapply(observations(oem, "stk"), stf, end=2035)

# SETUP future indices

observations(oem, "idx")[c(2, 3, 6)] <- 
  lapply(observations(oem, "idx")[c(2, 3, 6)], fwdWindow, end=2035)

observations(oem, "idx")[[8]] <-
  fwdWindow(observations(oem, "idx")[[8]], end=2034)
observations(oem, "idx")[[8]][,"2019"] <-
  observations(oem, "idx")[[8]][,"2018"]

# DISABLE projections

observations(oem)$ctl$n_proj_yrs <- 0

# SAVE

save(om, oem, file="../data/cjm.RData", compress="xz")
