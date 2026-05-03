library(testthat)
local_edition(3)

# ncoresrun, number of cores used in a run from tracking$pid
ncoresrun <- function(x) {
  length(unique(unlist(lapply(x, function(x)
    tracking(x)[metric == 'pid', unique(data)]))))
}

# dataset contains both OM (FLom) and OEM (FLoem)
data(plesim)

# Set control: sa and hcr
control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=10000, target=0.18))))

#
load("setup.rda")

#
test_check("mse")
