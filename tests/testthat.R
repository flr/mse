library(testthat)
library(mse)

# ncoresrun, number of cores used in a run from tracking$pid
ncoresrun <- function(x) {
  length(unique(unlist(lapply(x, function(x)
    tracking(x)[metric == 'pid', unique(data)]))))
}

test_check("mse")
