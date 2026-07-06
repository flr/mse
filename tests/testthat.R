library(testthat)
library(mse)

# Set default to short tests for R CMD check
if (Sys.getenv("MSE_TEST_SUITE") == "") {
  Sys.setenv(MSE_TEST_SUITE = "short")
}

test_check("mse")
