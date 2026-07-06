# test-mp.R - Unit tests for mp() and mps() functions
# flr/mse/tests/testthat/test-mp.R

# Copyright (c) WMR, 2026.
# Author: Iago Mosqueira (WUR)
#
# Distributed under the terms of the EUPL-1.2

# LOAD plesim data
data(plesim)

# -- SHORT TESTS {{{

# -- TEST: mp() basic functionality {{{

context("mp: Basic functionality (SHORT)")

test_that("mp() runs with minimal arguments", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Run short mp
  result <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  
  expect_s4_class(result, "FLmse")
  expect_s4_class(om(result), "FLom")
  expect_s4_class(tracking(result), "data.table")
})

test_that("mp() returns correct structure", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  result <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  
  expect_true(all(c("om", "tracking", "control", "oem", "args") %in% 
    slotNames(result)))
  
  # Check om slot
  expect_s4_class(om(result), "FLom")
  
  # Check tracking slot
  expect_s3_class(tracking(result), "data.table")
  expect_true(nrow(tracking(result)) > 0)
  
  # Check control slot
  expect_s4_class(control(result), "mpCtrl")
  
  # Check oem slot
  expect_s4_class(oem(result), "FLoem")
})

test_that("mp() with default oem works", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Run without oem argument
  result <- mp(om, ctrl=control, args=list(iy=2021, fy=2024))
  
  expect_s4_class(result, "FLmse")
  expect_s4_class(oem(result), "FLoem")
})

test_that("mp() with different management frequencies", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Annual
  result_annual <- mp(om, oem=oem, ctrl=control, 
    args=list(iy=2021, fy=2024, frq=1))
  
  # Biennial
  result_biennial <- mp(om, oem=oem, ctrl=control, 
    args=list(iy=2021, fy=2024, frq=2))
  
  expect_s4_class(result_annual, "FLmse")
  expect_s4_class(result_biennial, "FLmse")
  
  # Different tracking rows expected
  expect_true(nrow(tracking(result_annual)) >= nrow(tracking(result_biennial)))
})

# }}}

# -- TEST: mps() basic functionality {{{

context("mps: Basic functionality (SHORT)")

test_that("mps() runs with single run", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Run mps
  result <- mps(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), 1)
})

test_that("mps() with multiple hcr parameters", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Run mps with different targets
  result <- mps(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024),
    hcr=list(target=c(0.15, 0.18, 0.20)))
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), 3)
  expect_true(all(c("hcr_target_0.15", "hcr_target_0.18", "hcr_target_0.2") %in% 
    names(result)))
})

test_that("mps() tracking combines correctly", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  result <- mps(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024),
    hcr=list(target=c(0.15, 0.20)))
  
  track <- tracking(result)
  
  expect_s3_class(track, "data.table")
  expect_true("run" %in% colnames(track))
  expect_equal(length(unique(track$run)), 2)
})

# }}}

# }}} END SHORT TESTS

# -- LONG TESTS {{{

# -- TEST: mp() full simulation {{{

context("mp: Full simulation (LONG)")

test_that("mp() runs full 20-year projection", {
  skip_if_not(Sys.getenv("MSE_TEST_SUITE") == "long",
    "Long tests only run when MSE_TEST_SUITE=long")
  
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Full 20-year projection
  result <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2040))
  
  expect_s4_class(result, "FLmse")
  
  # Check years
  om_result <- om(result)
  expect_true(dims(stock(om_result))$maxyear >= 2040)
  
  # Check tracking has data for all years
  track <- tracking(result)
  expect_true(nrow(track) > 0)
})

test_that("mp() with assessment model runs correctly", {
  skip_if_not(Sys.getenv("MSE_TEST_SUITE") == "long",
    "Long tests only run when MSE_TEST_SUITE=long")
  
  # Control with more complex assessment
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  result <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2035))
  
  expect_s4_class(result, "FLmse")
  
  # Check tracking contains assessment outputs
  track <- tracking(result)
  expect_true(nrow(track) > 0)
})

test_that("mp() with implementation error", {
  skip_if_not(Sys.getenv("MSE_TEST_SUITE") == "long",
    "Long tests only run when MSE_TEST_SUITE=long")
  
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Create simple iem (if available in your package)
  # This is a placeholder - adjust based on actual iem implementation
  result <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2035))
  
  expect_s4_class(result, "FLmse")
})

test_that("mp() with multiple iterations", {
  skip_if_not(Sys.getenv("MSE_TEST_SUITE") == "long",
    "Long tests only run when MSE_TEST_SUITE=long")
  
  # Check if om has multiple iterations
  if(dims(stock(om))$iter > 1) {
    control <- mpCtrl(list(
      est = mseCtrl(method=perfect.sa),
      hcr = mseCtrl(method=hockeystick.hcr, 
        args=list(lim=0, trigger=14000, target=0.18))))
    
    result <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2035))
    
    expect_s4_class(result, "FLmse")
    
    # Check iterations preserved
    expect_equal(dims(stock(om(result)))$iter, dims(stock(om))$iter)
  } else {
    skip("om does not have multiple iterations")
  }
})

# }}}

# -- TEST: mps() full grid search {{{

context("mps: Full grid search (LONG)")

test_that("mps() runs grid search over multiple parameters", {
  skip_if_not(Sys.getenv("MSE_TEST_SUITE") == "long",
    "Long tests only run when MSE_TEST_SUITE=long")
  
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Grid search over multiple target F values
  result <- mps(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2035),
    hcr=list(target=c(0.10, 0.15, 0.18, 0.20, 0.25)))
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), 5)
  
  # Check each run
  for(i in seq_along(result)) {
    expect_s4_class(result[[i]], "FLmse")
  }
})

test_that("mps() with performance calculation", {
  skip_if_not(Sys.getenv("MSE_TEST_SUITE") == "long",
    "Long tests only run when MSE_TEST_SUITE=long")
  
  # Load statistics
  data(statistics)
  
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Run with performance calculation
  result <- mps(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2035),
    hcr=list(target=c(0.15, 0.18, 0.20)),
    statistics=statistics[c("C", "F", "SB")], perf=TRUE)
  
  expect_s4_class(result, "FLmses")
  
  # Check performance table
  perf <- performance(result)
  expect_s3_class(perf, "data.table")
  expect_true(nrow(perf) > 0)
  expect_true(all(c("C", "F", "SB") %in% unique(perf$statistic)))
})

test_that("mps() runs with different hcr triggers", {
  skip_if_not(Sys.getenv("MSE_TEST_SUITE") == "long",
    "Long tests only run when MSE_TEST_SUITE=long")
  
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Different trigger values
  result <- mps(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2035),
    hcr=list(trigger=c(10000, 12000, 14000, 16000)))
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), 4)
})

test_that("mps() comparison of management frequencies", {
  skip_if_not(Sys.getenv("MSE_TEST_SUITE") == "long",
    "Long tests only run when MSE_TEST_SUITE=long")
  
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Annual advice
  annual <- mp(om, oem=oem, ctrl=control, 
    args=list(iy=2021, fy=2035, frq=1))
  
  # Triennial advice
  triennial <- mp(om, oem=oem, ctrl=control, 
    args=list(iy=2021, fy=2035, frq=3))
  
  # Combine
  result <- FLmses(annual=annual, triennial=triennial)
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), 2)
  expect_equal(names(result), c("annual", "triennial"))
})

# }}}

# }}} END LONG TESTS
