# test-FLmses.R - Unit tests for FLmses class and methods
# flr/mse/tests/testthat/test-FLmses.R

# Copyright (c) WMR, 2026.
# Author: Iago Mosqueira (WUR)
#
# Distributed under the terms of the EUPL-1.2

# LOAD plesim data
data(plesim)

# -- TEST: FLmses constructor {{{

context("FLmses: Constructor")

test_that("FLmses() creates empty object", {
  x <- FLmses()
  
  expect_s4_class(x, "FLmses")
  expect_equal(length(x), 0)
})

test_that("FLmses() from list of FLmse objects", {
  # Create a simple FLmse for testing
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  # Run mp
  run1 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  run2 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  
  # Create FLmses
  x <- FLmses(run1=run1, run2=run2)
  
  expect_s4_class(x, "FLmses")
  expect_equal(length(x), 2)
  expect_equal(names(x), c("run1", "run2"))
})

# }}}

# -- TEST: FLmses accessors {{{

context("FLmses: Accessors")

test_that("performance() accessor works", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  run1 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  run2 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  
  x <- FLmses(run1=run1, run2=run2)
  
  # Get performance
  perf <- performance(x)
  
  expect_s3_class(perf, "data.table")
})

test_that("tracking() accessor for FLmses works", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  run1 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  run2 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  
  x <- FLmses(run1=run1, run2=run2)
  
  # Get tracking
  track <- tracking(x)
  
  expect_s3_class(track, "data.table")
  expect_true("run" %in% colnames(track))
})

test_that("Subsetting FLmses works", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  run1 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  run2 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  run3 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  
  x <- FLmses(run1=run1, run2=run2, run3=run3)
  
  # Subset by name
  y <- x["run1"]
  expect_equal(length(y), 1)
  expect_equal(names(y), "run1")
  
  # Subset by index
  z <- x[c(1, 3)]
  expect_equal(length(z), 2)
  expect_equal(names(z), c("run1", "run3"))
})

# }}}

# -- TEST: FLmses combination {{{

context("FLmses: Combination")

test_that("c() combines FLmses objects", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  run1 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  run2 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  
  x <- FLmses(run1=run1)
  y <- FLmses(run2=run2)
  
  # Combine
  z <- c(x, y)
  
  expect_s4_class(z, "FLmses")
  expect_equal(length(z), 2)
  expect_equal(names(z), c("run1", "run2"))
})

test_that("$ assignment works for FLmses", {
  control <- mpCtrl(list(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=hockeystick.hcr, 
      args=list(lim=0, trigger=14000, target=0.18))))
  
  run1 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  run2 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2024))
  
  x <- FLmses(run1=run1)
  
  # Add via $
  x$run2 <- run2
  
  expect_equal(length(x), 2)
  expect_equal(names(x), c("run1", "run2"))
})

# }}}
