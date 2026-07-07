# test-FLmses.R - Unit tests for FLmses class and methods
# flr/mse/tests/testthat/test-FLmses.R

# Copyright (c) WMR, 2026.
# Author: Iago Mosqueira (WUR)
#
# Distributed under the terms of the EUPL-1.2

# LOAD inputs
load("inputs.rda", verbose=FALSE)

# -- TEST: FLmses constructor {{{

context("FLmses: Constructor")

test_that("FLmses() creates empty object", {
  x <- FLmses()
  
  expect_s4_class(x, "FLmses")
  expect_equal(length(x), 0)
})

test_that("FLmses() from list of FLmse objects", {
  # Use FLmse from inputs.rda
  x <- FLmses(run1=flmse, run2=flmse)
  
  expect_s4_class(x, "FLmses")
  expect_equal(length(x), 2)
  expect_equal(names(x), c("run1", "run2"))
})

test_that("FLmses() from existing FLmses object", {
  # Use FLmses from inputs.rda
  expect_s4_class(flmses, "FLmses")
  expect_gte(length(flmses), 1)
})

# }}}

# -- TEST: FLmses accessors {{{

context("FLmses: Accessors")

test_that("performance() accessor works", {
  # Use existing FLmses from inputs.rda
  x <- flmses
  
  # Get performance
  perf <- performance(x)
  
  expect_s3_class(perf, "data.table")
})

test_that("tracking() accessor for FLmses works", {
  # Use existing FLmses from inputs.rda
  x <- flmses
  
  # Get tracking
  track <- tracking(x)
  
  expect_s3_class(track, "data.table")
  expect_true("run" %in% colnames(track))
})

test_that("Subsetting FLmses works", {
  # Create FLmses with 3 elements
  x <- FLmses(run1=flmse, run2=flmse, run3=flmse)
  
  # Subset by index with [
  y <- x[1]
  expect_s4_class(y, "FLmses")
  expect_equal(length(y), 1)
  expect_equal(names(y), "run1")
  
  # Subset by name with [
  z <- x["run2"]
  expect_s4_class(z, "FLmses")
  expect_equal(length(z), 1)
  expect_equal(names(z), "run2")
  
  # Subset multiple elements
  w <- x[c(1, 3)]
  expect_s4_class(w, "FLmses")
  expect_equal(length(w), 2)
  expect_equal(names(w), c("run1", "run3"))
  
  # Subset by index with [[
  y2 <- x[[1]]
  expect_s4_class(y2, "FLmse")
  
  # Subset by name with [[
  z2 <- x[["run2"]]
  expect_s4_class(z2, "FLmse")
  
  # Check length
  expect_equal(length(x), 3)
  expect_equal(names(x), c("run1", "run2", "run3"))
})

# }}}

# -- TEST: FLmses combination {{{

context("FLmses: Combination")

test_that("c() combines FLmses objects using inputs.rda", {
  # Use existing FLmses from inputs.rda
  x <- FLmses(run1=flmse)
  y <- FLmses(run2=flmse)
  
  # Combine
  z <- c(x, y)
  
  expect_s4_class(z, "FLmses")
  expect_equal(length(z), 2)
  expect_equal(names(z), c("run1", "run2"))
})

test_that("$ assignment works for FLmses", {
  # Use existing FLmse from inputs.rda
  x <- FLmses(run1=flmse)
  
  # Add via $
  x$run2 <- flmse
  
  expect_equal(length(x), 2)
  expect_equal(names(x), c("run1", "run2"))
})

# }}}

# -- TEST: c() method with various object types {{{

context("FLmses: c() method with various object types")

test_that("c(FLmses, FLmses) combines two FLmses objects", {
  # Create second FLmses
  flmses2 <- flmses
  
  # Combine
  result <- c(flmses, flmses2)
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), length(flmses) + length(flmses2))
})

test_that("c(FLmses, FLmse) combines FLmses with individual FLmse", {
  # Combine FLmses with individual FLmse
  result <- c(flmses, newrun=flmse)
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), length(flmses) + 1)
  expect_true("newrun" %in% names(result))
})

test_that("c(FLmses, FLo) combines FLmses with FLom object", {
  # Combine FLmses with FLom
  result <- c(flmses, om_run=flom)
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), length(flmses) + 1)
  expect_true("om_run" %in% names(result))
  
  # Check that the FLom was converted to FLmse
  expect_s4_class(result[["om_run"]], "FLmse")
})

test_that("c(FLmses, FLo) preserves performance slots", {
  # Get performance from original FLmses
  perf_orig <- performance(flmses)
  
  # Combine with FLom
  result <- c(flmses, om_run=flom)
  
  # Get performance from combined object
  perf_combined <- performance(result)
  
  expect_s3_class(perf_combined, "data.table")
  
  # Performance should include rows from both original and new element
  if(nrow(perf_orig) > 0) {
    expect_gte(nrow(perf_combined), nrow(perf_orig))
  }
})

test_that("c() with multiple FLo objects", {
  # Combine FLmses with multiple FLom objects
  result <- c(flmses, om1=flom, om2=flom)
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), length(flmses) + 2)
  expect_true(all(c("om1", "om2") %in% names(result)))
})

test_that("c() with mixed FLmse and FLo objects", {
  # Combine with both FLmse and FLom
  result <- c(flmses, mse_run=flmse, om_run=flom)
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), length(flmses) + 2)
  expect_true(all(c("mse_run", "om_run") %in% names(result)))
  
  # Both should be FLmse objects in the result
  expect_s4_class(result[["mse_run"]], "FLmse")
  expect_s4_class(result[["om_run"]], "FLmse")
})

test_that("c() preserves names of original objects", {
  orig_names <- names(flmses)
  result <- c(flmses, new=flmse)
  
  # Original names should be preserved
  expect_true(all(orig_names %in% names(result)))
})

test_that("c() handles empty FLmses", {
  empty <- FLmses()
  result <- c(empty, run1=flmse)
  
  expect_s4_class(result, "FLmses")
  expect_equal(length(result), 1)
  expect_equal(names(result), "run1")
})

# }}}
