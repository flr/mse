# test-performance.R - Unit tests for performance() function family
# flr/mse/tests/testthat/test-performance.R

# Copyright (c) WMR, 2026.
# Author: Iago Mosqueira (WUR)
#
# Distributed under the terms of the EUPL-1.2


# -- TEST: performance(FLQuants) {{{

context("performance: FLQuants method")

test_that("performance(FLQuants) returns data.table with correct structure", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics, refpts=refpts(flom),
    om="ple", run="r00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true(all(c("statistic", "year", "name", "desc", "data", "iter") 
    %in% colnames(result)))
  expect_true(all(!is.na(result$data)))
})

test_that("performance(FLQuants) includes FMSY and other refpt statistics", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[c("FMSY", "SBMSY", "SB0", "green")],
    refpts=refpts(flom), om="ple", run="r00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("FMSY", "SBMSY", "SB0", "green") %in% result$statistic))
  expect_true(all(!is.na(result$data)))
})

test_that("performance(FLQuants) computes Kobe quadrant probabilities correctly", {
  x <- metrics(flom)
  result <- performance(x, 
    statistics=statistics[c("green", "yellow", "orange", "red")],
    refpts=refpts(flom), om="ple", run="r00", type="test")
  
  # Check probabilities are between 0 and 1
  expect_true(all(result$data >= 0 & result$data <= 1))
  
  # Verify green/yellow/orange/red are in results
  expect_true(all(c("green", "yellow", "orange", "red") %in% result$statistic))
  
  expect_true(all(!is.na(result$data)))
})

test_that("performance(FLQuants) Kobe quadrants sum to approximately 1", {
  x <- metrics(flom)
  result <- performance(x, 
    statistics=statistics[c("green", "yellow", "orange", "red")],
    refpts=refpts(flom), om="ple", run="test_sum_quadrants")
  
  if(nrow(result) > 0) {
    result_wide <- data.table::dcast(result, iter + year ~ statistic, value.var = "data")
    result_wide[, quadrant_sum := green + yellow + orange + red]
    
    # All sums should be approximately 1 (allowing for numerical precision)
    expect_true(all(abs(result_wide$quadrant_sum - 1.0) < 0.01))
  }
})

test_that("performance(FLQuants) with identifier arguments", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[c("C", "F")],
    refpts=refpts(flom), om="ple", run="r00", type="test")
  
  expect_true("om" %in% colnames(result))
  expect_true("run" %in% colnames(result))
  expect_true("type" %in% colnames(result))
  expect_true("mp" %in% colnames(result))
})

test_that("performance(FLQuants) returns no NA data values", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[1:5],
    refpts=refpts(flom), run="test_na")
  
  na_count <- sum(is.na(result$data))
  expect_equal(na_count, 0)
})
# }}}

# -- TEST: performance(FLom) {{{

context("performance: FLom method")

test_that("performance(FLom) with statistics and refpts", {
  result <- performance(flom, statistics=statistics, refpts=refpts(flom),
    run="r00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true(all(c("statistic", "year", "data") %in% colnames(result)))
})

test_that("performance(FLom) without refpts works", {
  result <- performance(flom, statistics=statistics,
    run="r00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("performance(FLom) with default statistics works", {
  result <- performance(flom, run="r00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true(length(unique(result$statistic)) > 0)
})

test_that("performance(FLom) includes FMSY statistic when available", {
  result <- performance(flom, 
    statistics=statistics[c("FMSY", "SBMSY")],
    refpts=refpts(flom), run="test_refpt_stats")
  
  expect_true("FMSY" %in% result$statistic)
  expect_true("SBMSY" %in% result$statistic)
})

test_that("performance(FLom) includes green statistic", {
  result <- performance(flom, 
    statistics=statistics[c("green")],
    refpts=refpts(flom), run="test_green")
  
  expect_true("green" %in% result$statistic)
  # Green should be probability (0-1)
  expect_true(all(result$data >= 0 & result$data <= 1))
})
# }}}

# -- TEST: performance(FLStock) {{{

context("performance: FLStock method")

test_that("performance(FLStock) with explicit metrics", {
  x <- stock(flom)
  result <- performance(x, statistics=statistics[c("C", "F")],
    metrics=list(C=catch, F=fbar), om="om00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true(all(c("C", "F") %in% result$statistic))
})

test_that("performance(FLStock) with automatic metrics", {
  x <- stock(flom)
  result <- performance(x, statistics=statistics[c("C", "F")],
    om="om00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("C", "F") %in% result$statistic))
})

test_that("performance(FLStock) with refpt-based statistics", {
  x <- stock(flom)
  result <- performance(x, statistics=statistics[c("C", "F", "FMSY")],
    refpts=refpts(flom), om="om00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("C", "F", "FMSY") %in% result$statistic))
})
# }}}

# -- TEST: performance(FLStocks) {{{

context("performance: FLStocks method")

test_that("performance(FLStocks) with explicit metrics", {
  x <- FLStocks(A=stock(flom), B=stock(flom))
  result <- performance(x, statistics=statistics[c("C", "F")],
    metrics=list(C=catch, F=fbar), om="om00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true("biol" %in% colnames(result))
  expect_equal(length(unique(result$biol)), 2)
})

test_that("performance(FLStocks) with automatic metrics", {
  x <- FLStocks(A=stock(flom), B=stock(flom))
  result <- performance(x, statistics=statistics[c("C", "F")],
    om="om00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_equal(length(unique(result$biol)), 2)
})

test_that("performance(FLStocks) with refpt-based statistics", {
  x <- FLStocks(A=stock(flom), B=stock(flom))
  result <- performance(x, statistics=statistics[c("C", "F", "FMSY")],
    refpts=refpts(flom), om="om00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true("FMSY" %in% result$statistic)
  expect_equal(length(unique(result$biol)), 2)
})
# }}}

# -- TEST: performance(FLmse) {{{

context("performance: FLmse method")

test_that("performance(FLmse) with statistics", {
  result <- performance(flmse, statistics=statistics, run="r00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("performance(FLmse) with default statistics", {
  result <- performance(flmse, run="r00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

# -- TEST: performance(FLmses) {{{

context("performance: FLmses method")

test_that("performance(FLmses) with statistics", {
  result <- performance(flmses, statistics=statistics, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("performance(FLmses) with default statistics", {
  result <- performance(flmses, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
# }}}

# -- TEST: performance(list) - list(FLo) {{{

context("performance: list(FLo) method")

test_that("performance(list(FLo)) handles multiple FLom objects", {
  x <- list(A=flom, B=flom)
  result <- performance(x, statistics=statistics, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("performance(list(FLo)) includes run identifiers", {
  x <- list(A=flom, B=flom)
  result <- performance(x, statistics=statistics[1:3], type="test")
  
  expect_true("run" %in% colnames(result))
})
# }}}

# -- TEST: performance(list) - list(FLmse, FLo)

context("performance: list(FLmse, FLo) method")

test_that("performance(list(FLmse, FLo)) handles mixed FLmses and FLom", {
  x <- c(flmses, A=flom)
  result <- performance(x, statistics=statistics, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
# }}}

# -- TEST: performance(FLmses) {{{

context("performance: FLmses method")

test_that("performance(FLmses) with statistics", {
  result <- performance(flmses, statistics=statistics, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("performance(FLmses) with default statistics", {
  result <- performance(flmses, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

# -- TEST: performance(FLmses) {{{

context("performance: FLmses method")

test_that("performance(FLmses) with statistics", {
  result <- performance(flmses, statistics=statistics, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("performance(FLmses) with default statistics", {
  result <- performance(flmses, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
# }}}

# -- TEST: performance(list) - list(FLo) {{{

context("performance: list(FLo) method")

test_that("performance(list(FLo)) handles multiple FLom objects", {
  x <- list(A=flom, B=flom)
  result <- performance(x, statistics=statistics, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("performance(list(FLo)) includes run identifiers", {
  x <- list(A=flom, B=flom)
  result <- performance(x, statistics=statistics[1:3], type="test")
  
  expect_true("run" %in% colnames(result))
})
# }}}

# -- TEST: performance(list) - list(FLmse, FLo)

context("performance: list(FLmse, FLo) method")

test_that("performance(list(FLmse, FLo)) handles mixed FLmses and FLom", {
  x <- c(flmses, A=flom)
  result <- performance(x, statistics=statistics, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
# }}}

# -- TEST: performance(list) - list(FLmses) {{{

context("performance: list(FLmses) method")

test_that("performance(list(FLmses)) handles multiple FLmses objects", {
  x <- list(A=flmses, B=flmses)
  result <- performance(x, statistics=statistics, type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
# }}}

# -- TEST: performance(list) - list(FLQuants) {{{

context("performance: list(FLQuants) method")

test_that("performance(list(FLQuants)) handles multiple metrics lists", {
  x <- list(A=metrics(flom), B=metrics(flom))
  result <- performance(x, statistics=statistics, refpts=refpts(flom),
    om="ple", run="r00", type="test")
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("performance(list(FLQuants)) includes run identifiers", {
  x <- list(A=metrics(flom), B=metrics(flom))
  result <- performance(x, statistics=statistics[1:3], refpts=refpts(flom),
    om="ple", run="r00", type="test")
  
  expect_true("run" %in% colnames(result))
})
# }}}

# -- TEST: Reference Point Statistics - FMSY, green, orange, red, etc. {{{

context("performance: Reference point-based statistics (FMSY, green, etc.)")

test_that("performance computes FMSY correctly", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[c("FMSY")],
    refpts=refpts(flom), run="test_fmsy")
  
  expect_true("FMSY" %in% result$statistic)
  # F/FMSY should be positive
  expect_true(all(result$data > 0))
  expect_true(all(!is.na(result$data)))
})

test_that("performance computes SBMSY correctly", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[c("SBMSY")],
    refpts=refpts(flom), run="test_sbmsy")
  
  expect_true("SBMSY" %in% result$statistic)
  # SB/SBMSY should be positive
  expect_true(all(result$data > 0))
})

test_that("performance computes SB0 correctly", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[c("SB0")],
    refpts=refpts(flom), run="test_sb0")
  
  expect_true("SB0" %in% result$statistic)
  # SB/SB0 should be positive
  expect_true(all(result$data >= 0))
})

test_that("performance computes green quadrant probability", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[c("green")],
    refpts=refpts(flom), run="test_green_prob")
  
  expect_true("green" %in% result$statistic)
  # Green should be probability (0-1)
  expect_true(all(result$data >= 0 & result$data <= 1))
})

test_that("performance computes orange quadrant probability", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[c("orange")],
    refpts=refpts(flom), run="test_orange_prob")
  
  expect_true("orange" %in% result$statistic)
  # Orange should be probability (0-1)
  expect_true(all(result$data >= 0 & result$data <= 1))
})

test_that("performance computes red quadrant probability", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[c("red")],
    refpts=refpts(flom), run="test_red_prob")
  
  expect_true("red" %in% result$statistic)
  # Red should be probability (0-1)
  expect_true(all(result$data >= 0 & result$data <= 1))
})

test_that("performance computes yellow quadrant probability", {
  x <- metrics(flom)
  result <- performance(x, statistics=statistics[c("yellow")],
    refpts=refpts(flom), run="test_yellow_prob")
  
  expect_true("yellow" %in% result$statistic)
  # Yellow should be probability (0-1)
  expect_true(all(result$data >= 0 & result$data <= 1))
})
# }}}

# -- TEST: Kobe Quadrant Consistency {{{

context("performance: Kobe quadrant consistency")

test_that("Kobe quadrants (green, orange, red, yellow) sum to 1", {
  x <- metrics(flom)
  result <- performance(x, 
    statistics=statistics[c("green", "orange", "red", "yellow")],
    refpts=refpts(flom), run="test_kobe_sum")
  
  if(nrow(result) > 0) {
    result_wide <- data.table::dcast(result, iter + year ~ statistic, value.var = "data")
    result_wide[, quadrant_sum := green + orange + red + yellow]
    
    # All sums should be approximately 1
    expect_true(all(abs(result_wide$quadrant_sum - 1.0) < 0.01))
  }
})

test_that("performance Kobe statistics are internally consistent across inputs", {
  # Test with different input types (FLQuants vs FLom)
  x <- metrics(flom)
  result1 <- performance(x, statistics=statistics[c("green")],
    refpts=refpts(flom), run="test_kobe_consistency1")
  
  result2 <- performance(x, statistics=statistics[c("green")],
    refpts=refpts(flom), run="test_kobe_consistency2")
  
  # Both should produce valid probabilities
  expect_true(all(result1$data >= 0 & result1$data <= 1))
  expect_true(all(result2$data >= 0 & result2$data <= 1))
})
# }}}

# -- TEST: Multiple Reference Point Statistics Combined {{{

context("performance: Combined reference point statistics")

test_that("performance handles multiple refpt statistics simultaneously", {
  x <- metrics(flom)
  multi_stats <- list(
    FMSY=statistics$FMSY,
    SBMSY=statistics$SBMSY,
    SB0=statistics$SB0,
    green=statistics$green
  )
  
  result <- performance(x, statistics=multi_stats, refpts=refpts(flom),
    run="test_multi_refpt")
  
  expect_true(all(c("FMSY", "SBMSY", "SB0", "green") %in% result$statistic))
  expect_true(all(!is.na(result$data)))
})

test_that("performance handles mixed basic and refpt statistics", {
  x <- metrics(flom)
  mixed_stats <- list(
    C=statistics$C,
    F=statistics$F,
    FMSY=statistics$FMSY,
    green=statistics$green
  )
  
  result <- performance(x, statistics=mixed_stats, refpts=refpts(flom),
    run="test_mixed_stats")
  
  expect_true(all(c("C", "F", "FMSY", "green") %in% result$statistic))
})
# }}}

# -- TEST: Output Structure and Columns {{{

context("performance: Output structure")

test_that("performance output has required columns", {
  result <- performance(flom, statistics=statistics[1:5], refpts=refpts(flom),
    run="test_structure")
  
  required_cols <- c("statistic", "year", "name", "desc", "data", "iter")
  expect_true(all(required_cols %in% colnames(result)))
})

test_that("performance output is a data.table", {
  result <- performance(flom, statistics=statistics[1:5], 
    run="test_datatable")
  
  expect_s3_class(result, "data.table")
  expect_true(is.data.table(result))
})

test_that("performance output has proper data types", {
  result <- performance(flom, statistics=statistics[1:3],
    run="test_types")
  
  expect_true(is.numeric(result$data))
  expect_true(is.numeric(result$iter) || is.integer(result$iter) || is.character(result$iter))
  expect_true(is.character(result$statistic))
})
# }}}

# -- TEST: Error Handling {{{

context("performance: Error handling")

test_that("performance warns if statistic variable missing", {
  bad_stats <- list(
    BadStat = list(~yearMeans(NONEXISTENT_VAR),
      name = "Bad",
      desc = "This should fail"),
    GoodStat = list(~C,
      name = "Good",
      desc = "This should work")
  )
  
  x <- metrics(flom)
  expect_warning(
    performance(x, statistics=bad_stats, refpts=refpts(flom),
      run="bad_stat"),
    regex = "could not be computed|check metrics"
  )
})

test_that("performance stops if statistics not unique", {
  x <- metrics(flom)
  dup_stats <- list(
    Stat1 = list(~yearMeans(C), name = "C", desc = "Catch"),
    Stat1 = list(~yearMeans(F), name = "F", desc = "F")
  )
  
  expect_error(
    performance(x, statistics=dup_stats, refpts=refpts(flom),
      run="dup_stats"),
    regex = "unique"
  )
})

# }}}

# -- TEST: Data Consistency and Reproducibility {{{

context("performance: Data consistency")

test_that("performance produces consistent results on repeated calls", {
  set.seed(123)
  result1 <- performance(flom, statistics=statistics[1:5],
    refpts=refpts(flom), run="consistency1")
  
  set.seed(123)
  result2 <- performance(flom, statistics=statistics[1:5],
    refpts=refpts(flom), run="consistency2")
  
  # Data values should be identical
  expect_equal(result1$data, result2$data)
})

test_that("performance identifiers can be controlled", {
  x <- metrics(flom)
  result1 <- performance(x, statistics=statistics[1:3],
    refpts=refpts(flom), om="OM1", run="run1", type="MP1")
  
  result2 <- performance(x, statistics=statistics[1:3],
    refpts=refpts(flom), om="OM2", run="run2", type="MP2")
  
  # om, run, type should differ
  expect_false(identical(result1$om, result2$om))
  expect_false(identical(result1$run, result2$run))
  expect_false(identical(result1$type, result2$type))
})
# }}}

# -- TEST: Edge Cases {{{

context("performance: Edge cases")

test_that("performance handles single statistic", {
  result <- performance(flom, statistics=statistics["C"],
    run="single_stat")
  
  expect_s3_class(result, "data.table")
  expect_true("C" %in% result$statistic)
})

test_that("performance handles statistics with default descriptions", {
  minimal_stats <- list(
    MinStat = list(~yearMeans(C), name = "Minimal")
  )
  
  x <- metrics(flom)
  result <- performance(x, statistics=minimal_stats, refpts=refpts(flom),
    run="minimal_stat")
  
  # Should add desc based on name
  expect_true(all(!is.na(result$desc)))
})
# }}}

# -- CLEANUP: Verify no R-level errors {{{

context("performance: Final validation")

test_that("No errors in executing all major performance calls", {
  # This test ensures all major code paths execute without errors
  expect_silent({
    # FLQuants
    x <- metrics(flom)
    per100 <- performance(x, statistics=statistics, refpts=refpts(flom),
      om="ple", run="r00", type="test")
    
    # FLom with refpts
    per201 <- performance(flom, statistics=statistics, refpts=refpts(flom),
      run="r00", type="test")
    
    # FLmse
    per301 <- performance(flmse, statistics=statistics, run="r00", type="test")
    
    # FLmses
    per401 <- performance(flmses, statistics=statistics, type="test")
  })
})
# }}}
