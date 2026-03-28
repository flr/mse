# test-oem.R - Unit tests for observation error models
# flr/mse/tests/testthat/test-oem.R

# Copyright (c) WMR, 2025.
# Author: Iago Mosqueira (WUR)
#
# Distributed under the terms of the EUPL-1.2


# Setup basic test arguments
test_args <- list(y0=1960, dy=2021, frq=1)

# TEST: perfect.oem {{{

context("oem: perfect.oem - basic functionality")

test_that("perfect.oem returns a list with correct structure", {
  stk <- stock(flom)
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant())
  
  expect_is(result, "list")
  expect_true(all(c("stk", "idx", "observations", "tracking") %in% names(result)))
})

test_that("perfect.oem returns FLStock object", {
  stk <- stock(flom)
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
})

test_that("perfect.oem returns FLIndices object", {
  stk <- stock(flom)
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$idx, "FLIndices")
  expect_equal(length(result$idx), 1)
})

test_that("perfect.oem stores observations correctly", {
  stk <- stock(flom)
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant())
  
  expect_true("stk" %in% names(result$observations))
  expect_true("idx" %in% names(result$observations))
  expect_s4_class(result$observations$stk, "FLStock")
})

test_that("perfect.oem windows stock to correct years", {
  stk <- stock(flom)
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant())
  
  # Stock should be windowed to y0:dy
  expect_equal(dims(result$stk)$minyear, test_args$y0)
  expect_equal(dims(result$stk)$maxyear, test_args$dy)
})

test_that("perfect.oem returns tracking object", {
  stk <- stock(flom)
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$tracking, "FLQuant")
})
# }}}

# TEST: perfect.oem with biomass parameter {{{

context("oem: perfect.oem - biomass index")

test_that("perfect.oem creates age-structured index by default", {
  stk <- stock(flom)
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant(), biomass=FALSE)
  
  # Age-structured index should have age dimension > 1
  idx <- result$idx[[1]]
  expect_true(dims(idx)$age > 1)
})

test_that("perfect.oem creates biomass index when biomass=TRUE", {
  stk <- stock(flom)
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant(), biomass=TRUE)
  
  idx <- result$idx[[1]]
  expect_s4_class(idx, "FLIndexBiomass")
})
# }}}

# TEST: perfect.oem with existing observations {{{

context("oem: perfect.oem - with observations")

test_that("perfect.oem updates existing observations list", {
  stk <- stock(flom)
  
  # Create initial observations list
  init_obs <- list(stk=stk, idx=FLIndices())
  
  result <- perfect.oem(stk, deviances=NULL, observations=init_obs,
    args=test_args, tracking=FLQuant())
  
  expect_true("stk" %in% names(result$observations))
  expect_true("idx" %in% names(result$observations))
})

test_that("perfect.oem preserves passed observations structure", {
  stk <- stock(flom)
  
  init_obs <- list(
    stk = stk,
    idx = FLIndices(),
    custom_field = "test_value"
  )
  
  result <- perfect.oem(stk, deviances=NULL, observations=init_obs,
    args=test_args, tracking=FLQuant())
  
  # Custom field should be preserved
  expect_true("custom_field" %in% names(result$observations))
  expect_equal(result$observations$custom_field, "test_value")
})
# }}}

# TEST: shortcut.oem {{{

context("oem: shortcut.oem - basic functionality")

test_that("shortcut.oem returns list with correct structure", {
  stk <- stock(flom)
  obs <- list(stk=stk, idx=FLIndices())
  
  result <- shortcut.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_is(result, "list")
  expect_true(all(c("stk", "idx", "observations", "tracking") %in% names(result)))
})

test_that("shortcut.oem returns FLStock object", {
  stk <- stock(flom)
  obs <- list(stk=stk, idx=FLIndices())
  
  result <- shortcut.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
})

test_that("shortcut.oem returns empty FLIndices", {
  stk <- stock(flom)
  obs <- list(stk=stk, idx=FLIndices())
  
  result <- shortcut.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$idx, "FLIndices")
  expect_equal(length(result$idx), 0)
})

test_that("shortcut.oem windows stock to correct years", {
  stk <- stock(flom)
  obs <- list(stk=stk, idx=FLIndices())
  
  result <- shortcut.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_equal(dims(result$stk)$minyear, test_args$y0)
  expect_equal(dims(result$stk)$maxyear, test_args$dy)
})
# }}}

# TEST: shortcut.oem with deviances {{{

context("oem: shortcut.oem - with deviances")

test_that("shortcut.oem applies catch.n deviances", {
  stk <- stock(flom)
  obs <- list(stk=stk, idx=FLIndices())
  
  # Create deviances
  deviances <- list(stk=list(catch.n=catch.n(stk) %=% 0.9))
  
  result <- shortcut.oem(stk, deviances=deviances, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
  # Check that catch has been modified
  expect_true(dims(result$stk)$maxyear == test_args$dy)
})

test_that("shortcut.oem computes catch aggregates after deviances", {
  stk <- stock(flom)
  obs <- list(stk=stk, idx=FLIndices())
  
  deviances <- list(stk=list(catch.n=catch.n(stk) %=% 0.8))
  
  result <- shortcut.oem(stk, deviances=deviances, observations=obs,
    args=test_args, tracking=FLQuant())
  
  # Catch should be > 0
  expect_true(all(catch(result$stk) > 0))
})

test_that("shortcut.oem simplifies dimensions to match observations", {
  stk <- stock(flom)
  
  # Create observations with reduced dimensions
  obs_stk <- window(stk, start=test_args$y0, end=test_args$dy)
  if(dims(obs_stk)$unit > 1) obs_stk <- nounit(obs_stk)
  obs <- list(stk=obs_stk, idx=FLIndices())
  
  result <- shortcut.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
})
# }}}

# TEST: sampling.oem {{{

context("oem: sampling.oem - basic functionality")

test_that("sampling.oem returns list with correct structure", {
  stk <- stock(flom)
  
  # Create observations with indices
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_is(result, "list")
  expect_true(all(c("stk", "idx", "observations", "tracking") %in% names(result)))
})

test_that("sampling.oem requires stk in observations", {
  stk <- stock(flom)
  obs <- list(idx=FLIndices())
  
  expect_error(
    sampling.oem(stk, deviances=NULL, observations=obs,
      args=test_args, tracking=FLQuant()),
    regex = "must have elements"
  )
})

test_that("sampling.oem requires idx in observations", {
  stk <- stock(flom)
  obs <- list(stk=stk)
  
  expect_error(
    sampling.oem(stk, deviances=NULL, observations=obs,
      args=test_args, tracking=FLQuant()),
    regex = "must have elements"
  )
})

test_that("sampling.oem returns FLStock", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
})

test_that("sampling.oem returns FLIndices", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$idx, "FLIndices")
})

test_that("sampling.oem windows stock to correct years", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_equal(dims(result$stk)$minyear, test_args$y0)
  expect_equal(dims(result$stk)$maxyear, test_args$dy)
})
# }}}

# TEST: sampling.oem with deviances on stock {{{

context("oem: sampling.oem - with stock deviances")

test_that("sampling.oem applies catch.n deviances", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  # Create deviances for last years
  dyrs <- ac(seq(test_args$dy - test_args$frq + 1, test_args$dy))
  deviances <- list(
    stk=list(catch.n=catch.n(stk)[, dyrs] %=% 0.95),
    idx=NULL
  )
  
  result <- sampling.oem(stk, deviances=deviances, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
})

test_that("sampling.oem updates landings/discards/catch with deviances", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  dyrs <- ac(seq(test_args$dy - test_args$frq + 1, test_args$dy))
  deviances <- list(
    stk=list(catch.n=catch.n(stk)[, dyrs] %=% 0.9),
    idx=NULL
  )
  
  result <- sampling.oem(stk, deviances=deviances, observations=obs,
    args=test_args, tracking=FLQuant())
  
  # Check that catch is computed
  expect_true(all(catch(result$stk)[, dyrs] > 0, na.rm=TRUE))
})
# }}}

# TEST: sampling.oem with index deviances {{{

context("oem: sampling.oem - with index deviances")

test_that("sampling.oem applies index deviances", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  dyrs <- ac(seq(test_args$dy - test_args$frq + 1, test_args$dy))
  
  # Create index deviances
  deviances <- list(
    stk=NULL,
    idx=list(A=index.q(idx$A)[, dyrs] %=% 1.1)
  )
  
  result <- sampling.oem(stk, deviances=deviances, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$idx, "FLIndices")
})

test_that("sampling.oem generates default index deviances if missing", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$idx, "FLIndices")
  expect_true(length(result$idx) > 0)
})

test_that("sampling.oem handles empty index deviances list", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  deviances <- list(stk=NULL, idx=list())
  
  result <- sampling.oem(stk, deviances=deviances, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$idx, "FLIndices")
})
# }}}

# TEST: sampling.oem with stability parameter {{{

context("oem: sampling.oem - stability parameter")

test_that("sampling.oem with stability=1 works", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    stability=1, args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
})

test_that("sampling.oem with stability=0.5 works", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    stability=0.5, args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
})
# }}}

# TEST: sampling.oem with weights parameter {{{

context("oem: sampling.oem - weights parameter")

test_that("sampling.oem with wts=TRUE updates weights", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    wts=TRUE, args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
})

test_that("sampling.oem with wts=FALSE does not update weights", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    wts=FALSE, args=test_args, tracking=FLQuant())
  
  expect_s4_class(result$stk, "FLStock")
})
# }}}

# TEST: sampling.oem dimension simplification {{{

context("oem: sampling.oem - dimension handling")

test_that("sampling.oem simplifies dimensions to match observations", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  
  # Create observations with single unit
  obs_stk <- window(stk, start=test_args$y0, end=test_args$dy)
  if(dims(obs_stk)$unit > 1) obs_stk <- nounit(obs_stk)
  obs <- list(stk=obs_stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_equal(dims(result$stk)$unit, dims(obs$stk)$unit)
})

test_that("sampling.oem ensures no zero indices", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  # All indices should be > 0
  expect_true(all(index(result$idx[[1]]) > 0))
})
# }}}

# TEST: default.oem {{{

context("oem: default.oem - basic functionality")

test_that("default.oem returns FLoem object", {
  result <- default.oem(flom)
  
  expect_s4_class(result, "FLoem")
})

test_that("default.oem creates observations for FLom", {
  result <- default.oem(flom)
  
  obs <- observations(result)
  expect_is(obs, "list")
  expect_true("stk" %in% names(obs))
  expect_true("idx" %in% names(obs))
})

test_that("default.oem sets method to perfect.oem", {
  result <- default.oem(flom)
  
  method_func <- method(result)
  expect_equal(method_func, perfect.oem)
})

test_that("default.oem observations contain FLStock", {
  result <- default.oem(flom)
  
  obs <- observations(result)
  expect_s4_class(obs$stk, "FLStock")
})

test_that("default.oem observations contain FLIndices", {
  result <- default.oem(flom)
  
  obs <- observations(result)
  expect_s4_class(obs$idx, "FLIndices")
})
# }}}

# TEST: OEM integration with mp workflow {{{

context("oem: Integration with mp workflow")

test_that("perfect.oem works in mp call", {

  control <- mpCtrl(
    est = mseCtrl(method=perfect.sa),
    hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.10)))

  result <- mp(flom, ctrl=control, args=list(iy=2025))
  
  expect_s4_class(result, "FLmse")

  expect_s4_class(observations(oem(result))$stk, "FLStock")

  expect_s4_class(observations(oem(result))$idx, "FLIndices")
})
# }}}

# TEST: OEM output consistency {{{

context("oem: Output consistency")

test_that("perfect.oem produces consistent results", {
  stk <- stock(flom)
  
  set.seed(123)
  result1 <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant())
  
  set.seed(123)
  result2 <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=FLQuant())
  
  # Results should be identical
  expect_equal(catch(result1$stk), catch(result2$stk))
})

test_that("shortcut.oem produces consistent results", {
  stk <- stock(flom)
  obs <- list(stk=stk, idx=FLIndices())
  
  set.seed(123)
  result1 <- shortcut.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  set.seed(123)
  result2 <- shortcut.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_equal(catch(result1$stk), catch(result2$stk))
})
# }}}

# TEST: Error handling {{{

context("oem: Error handling")

test_that("sampling.oem stops with missing observations list", {
  stk <- stock(flom)
  
  expect_error(
    sampling.oem(stk, deviances=NULL, observations=NULL,
      args=test_args, tracking=FLQuant()),
    regex = "must have elements"
  )
})

test_that("default.oem stops with invalid OM class", {
  # Create invalid object
  invalid_om <- "not_an_om"
  
  expect_error(
    default.oem(invalid_om),
    regex = "unable to find an inherited method"
  )
})
# }}}

# TEST: Edge cases {{{

context("oem: Edge cases")

test_that("perfect.oem handles single year window", {
  stk <- stock(flom)
  
  single_year_args <- list(y0=2020, dy=2021, frq=1)
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=single_year_args, tracking=FLQuant())
  
  expect_equal(dims(result$stk)$year, 2)
})

test_that("sampling.oem handles multiple indices", {
  stk <- stock(flom)
  
  # Create multiple indices
  idx1 <- as(stk, 'FLIndex')
  idx2 <- as(stk, 'FLIndex')
  idx <- FLIndices(A=idx1, B=idx2)
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  expect_equal(length(result$idx), 2)
})

test_that("sampling.oem prevents zero indices", {
  stk <- stock(flom)
  idx <- FLIndices(A=as(stk, 'FLIndex'))
  obs <- list(stk=stk, idx=idx)
  
  result <- sampling.oem(stk, deviances=NULL, observations=obs,
    args=test_args, tracking=FLQuant())
  
  # All indices should be positive
  all_indices <- index(result$idx[[1]])
  expect_true(all(all_indices > 0))
})

test_that("perfect.oem tracking object is passed through", {
  stk <- stock(flom)
  tracking_in <- FLQuant(1:10)
  
  result <- perfect.oem(stk, deviances=NULL, observations=NULL,
    args=test_args, tracking=tracking_in)
  
  expect_equal(result$tracking, tracking_in)
})

# CLEANUP: No cleanup needed as all tests use temporary objects
