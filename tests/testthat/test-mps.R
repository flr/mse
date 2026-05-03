# test-mps.R - DESC
# mse/tests/testthat/test-mps.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# dataset contains both OM (FLom) and OEM (FLoem)
data(plesim)
data(statistics)

# ncoresrun, number of cores used in a run from tracking$pid
ncoresrun <- function(x) {
  length(unique(unlist(lapply(x, function(x)
    tracking(x)[metric == 'pid', unique(data)]))))
}

# Set control: sa and hcr
control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=10000, target=0.18))))

# CHECK: RUN mps w/one hcr argument

plan(sequential)

test_that("mps runs in sequential with one hcr argument", {

  tes11 <- mps(om=om, control=control, args=list(iy=2025, fy=2028),
    hcr=list(trigger=seq(9e3, 12e3, length=3)))
  
  # RUNS all 3 hcrs
  expect_equal(length(tes11), 3)

  # RUNS on the same code (process)
  expect_equal(ncoresrun(tes11), 1)

})

# CHECK: RUN mps w/multiple cores

if(os.linux()) {
  plan(multicore, workers=3)
} else {
  plan(multisession, workers=3)
}

#
test_that("mps runs with one hcr argument on multiple cores", {
  
  tes12 <- mps(om=om, control=control, args=list(iy=2025, fy=2028),
  hcr=list(trigger=seq(9e3, 16e3, length=3)))

  # RUNS on 3 cores
  expect_equal(ncoresrun(tes12), 3)

})

#
test_that("mps runs with one hcr argument on multiple cores with statistics", {

  tes13 <- mps(om=om, control=control, args=list(iy=2025, fy=2028),
    hcr=list(trigger=seq(12e3, 16e3, length=3)), statistics=statistics[1:4])

  # RUNS on 3 cores
  expect_equal(ncoresrun(tes13), 3)

})

# CHECK: reports years in progress bar
test_that("mps runs with one hcr argument on multiple cores with performance", {

  tes14 <- mps(om=om, control=control, args=list(iy=2025, fy=2028),
    hcr=list(trigger=seq(12e3, 16e3, length=4)))

  expect_type(performance(tes14), "data.table")

})


# RUN mps w/two hcr arguments

test_that("mps runs with two hcr arguments", {

  tes20 <- mps(om=om, control=control, args=list(iy=2025, fy=2028),
    hcr=list(trigger=seq(12e3, 16e3, length=3),
      lim=seq(4000, 6000, length=3)))

  expect_equal(length(tes20), 9)

})

test_that("mps runs with two hcr arguments on multiple cores", {
  
  tes21 <- mps(om=om, control=control, args=list(iy=2025, fy=2028),
    hcr=list(trigger=seq(12e3, 16e3, length=3),
      lim=seq(4000, 6000, length=3)))

  # CHECK: run on 3 cores
  expect_equal(ncoresrun(tes21), 3)

})

test_that("mps runs with two hcr arguments on multiple cores with statistics", {

  tes22 <- mps(om=om, control=control, args=list(iy=2025, fy=2028),
    hcr=list(trigger=seq(12e3, 16e3, length=3),
      lim=seq(4000, 6000, length=3)), statistics=statistics[1:4])
  
  expect_equal(ncoresrun(tes22), 3)

})

test_that("mps runs with two hcr arguments on multiple cores with performance", {

  tes23 <- mps(om=om, control=control, args=list(iy=2025, fy=2028),
    hcr=list(trigger=seq(12e3, 16e3, length=3),
      lim=seq(4000, 6000, length=3)))

  expect_is(performance(tes23), "data.table")

})
