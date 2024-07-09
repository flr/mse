# test-mp_OMs.R - DESC
# /home/mosqu003/FLR/mse/mse/tests/testthat/test-mp_OMs.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# --

library("doFuture")

plan(list(
  tweak(multisession, workers = 2L),
  tweak(multisession, workers = 3L)
))

outer <- foreach(ii = 1:2) %dofuture% {
  info <- list(
    available = nbrOfWorkers(),
    pid       = Sys.getpid()
  )

  inner <- foreach(jj = 1:3) %dofuture% {
    Sys.sleep(1)
    list(
      available = nbrOfWorkers(),
      pid       = Sys.getpid()
    )
  }

  c(outer=info, inner=inner)
}

str(outer)


# --

data(sol274)

om <- iter(window(om, start=2010), 1:3)
oms <- list(A=om, B=om)

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=41500, target=0.27))))

# - NESTED

plan(list(
  tweak(multicore, workers=2L),
  tweak(multicore, workers=3L)
))

system.time(
tes2 <- mp(oms, oem=oem, control=control, args=list(iy=2021, fy=2022))
)

# CHECK cores used
tracking(tes2[[1]])['pid',]
tracking(tes2[[2]])['pid',]

# - SINGLE

plan(multicore, workers=2L)

system.time(
tes1 <- mp(oms, oem=oem, control=control, args=list(iy=2021, fy=2022))
)

# CHECK cores used
tracking(tes1[[1]])['pid',]
tracking(tes1[[2]])['pid',]





# - MULTIPLE cores if run on one om?

plan(multicore, workers=2L)

tes1 <- mp(oms[[1]], oem=oem, control=control, args=list(iy=2021, fy=2025))

tracking(tes1)['pid',]

#

