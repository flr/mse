# test-oem.R - DESC
# /test-oem.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(ple4om)

# --- perfect.oem

args <- list(y0=dims(om)$minyear, dy=dims(om)$maxyear)

obs <- perfect.oem(om, deviances=NULL, observations=NULL, args=args,
  tracking=FLQuant())

# CHECK om and obs match, with no deviances

all.equal(obs$stk, stock(om))

all.equal(index(obs$idx[[1]]) * index.q(obs$idx[[1]]),
  stock.n(obs$stk), check.attributes = FALSE)


# --- sampling.oem


obs <- method(oem)(om, deviances=deviances(oem), observations=observations(oem),
  args=list(y0=1957, dy=2017, ay=2018, freq=1), tracking=FLQuant(), oe="both")

catch.n(obs$stk) / window(catch.n(observations(oem, "stk")), end=2017)

# TEST w/ FLIndexBiomass


plot(FLQuants(OEM=catch.n(obs$stk), OBS=catch.n(observations(oem, "stk"))))


# TEST index not to be updated (ends before iy)

observations(oem)$idx <- FLIndices(A=observations(oem)$idx[[1]],
  B=window(observations(oem)$idx[[1]], end=2015))

deviances(oem)$idx <- FLQuants(A=deviances(oem)$idx[[1]], 
  B=window(deviances(oem)$idx[[1]], end=2015))

# TODO deviances(oem, 'idx')

obs <- method(oem)(om, deviances=deviances(oem), observations=observations(oem),
  args=list(y0=1957, dy=2018, ay=2019, frq=1), tracking=FLQuant(), oe="both")

