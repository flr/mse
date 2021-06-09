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




plot(FLQuants(OEM=catch.n(obs$stk), OBS=catch.n(observations(oem, "stk"))))

