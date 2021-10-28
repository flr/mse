# alb.R - DESC
# /alb.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(ss3om)

out <- readOutputss3("alb", compress="bz2")

# FLStock
stk <- simplify(buildFLSss330(out), 'season')

st <- simplify(buildFLSss330(out))

ns <- noseason(buildFLSss330(out))
nu <- nounit(buildFLSss330(out))
nn <- noseason(nounit(buildFLSss330(out)))
nn <- nounit(noseason(buildFLSss330(out)))

plot(st, nn)

plot(stk, ns)


# FLSR
srr <- buildFLSRss3(out)

# FLIndexBiomass
idx <- buildFLIBss330(out, fleets=1:4)

# refpts
rps <- buildFLRPss330(out)

# results
res <- buildRESss330(out)

# OM conditioned
omc <- FLom(stock=stk, refpts=rps, sr=srr)

# fwdWindow(om)
om <- fwdWindow(omc, end=2046)

# fwd

f0 <- fwd(om, control=fwdControl(year=2018:2046, quant="fbar", value=0))

plot(f0)
