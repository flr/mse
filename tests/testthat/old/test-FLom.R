# test-FLom.R - DESC
# /test-FLom.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(ple4om)

# ---  CHECK map of accessors methods

catch(om)
catch.n(om)
catch.wt(om)

landings(om)
landings.n(om)
landings.wt(om)

discards(om)
discards.n(om)
discards.wt(om)

# --- CHECK metrics

ssb(om)
tsb(om)
rec(om)
fbar(om)
