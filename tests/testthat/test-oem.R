# test-oem.R - DESC
# /test-oem.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# XX {{{
# }}}

data(p4om)

args <- list(y0=dims(om)$minyear, dy=dims(om)$maxyear)

stk <- perfect.oem(om, deviances=NULL, observations=NULL, args=args,
  tracking=FLQuant())

# CHECK
