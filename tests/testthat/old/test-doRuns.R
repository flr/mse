# test-doRuns.R - DESC
# /test-doRuns.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# XX {{{
# }}}

# STOCK

data(ple4)

# SR

sr4 <- fmle(as.FLSR(ple4, model='ricker'))

# Future catches
catch <- catch(ple4)[,ac(2000:2008)]
