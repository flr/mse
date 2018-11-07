# is.R - DESC
# mse/R/is.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# map.tm {{{

mpa.tm <- function(stk, genArgs, sel.objective, tracking){             
	sqy <- genArgs$sqy
	sold <- snew <- yearMeans(harvest(stk)[,sqy])
	snew[] <- predict(sel.objective, x=as.numeric(dimnames(snew)[[1]]))
	v <- range(stk, "minfbar"):range(stk,"maxfbar")
	snew <- snew * mean(sold[v])/mean(snew[v])
	list(flq=snew, tracking=tracking)
} # }}}
