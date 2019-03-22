# iem.R - DESC
# mse/R/iem.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# noise.iem {{{

noise.iem <- function(ctrl, fun="rlnorm", mean=0, sd=0.1, multiplicative=TRUE, genArgs, tracking){
  # eh? number of non NA values in target. But if there are NAs then we need to
  # know their position for the *ctrl@trgtArray later

	# use decision taken in year considering management lag
    ctrl@trgtArray[,"val",] <- tracking["metric.is",ac(genArgs$ay-genArgs$management_lag+1)]
	iem <- list(mean = mean, sd = sd, n = sum(!is.na(ctrl@trgtArray)))
	if(multiplicative){
		ctrl@trgtArray <- do.call(fun, iem) * ctrl@trgtArray
	} else {
		ctrl@trgtArray <- do.call(fun, iem) + ctrl@trgtArray
	}
	list(ctrl=ctrl, tracking=tracking)
} # }}}


