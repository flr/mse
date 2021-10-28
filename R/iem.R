# iem.R - DESC
# mse/R/iem.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# noise.iem {{{

# TODO bias-correct

noise.iem <- function(ctrl, fun="rlnorm", mean=0, sd=0.1, multiplicative=TRUE, args, tracking){
  # eh? number of non NA values in target. But if there are NAs then we need to
  # know their position for the *ctrl@iters later
	# use decision taken in year considering management lag
	iem <- list(mean = mean, sd = sd, n = sum(!is.na(ctrl@iters)))
	if(multiplicative){
		ctrl@iters <- do.call(fun, iem) * ctrl@iters
	} else {
		ctrl@iters <- do.call(fun, iem) + ctrl@iters
	}
	lst <- list(ctrl=ctrl, tracking=tracking)
	lst
} # }}}
