# fb.R - DESC
# mse/R/fb.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# hyperstability.fb {{{
hyperstability.fb <- function(ctrl, beta=1, maxF=2, alpha=maxF^(1-beta), tracking, genArgs) {

	# Only operates on F targets - so nothing happens to TAC
	# This function creates a control file to be later used in the fwd()
	# function where two optional relations are established between
	# fishing effort and fishing mortality
	
  # Beta is in this MSE either 1 for a 1:1 linear relationship between
	# F and effort, if beta = 0.7, the relation is not linear and it can
	# mimick a hyperstability scenario.
	# alpha = maxF^(1-beta) # linear meets curve at maxF
	
  ctrl@trgtArray[ctrl@target[,"quantity"]=="f",,] <- alpha * ctrl@trgtArray[ctrl@target[,"quantity"]=="f",,]^beta
	list(ctrl=ctrl, tracking=tracking)
	
} # }}}

