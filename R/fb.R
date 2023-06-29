# fb.R - DESC
# mse/R/fb.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# hyperstability.fb {{{

hyperstability.fb <- function(ctrl, beta=1, maxF=2, alpha=maxF^(1-beta), tracking, args) {

	# Only operates on F targets - so nothing happens to TAC
	# This function creates a control file to be later used in the fwd()
	# function where two optional relations are established between
	# fishing effort and fishing mortality
	
  # Beta is in this MSE either 1 for a 1:1 linear relationship between
	# F and effort, if beta = 0.7, the relation is not linear and it can
	# mimick a hyperstability scenario.
	# alpha = maxF^(1-beta) # linear meets curve at maxF
	
  ctrl$value[ctrl$quant == 'f'] <- alpha *
    ctrl$value[ctrl$quant == 'f']^beta
	
  list(ctrl=ctrl, tracking=tracking)
	
} # }}}

# response.fb {{{

response.fb <- function(ctrl, min=0.90, args, tracking) {

  # GET current and previous decision year
  ay <- args$ay
  dy <- args$dy
  py <- ac(ay + args$management_lag)

  # GET past catch
  past <- tracking[[1]]['C.obs', ac(dy)]

  # ID iters where TAC_ay < C_dy
  min <- ifelse(ctrl$value < c(past), min, 0)

  mctrl <- fwdControl(year=py, quant="effort", relYear=ay,
    fishery=1, relFishery=1, min=min)

  # TRACK if min is aplied
  track(tracking, "fb.imp", ac(ay)) <- min

  return(list(ctrl=merge(ctrl, mctrl), tracking=tracking))
}
# }}}

# effortlimit.fb {{{

effortlimit.fb <- function(ctrl, max=1, tracking, args) {

  ctrl <- merge(ctrl,
    fwdControl(lapply(ctrl$year, function(x) list(year=x, relYear=x - 1,
      quant="effort", max=rep(max, args$it), fishery=1, relFishery=1)))
  )

  return(list(ctrl=ctrl, tracking=tracking))
}
# }}}
