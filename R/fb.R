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


effortlimit.fb <- function(ctrl, max=1, tracking, args) {

# merge {{{

setMethod("merge", signature(x="fwdControl", y="fwdControl"),
  function(x, y, ...) {

  # PARSE args
  args <- c(list(x, y), list(...))

  # RBIND @target
  target <- do.call(rbind, lapply(args, slot, "target"))

  # MERGE @iters
  its <- lapply(args, slot, "iters")

  # rows per ctrl
  ros <- unlist(lapply(its, nrow))
  pos <- c(1, ros[-length(ros)] + 1)
  nros <- sum(ros)
  
  # STOP if different iters
  nits <- unique(unlist(lapply(its, function(x) dim(x)[3])))
  if(length(nits) > 1)
    stop("fwdControl objects must have the same number of iters")

  iters <- array(dim=c(nros, 3, nits), dimnames=list(row=seq(nros),
      val=c("min", "value", "max"), iter=seq(nits)))
 
  for(i in seq(its))
    iters[seq(pos[i], length=ros[i]), ,]  <- c(its[[i]])

  return(fwdControl(target=target, iters=iters))
})

# }}}
 
  ctrl <- merge(ctrl,
    fwdControl(year=ctrl$year, quant="effort", max=max, fishery=1))

  return(list(ctrl=ctrl, tracking=tracking))
}


