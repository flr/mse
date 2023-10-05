# iem.R - DESC
# mse/R/iem.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# noise.iem {{{

noise.iem <- function(ctrl, noise, multiplicative=TRUE, fishery=NULL,
  args, tracking){

  # GET implementation years
  cyrs <- unique(ctrl$year)

  # MISSING noise
  if(missing(noise)) {
    warning("'iem' setup with no 'noise' argument, adding no noise.")
	  return(list(ctrl=ctrl, tracking=tracking)) 
  }

  # PARSE noise
  if(is(noise, 'numeric'))
    noise <- FLQuant(noise, dimnames=list(year=cyrs,
      iter=seq(dim(iters(ctrl))[3])))

  # APPLY noise
  if(multiplicative) {
    iters(ctrl)[, 'value',]  <- c(iters(ctrl)[, 'value',]) *
      c(noise[, ac(cyrs)])
  } else {
    iters(ctrl)[, 'value',]  <- c(iters(ctrl)[, 'value',]) +
      c(noise[, ac(cyrs)])
  }
  
	return(list(ctrl=ctrl, tracking=tracking)) 
	
} # }}}

# partial.iem {{{

partial.iem <- function(ctrl, args, tracking, response=c("catch", "effort"),
  nocomp=NULL) {
  
  # TODO MATCH names

  idx <- ctrl$fishery %in% nocomp

  # OPTION 1. nocomp fisheries keep catch as last year

  # iters$value as 1
  ctrl@iters[idx,"value",] <- 1

  # target$relYear as year - 1
  ctrl@target[idx, "relYear"] <- ctrl@target[idx, "year"] - 1
  ctrl@target[idx, "relSeason"] <- ctrl@target[idx, "season"]
  ctrl@target[idx, "relFishery"] <- ctrl@target[idx, "fishery"]
  ctrl@target[idx, "relCatch"] <- ctrl@target[idx, "catch"]

  # TODO ADD effort limit
  if(response[1] == "catch") {
    a <- 1
  }

  # OPTION 2. nocomp fisheries keep effort as last year
  
  if(response[1] == "effort") {
    ctrl@target[idx, "quant"] <- "effort"
    ctrl@target[idx, "relCatch"] <- NA
    ctrl@target[idx, "catch"] <- NA
  }

  res <- list(ctrl=ctrl, tracking=tracking)
} # }}}
