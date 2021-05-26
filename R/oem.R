# oem.R - DESC
# mse/R/oem.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# default.oem {{{
default.oem <- function(object) {

  # OBSERVATIONS

  # indices:
  idx <- lapply(biols(:
    range=c(startf=0, endf=0)))

  # stocks
  # TODO stk <- as.FLStocks(biols, fisheries)

  # DEVIANCES

  # catch.n
  cn <- lapply(catch.n(fisheries(object)), function(x) x * 0 + 1)

  # index

  obs <- list(idx=idx)
  dev <- list(stk=FLQuants(catch.n=cn))
	
  return(FLoem(method=perfect.oem, observations=obs, deviances=dev))
}
# }}}

# sampling.oem {{{

sampling.oem <- function(object, deviances, observations, args, tracking,
  oe=c("both","index","catch")) {

  stk <- object@stock
	
  # TODO needs more work to remove the index OE, for now index OE is mandatory 
	
	#dataYears <- 1:(args$ay-args$y0-args$data_lag+1)
	dataYears <- args$y0:args$dy
	mxy <- ac(max(dataYears))
	assessmentYear <- ac(args$ay)

	# carry on stock information in the observations for "short-cut" approach
	stock.n(observations$stk)[,assessmentYear] <- stock.n(stk)[,assessmentYear]	
	harvest(observations$stk)[,assessmentYear] <- harvest(stk)[,assessmentYear]	
	
	# catch.n
	# note it's adding 1 individual to avoid sca from crashing
	if(any(oe %in% c("both","catch"))){
		catch.n(observations$stk)[,mxy] <- catch.n(stk)[,mxy] *
      deviances$stk$catch.n[,mxy] + 1
		catch(observations$stk)[,mxy] <- computeCatch(observations$stk[,mxy])
		stk0 <- observations$stk[,ac(dataYears)]
	}

	# indices
	if(any(oe %in% c("both","index"))){
		idx0 <- observations$idx
		for (idx_count in 1:length(observations$idx)){
			TS <- mean(range(observations$idx[[idx_count]])[c("startf", "endf")])
			ages <- dimnames(observations$idx[[idx_count]])$age
			i0 <- (stock.n(stk)[,mxy] * exp((-m(stk)[,mxy] - harvest(stk)[,mxy]) * TS))[ages]
			i0 <- i0 * deviances$idx[[idx_count]][,mxy]
			if(any(i0==0)) i0[i0==0] <- min(i0[i0>0])/2
			index(observations$idx[[idx_count]])[,mxy] <- i0
			idx0[[idx_count]] <- observations$idx[[idx_count]][,ac(range(observations$idx[[idx_count]])['minyear']:mxy)]
		}
	}

	# return
	list(stk=stk0, idx=idx0, observations=observations, tracking=tracking)
} # }}}

# perfect.oem {{{

perfect.oem <- function(stk, deviances, observations, args, tracking){
	dataYears <- ac(args$y0:args$dy)
	assessmentYear <- ac(args$ay)
	stk0 <- stk[,dataYears]
	idx0 <- FLIndices(a=FLIndex(index=stock.n(stk)[,dataYears]*0.01))
	range(idx0[[1]])[c("startf","endf")] <- c(0,0)
	list(stk=stk0, idx=idx0, observations=observations, tracking=tracking)
} # }}}
