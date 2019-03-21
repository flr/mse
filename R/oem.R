# oem.R - DESC
# mse/R/oem.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# sampling.oem {{{

sampling.oem <- function(stk, deviances, observations, genArgs, tracking,
  oe=c("both","index","catch"), lag=1) {
	
  # TODO needs more work to remove the index OE, for now index OE is mandatory 

	dataYears <- 1:(genArgs$ay-genArgs$y0-lag+1)
	assessmentYear <- ac(genArgs$ay)
	# dataYears is a position vector, not the years themselves

	# carry on stock information in the observations for "short-cut" approach
	stock.n(observations$stk)[,assessmentYear] <- stock.n(stk)[,assessmentYear]	
	harvest(observations$stk)[,assessmentYear] <- harvest(stk)[,assessmentYear]	
	
	# catch.n
	# note it's adding 1 individual to avoid sca from crashing
	if(any(oe %in% c("both","catch"))){
		catch.n(observations$stk)[,max(dataYears)] <- catch.n(stk)[,max(dataYears)]*deviances$stk$catch.n[,max(dataYears)] + 1
		catch(observations$stk)[,max(dataYears)] <- computeCatch(observations$stk[,max(dataYears)])
		stk0 <- observations$stk[,dataYears]
	}

	# indices
	if(any(oe %in% c("both","index"))){
		for (idx_count in 1:length(observations$idx)){
			TS <- mean(range(observations$idx[[i]])[c("startf", "endf")])
			index(observations$idx[[idx_count]])[,max(dataYears)] <- 
				stock.n(stk)[,max(dataYears)] * exp((-m(stk)[,max(dataYears)] - harvest(stk)[,max(dataYears)]) * TS) * deviances$idx[[idx_count]][,max(dataYears)]
		}
		idx0 <- lapply(observations$idx, function(x) x[,dataYears])
	}

	# return
	list(stk=stk0, idx=idx0, observations=observations, tracking=tracking)
} # }}}

# perfect.oem {{{

perfect.oem <- function(stk, deviances, observations, genArgs, tracking){
	dataYears <- genArgs$vy0
	assessmentYear <- ac(genArgs$ay)
	stk0 <- stk[,dataYears]
	idx0 <- FLIndices(a=FLIndex(index=stock.n(stk)[,dataYears]*0.01))
	range(idx0[[1]])[c("startf","endf")] <- c(0,0)
	list(stk=stk0, idx=idx0, deviances, observations, tracking=tracking)
} # }}}
