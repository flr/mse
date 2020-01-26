# oem.R - DESC
# mse/R/oem.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# sampling.oem {{{

sampling.oem <- function(stk, deviances, observations, genArgs, tracking,
  oe=c("both","index","catch")) {
	
  # TODO needs more work to remove the index OE, for now index OE is mandatory 
	
	#dataYears <- 1:(genArgs$ay-genArgs$y0-genArgs$data_lag+1)
	dataYears <- genArgs$y0:genArgs$dy
	mxy <- ac(max(dataYears))
	assessmentYear <- ac(genArgs$ay)

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

perfect.oem <- function(stk, deviances, observations, genArgs, tracking){
	dataYears <- genArgs$vy0
	assessmentYear <- ac(genArgs$ay)
	stk0 <- stk[,dataYears]
	idx0 <- FLIndices(a=FLIndex(index=stock.n(stk)[,dataYears]*0.01))
	range(idx0[[1]])[c("startf","endf")] <- c(0,0)
	list(stk=stk0, idx=idx0, deviances, observations, tracking=tracking)
} # }}}

# sample lengths

#sampling_lengths.oem <- function(stk, deviances, observations, genArgs, tracking, ...) {

#	dataYears <- genArgs$y0:genArgs$dy
#	mxy <- ac(max(dataYears))
#	assessmentYear <- ac(genArgs$ay)

#	 carry on stock information in the observations for "short-cut" approach
#	stock.n(observations$stk)[,assessmentYear] <- stock.n(stk)[,assessmentYear]	
#	harvest(observations$stk)[,assessmentYear] <- harvest(stk)[,assessmentYear]	
#	
#	 catch.n
#	 note it's adding 1 individual to avoid sca from crashing
#	flq <- observations$stk@catch.ml
#	flq[,mxy] <- quantSums(catch.n(stk)[,mxy]*deviances$stk$catch.n[,mxy]*deviances$stk$catch.la[,mxy])/quantSums(catch.n(stk)[,mxy]*deviances$stk$catch.n[,mxy])
#	attr(observations$stk, "catch.ml")<- flq
#	stk0 <- observations$stk[,ac(dataYears)]
#	 return
#	list(stk=stk0, idx=FLIndices(), observations=observations, tracking=tracking)
#}  }}}














