# oem.R - DESC
# mse/R/oem.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# perfect.oem {{{

#' [TODO:description]
#'
#' @param om [TODO:description]
#' @param deviances [TODO:description]
#' @param observations [TODO:description]
#' @param args [TODO:description]
#' @param tracking [TODO:description]
#'
#' @return [TODO:description]
#' @export
#'
#' @examples
#' data(cjm)
#' perfect.oem(om, deviances=NULL, observations=NULL,
#'   args=list(y0=1970, dy=2020), tracking=FLQuant())

perfect.oem <- function(om, deviances, observations, args, tracking) {

  # GET perfect stock
	stk <- window(stock(om), start=args$y0, end=args$dy, extend=FALSE)

  # SET perfect at-age FLIndex
  idx <- FLIndices(A=FLIndex(index=stock.n(stk) * 0.01,
    range=c(startf=0, endf=0)))

	list(stk=stk, idx=idx, observations=observations, tracking=tracking)

} # }}}


#{{{
#' catch.oem()   
#'
#' Function to set up forecast horizon of index.q for oem in mp
#'   
#' @param stk.om input object of the class FLom from package mse 
#' @param ce option to specify a fixed catch error (CV), overwriting catch variations estimates   
#' @param args list of generic args that match those for mp()..list(iy,fy,nsqy,nblocks=it,seed) 
#' @return catch.dev in the form of multiplicative lognormal errors
#' @export
catch.oem <- function(stk.om,ce=NULL,args){ 
  catch.dev <- log(stk.om@stock@catch.n)
  catch.dev <- catch.dev-iterMeans(catch.dev)
  # compute varcov for multivariate normal randomization
  vy = args$y0:args$fy
  if(is.null(ce)){
  Sig <- apply(catch.dev[,ac(args$y0:args$dy),1,1,,drop=TRUE], 3, function(x) cov(t(x)))
  Sig <- apply(Sig, 1, mean)
  Sig <- matrix(Sig, ncol=dim(catch.dev)[1])
  # randomize
  catch.dev[,ac(vy)][] <- t(mvrnorm((args$it) * length(vy), rep(0, nrow(Sig)), Sig))
  } else {
    catch.dev[,ac(vy)][] <- rlnorm((args$it) * length(vy),0,ce)
  }
  # exponentiate for OEM object
  return((catch.dev))
}
#}}}





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


