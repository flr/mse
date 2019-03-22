# mp.R - DESC
# mse/R/mp.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# mp {{{

mp <- function(om, oem=FLoem(), iem=NULL, ctrl.mp, genArgs, scenario="test", tracking="missing", verbose=TRUE){

browser()
	#============================================================
	# prepare the om
	stk.om <- stock(om)	
	name(stk.om) <- scenario
	sr.om <- sr(om)
	sr.om.res <- residuals(sr.om)
	sr.om.res.mult <- sr.om@logerror
	projection <- projection(om)
	if (!is.null(genArgs$nblocks)) nblocks <- genArgs$nblocks else nblocks <- 1
	fy <- genArgs$fy # final year
	iy <- genArgs$iy # initial year of projection (also intermediate)
	vy <- genArgs$vy <- ac(iy:fy) # vector of years to be projected
	# TODO mlag
	# om$its
	it <- genArgs$it <- dim(stk.om)[6]

	# SET lags
	if (is.null(genArgs$data_lag)) genArgs$data_lag <- 1
	if (is.null(genArgs$management_lag)) genArgs$management_lag <- 1

	# INIT tracking
	metric <- c("F.est", "B.est", "conv.est", "metric.hcr", "metric.is", "metric.iem", "metric.fb","F.om", "B.om", "C.om")
	
	if (!missing(tracking)) metric <- c(metric, tracking)
	tracking <- FLQuant(NA, dimnames=list(metric=metric, year=c((iy-genArgs$management_lag+1):(iy-1),vy), iter=1:it))

	# GET historical
	tracking["metric.is", ac((iy-genArgs$management_lag+1):(iy-1))] <- catch(stk.om)[,ac((iy+1):(iy+genArgs$management_lag-2))]

	# SET seed
	if (!is.null(genArgs$seed)) set.seed(genArgs$seed)

  
	# PREPARE objects for loop call
  if (exists(fleetBehaviour(om))) fb <- fleetBehaviour(om) else fb <- NULL 
	
	#============================================================
	# PREPARE for parallel if needed
	if(nblocks > 1){
		# SPLIT iters in nblocks
		its <- split(seq(it), sort(seq(it)%%nblocks) + 1)
		
		# LOOP and combine
		lst0 <- foreach(i=its, .combine=function(...) {
			list(stk.om=do.call('combine', lapply(list(...), '[[', 'stk.om')),
				tracking=do.call('combine', lapply(list(...), '[[', 'tracking')),
				oem=do.call('combine', lapply(list(...), '[[', 'oem')))
      }, .packages="mse", .multicombine=TRUE, .errorhandling = "stop", .inorder=TRUE) %dopar% {

			# SUBSET object(s)
			call0 <- list(
				stk.om = stk.om[,,,,,i],
				sr.om = FLCore::iter(sr.om,i),
				sr.om.res = sr.om.res[,,,,,i],
				oem = iters(oem, i),
				tracking = tracking[,,,,,i],
				sr.om.res.mult=sr.om.res.mult,
				fb=fb,
				projection=projection,
				iem=iem,
				ctrl.mp= iters(ctrl.mp, i),
				genArgs=genArgs,
				verbose=verbose)
			out <- do.call(mse:::goFish, call0)
			# RETURN
			list(stk.om=out$stk.om, tracking=out$tracking, oem=out$oem)
		}
	} else {
		call0 <- list(
			stk.om = stk.om,
			sr.om = sr.om,
			sr.om.res = sr.om.res,
			oem = oem,
			tracking = tracking,
			sr.om.res.mult=sr.om.res.mult,
			fb=fb,
			projection=projection,
			iem=iem,
			ctrl.mp=ctrl.mp,
			genArgs=genArgs,
			verbose=verbose)
		out <- do.call(mse:::goFish, call0)
		lst0 <- list(stk.om=out$stk.om, tracking=out$tracking, oem=out$oem)
	}

	# GET objects back from loop
	stk.om <- lst0$stk.om
	tracking <- lst0$tracking
	oem <- lst0$oem

	if(verbose) cat("\n")

	#============================================================
	# PREPARE for return
	res <- as(om, "FLmse")
	stock(res) <- window(stk.om, start=iy, end=fy)
	tracking(res) <- window(tracking, end=fy)
	genArgs(res) <- genArgs
	# TODO accessors
	res@oem <- oem
	res@control <- ctrl.mp
	return(res)
}

goFish <- function(stk.om, sr.om, sr.om.res, sr.om.res.mult, fb, projection, oem, iem, tracking, ctrl.mp, genArgs, verbose){

	it <- genArgs$it
	y0 <- genArgs$y0 # initial data year
	fy <- genArgs$fy # final year
	iy <- genArgs$iy # initial year of projection (also intermediate)
	nsqy <- genArgs$nsqy # number of years to compute status quo metrics
	vy <- genArgs$vy # vector of years to be projected

	#============================================================
	# go fish
	for(i in vy[-length(vy)]) {

		gc()
		if(verbose) cat(i, " > ")
		ay <- genArgs$ay <- an(i)
		#genArgs$vy0 <- 1:(ay-y0) # data years (positions vector) - one less than current year
		sqy <- genArgs$sqy <- ac((ay-1):(ay-nsqy)) # years for status quo computations 
		
    		# TRACK om
		tracking["F.om", ac(ay-1)] <- fbar(stk.om)[,ac(ay-1)]    
		tracking["B.om", ac(ay-1)] <- ssb(stk.om)[,ac(ay-1)]    
		tracking["C.om", ac(ay-1)] <- catch(stk.om)[,ac(ay-1)]    
		
		#==========================================================
		# OEM
		#==========================================================
		ctrl.oem <- args(oem)
		ctrl.oem$method <- method(oem)
		ctrl.oem$deviances <- deviances(oem)
		ctrl.oem$observations <- observations(oem)
		ctrl.oem$stk <- stk.om
		ctrl.oem$genArgs <- genArgs #vy0 <- vy0
		#ctrl.oem$ay <- ay
		ctrl.oem$tracking <- tracking
		ctrl.oem$ioval <- list(iv=list(t1=flsval), ov=list(t1=flsval, t2=flival))
		o.out <- do.call("mpDispatch", ctrl.oem)
		stk0 <- o.out$stk
		idx0 <- o.out$idx
		observations(oem) <- o.out$observations
		tracking <- o.out$tracking

		#==========================================================
		# MP
		# Estimator of stock statistics
		#==========================================================
		if (!is.null(ctrl.mp$ctrl.est)){
			ctrl.est <- args(ctrl.mp$ctrl.est)
			ctrl.est$method <- method(ctrl.mp$ctrl.est)
			ctrl.est$stk <- stk0
			ctrl.est$idx <- idx0
			ctrl.est$genArgs <- genArgs #ay <- ay
			ctrl.est$tracking <- tracking
			ctrl.est$ioval <- list(iv=list(t1=flsval, t2=flival), ov=list(t1=flsval))
			out.assess <- do.call("mpDispatch", ctrl.est)
			stk0 <- out.assess$stk
			tracking <- out.assess$tracking
		}
		tracking["F.est",ac(ay)] <- fbar(stk0)[,ac(ay-1)]
		tracking["B.est",ac(ay)] <- ssb(stk0)[,ac(ay-1)]

		#----------------------------------------------------------
		# HCR parametrization
		#----------------------------------------------------------
		if (!is.null(ctrl.mp$ctrl.phcr)){
			ctrl.phcr <- args(ctrl.mp$ctrl.phcr)
			ctrl.phcr$method <- method(ctrl.mp$ctrl.phcr) 
			ctrl.phcr$stk <- stk0
			ctrl.phcr$genArgs <- genArgs #ay <- ay
			#ctrl.phcr$iy <- iy
			ctrl.phcr$tracking <- tracking
			if(exists("hcrpars")) ctrl.phcr$hcrpars <- hcrpars
			ctrl.phcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flpval))
			out <- do.call("mpDispatch", ctrl.phcr)
			hcrpars <- out$hcrpars
			tracking <- out$tracking
		}

		#----------------------------------------------------------
		# HCR
		#----------------------------------------------------------
		if (!is.null(ctrl.mp$ctrl.hcr)){
			ctrl.hcr <- args(ctrl.mp$ctrl.hcr)
			ctrl.hcr$method <- method(ctrl.mp$ctrl.hcr)
			ctrl.hcr$stk <- stk0
			ctrl.hcr$genArgs <- genArgs #ay <- ay
			ctrl.hcr$tracking <- tracking
			if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars
			ctrl.hcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.hcr)
			ctrl <- out$ctrl
			tracking <- out$tracking
		} else {
			ctrl <- getCtrl(yearMeans(fbar(stk0)[,sqy]), "f", ay+1, it)
		}
		tracking["metric.hcr", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]
		
		#----------------------------------------------------------
		# Implementation system
		#----------------------------------------------------------
		if (!is.null(ctrl.mp$ctrl.is)){
			ctrl.is <- args(ctrl.mp$ctrl.is)
			ctrl.is$method <- method(ctrl.mp$ctrl.is)
			ctrl.is$ctrl <- ctrl
			ctrl.is$stk <- stk0
			ctrl.is$genArgs <- genArgs #ay <- ay
			ctrl.is$tracking <- tracking
			ctrl.is$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.is)
			ctrl <- out$ctrl
			tracking <- out$tracking
			tracking["metric.is", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]
		} else {
			tracking["metric.is", ac(ay)] <- tracking["metric.hcr", ac(ay+1)]
		}

		#----------------------------------------------------------
		# Technical measures
		#----------------------------------------------------------
		if (!is.null(ctrl.mp$ctrl.tm)){
			ctrl.tm <- args(ctrl.mp$ctrl.tm)
			ctrl.tm$method <- method(ctrl.mp$ctrl.tm)
			ctrl.tm$stk <- stk0
			ctrl.tm$genArgs <- genArgs #sqy <- sqy
			ctrl.tm$tracking <- tracking
			ctrl.tm$ioval <- list(iv=list(t1=flsval), ov=list(t1=flqval))
			out <- do.call("mpDispatch", ctrl.tm)
			attr(ctrl, "snew") <- out$flq
			tracking <- out$tracking
		}

		#==========================================================
		# IEM
		#==========================================================
		if(!is.null(iem)){
			ctrl.iem <- args(iem)
			ctrl.iem$method <- method(iem)
			ctrl.iem$ctrl <- ctrl
			ctrl.iem$genArgs <- genArgs
			ctrl.iem$tracking <- tracking
			ctrl.iem$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.iem)
			ctrl <- out$ctrl
			tracking <- out$tracking
		}
		tracking["metric.iem",ac(ay)] <- ctrl@trgtArray[,"val",]

		#==========================================================
		# OM
		# fleet dynamics/behaviour
		#==========================================================
		if (!is.null(fb)){
			ctrl.fb <- args(fb)
			ctrl.fb$method <- method(fb)
			ctrl.fb$ctrl <- ctrl
			ctrl.fb$genArgs <- genArgs
			ctrl.fb$tracking <- tracking
			ctrl.fb$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.fb)
			ctrl <- out$ctrl
			tracking <- out$tracking
		}
	    # TODO value()
		tracking["metric.fb",ac(ay)] <- ctrl@trgtArray[,"val",]

		#----------------------------------------------------------
		# stock dynamics and OM projections
		#----------------------------------------------------------
		if(!is.null(attr(ctrl, "snew"))) harvest(stk.om)[,ac(ay+1)] <- attr(ctrl, "snew")
		ctrl.om <- args(projection)
		ctrl.om$ctrl <- ctrl
		ctrl.om$stk <- stk.om
		ctrl.om$sr <- sr.om
		ctrl.om$sr.residuals <- sr.om.res
		ctrl.om$sr.residuals.mult <- sr.om.res.mult
		ctrl.om$method <- method(projection)
		ctrl.om$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flsval))
		stk.om <- do.call("mpDispatch", ctrl.om)$object

	}
	list(stk.om=stk.om, tracking=tracking, oem=oem, genArgs=genArgs)
}

mp00 <- function(om, oem=FLoem(), iem="missing", ctrl.mp, genArgs, scenario="test", tracking="missing", verbose=TRUE){


	#============================================================
	# prepare the om
	stk.om <- stock(om)	
	name(stk.om) <- scenario
	sr.om <- sr(om)
	sr.om.res <- residuals(sr.om)
	sr.om.res.mult <- sr.om@logerror
	fy <- genArgs$fy # final year
	# dy0 om$startyr | oem$args$
	y0 <- genArgs$y0 # initial data year
	# ay - oem$args$datalag
	#dy <- genArgs$dy # final data year
	# TODO mlag
	iy <- genArgs$iy # initial year of projection (also intermediate)
	nsqy <- genArgs$nsqy # number of years to compute status quo metrics
	#ny <- fy - iy + 1 # number of years to project from intial year
	vy <- ac(iy:fy) # vector of years to be projected
	# om$its
	it <- dim(stk.om)[6]

	# INIT tracking
	metric <- c("F.est", "B.est", "conv.est", "metric.hcr", "metric.is", "metric.iem", "metric.fb","F.om", "B.om", "C.om")
	
	if (!missing(tracking)) metric <- c(metric, tracking)
	tracking <- FLQuant(NA, dimnames=list(metric=metric, year=c(iy-1,vy), iter=1:it))

	# GET historical
	tracking["metric.is", ac(iy)] <- catch(stk.om)[,ac(iy)]

	# set seed
	if (!is.null(genArgs$seed)) set.seed(genArgs$seed)
  
	#============================================================
	# go fish
	for(i in vy[-length(vy)]) {

		gc()
		if(verbose) cat(i, " > ")
		ay <- genArgs$ay <- an(i)
		genArgs$vy0 <- 1:(ay-y0) # data years (positions vector) - one less than current year
		sqy <- genArgs$sqy <- ac((ay-1):(ay-nsqy)) # years for status quo computations 
		
    		# TRACK om
		tracking["F.om", ac(ay-1)] <- fbar(stk.om)[,ac(ay-1)]    
		tracking["B.om", ac(ay-1)] <- ssb(stk.om)[,ac(ay-1)]    
		tracking["C.om", ac(ay-1)] <- catch(stk.om)[,ac(ay-1)]    
		
		#==========================================================
		# OEM
		#----------------------------------------------------------
		# function o()
		ctrl.oem <- args(oem)
		ctrl.oem$method <- method(oem)
		ctrl.oem$deviances <- deviances(oem)
		ctrl.oem$observations <- observations(oem)
		ctrl.oem$stk <- stk.om
		ctrl.oem$genArgs <- genArgs #vy0 <- vy0
		#ctrl.oem$ay <- ay
		ctrl.oem$tracking <- tracking
		ctrl.oem$ioval <- list(iv=list(t1=flsval), ov=list(t1=flsval, t2=flival))
		o.out <- do.call("mpDispatch", ctrl.oem)
		stk0 <- o.out$stk
		idx0 <- o.out$idx
		observations(oem) <- o.out$observations
		tracking <- o.out$tracking

		#==========================================================
		# MP
		#----------------------------------------------------------
		# Estimator of stock statistics
		# function f()
		if (!is.null(ctrl.mp$ctrl.est)){
			ctrl.est <- args(ctrl.mp$ctrl.est)
			ctrl.est$method <- method(ctrl.mp$ctrl.est)
			ctrl.est$stk <- stk0
			ctrl.est$idx <- idx0
			ctrl.est$genArgs <- genArgs #ay <- ay
			ctrl.est$tracking <- tracking
			ctrl.est$ioval <- list(iv=list(t1=flsval, t2=flival), ov=list(t1=flsval))
			out.assess <- do.call("mpDispatch", ctrl.est)
			stk0 <- out.assess$stk
			tracking <- out.assess$tracking
		}
		tracking["F.est",ac(ay)] <- fbar(stk0)[,ac(ay-1)]
		tracking["B.est",ac(ay)] <- ssb(stk0)[,ac(ay-1)]
	

		#----------------------------------------------------------
		# HCR parametrization
		# function x()
		if (!is.null(ctrl.mp$ctrl.phcr)){
			ctrl.phcr <- args(ctrl.mp$ctrl.phcr)
			ctrl.phcr$method <- method(ctrl.mp$ctrl.phcr) 
			ctrl.phcr$stk <- stk0
			ctrl.phcr$genArgs <- genArgs #ay <- ay
			#ctrl.phcr$iy <- iy
			ctrl.phcr$tracking <- tracking
			if(exists("hcrpars")) ctrl.phcr$hcrpars <- hcrpars
			ctrl.phcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flpval))
			out <- do.call("mpDispatch", ctrl.phcr)
			hcrpars <- out$hcrpars
			tracking <- out$tracking
		}

		#----------------------------------------------------------
		# HCR
		# function h()
		if (!is.null(ctrl.mp$ctrl.hcr)){
			ctrl.hcr <- args(ctrl.mp$ctrl.hcr)
			ctrl.hcr$method <- method(ctrl.mp$ctrl.hcr)
			ctrl.hcr$stk <- stk0
			ctrl.hcr$genArgs <- genArgs #ay <- ay
			ctrl.hcr$tracking <- tracking
			if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars
			ctrl.hcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.hcr)
			ctrl <- out$ctrl
			tracking <- out$tracking
		} else {
			ctrl <- getCtrl(yearMeans(fbar(stk0)[,sqy]), "f", ay+1, it)
		}
		tracking["metric.hcr", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]
		
		#----------------------------------------------------------
		# Implementation system
		# function k()
		if (!is.null(ctrl.mp$ctrl.is)){
			ctrl.is <- args(ctrl.mp$ctrl.is)
			ctrl.is$method <- method(ctrl.mp$ctrl.is)
			ctrl.is$ctrl <- ctrl
			ctrl.is$stk <- stk0
			ctrl.is$genArgs <- genArgs #ay <- ay
			ctrl.is$tracking <- tracking
			ctrl.is$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.is)
			ctrl <- out$ctrl
			tracking <- out$tracking
			tracking["metric.is", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]
		} else {
			tracking["metric.is", ac(ay)] <- tracking["metric.hcr", ac(ay+1)]
		}

		#----------------------------------------------------------
		# Technical measures
		# function w()
		if (!is.null(ctrl.mp$ctrl.tm)){
			ctrl.tm <- args(ctrl.mp$ctrl.tm)
			ctrl.tm$method <- method(ctrl.mp$ctrl.tm)
			ctrl.tm$stk <- stk0
			ctrl.tm$genArgs <- genArgs #sqy <- sqy
			ctrl.tm$tracking <- tracking
			ctrl.tm$ioval <- list(iv=list(t1=flsval), ov=list(t1=flqval))
			out <- do.call("mpDispatch", ctrl.tm)
			attr(ctrl, "snew") <- out$flq
			tracking <- out$tracking
		}

		#==========================================================
		# IEM
		#----------------------------------------------------------
		if(!missing(iem)){
			ctrl.iem <- args(iem)
			ctrl.iem$method <- method(iem)
			ctrl.iem$ctrl <- ctrl
			ctrl.iem$genArgs <- genArgs
			ctrl.iem$tracking <- tracking
			ctrl.iem$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.iem)
			ctrl <- out$ctrl
			tracking <- out$tracking
		}
		tracking["metric.iem",ac(ay)] <- ctrl@trgtArray[,"val",]

		#==========================================================
		# OM
		#----------------------------------------------------------
		# fleet dynamics/behaviour
		# function j()
		if (exists(fleetBehaviour(om))){
			ctrl.fb <- args(fleetBehaviour(om))
			ctrl.fb$method <- method(fleetBehaviour(om))
			ctrl.fb$ctrl <- ctrl
			ctrl.fb$genArgs <- genArgs
			ctrl.fb$tracking <- tracking
			ctrl.fb$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.fb)
			ctrl <- out$ctrl
			tracking <- out$tracking
		}
	    # TODO value()
		tracking["metric.fb",ac(ay)] <- ctrl@trgtArray[,"val",]

		#----------------------------------------------------------
		# stock dynamics and OM projections
		# function g()
		if(!is.null(attr(ctrl, "snew"))) harvest(stk.om)[,ac(ay+1)] <- attr(ctrl, "snew")
		stk.om <- fwd(stk.om, ctrl=ctrl, sr=sr.om, sr.residuals = sr.om.res, sr.residuals.mult = sr.om.res.mult, maxF=2)
		stk.om <- fwd(stk.om, ctrl=ctrl, sr=sr.om, sr.residuals = sr.om.res, sr.residuals.mult = sr.om.res.mult, maxF=2)
	}
	if(verbose) cat("\n")

	#============================================================
	res <- as(om, "FLmse")
	stock(res) <- window(stk.om, start=iy, end=fy)
	tracking(res) <- window(tracking, end=fy)
	genArgs(res) <- genArgs
	# TODO accessors
	res@oem <- oem
	res@control <- ctrl.mp
	return(res)
}

