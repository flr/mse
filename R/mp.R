# mp.R - DESC
# mse/R/mp.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# mp {{{

mp <- function(om, oem=FLoem(), iem=NULL, ctrl, genArgs, scenario="test", tracking="missing", verbose=TRUE){

	#============================================================
	# prepare the om
	stk.om <- stock(om)	
	name(stk.om) <- scenario
	sr.om <- sr(om)
	sr.om.res <- residuals(sr.om)
	sr.om.res.mult <- sr.om@logerror
	projection <- projection(om)
	#if (!is.null(genArgs$nblocks)) nblocks <- genArgs$nblocks else nblocks <- 1
	fy <- genArgs$fy # final year
	iy <- genArgs$iy # initial year of projection (also intermediate)
	nsqy <- genArgs$nsqy # sq years
	vy <- genArgs$vy <- ac(iy:fy) # vector of years to be projected
	# om$its
	it <- genArgs$it <- dim(stk.om)[6]

	# SET lags
	# data lag = time in years between the data and the year 
	#  the assessment is performed (ay).
	# management lag = time in years between the year the 
	#  assessment is performed (ay) and the implementation of the management action.
	if (is.null(genArgs$data_lag)) genArgs$data_lag <- 1
	if (is.null(genArgs$management_lag)) genArgs$management_lag <- 1

	# INIT tracking
	metric <- c("C.obs", "F.est", "B.est", "C.est", "conv.est", "metric.phcr", "metric.hcr", "metric.is", "metric.iem", "metric.fb","F.om", "B.om", "C.om")

	if (!missing(tracking)) metric <- c(tracking, metric)
	tracking <- FLQuant(NA, dimnames=list(metric=metric, year=unique(c((iy-genArgs$management_lag+1):iy,vy)), iter=1:it))
	# GET historical from OM 
	# ToDo intermediate year function
	catch.inty <- catch(stk.om)[,ac((iy+1):(iy+genArgs$management_lag))]
	fsq.inty <- c(yearMeans(fbar(stk.om)[,ac((iy-1):(iy-nsqy))]))
	if(sum(is.na(catch.inty))==0){
		tracking["metric.is", ac((iy-genArgs$management_lag+1):iy)] <- tracking["metric.iem", ac((iy-genArgs$management_lag+1):iy)] <- catch.inty
	} else {
		tracking["metric.is", ac((iy-genArgs$management_lag+1):iy)] <- tracking["metric.iem", ac((iy-genArgs$management_lag+1):iy)] <- 1 #fsq.inty
	}
	
	# SET seed
	if (!is.null(genArgs$seed)) set.seed(genArgs$seed)
  
	# PREPARE objects for loop call
    if (exists(fleetBehaviour(om))) fb <- fleetBehaviour(om) else fb <- NULL 
	
	#============================================================
	# PREPARE for parallel if needed
	if(getDoParWorkers() > 1){
		cat("Going parallel with ", getDoParWorkers(), " cores !\n")

		# LOOP and combine
		lst0 <- foreach(i=1:it, .combine=function(...) {
			list(stk.om=do.call('combine', lapply(list(...), '[[', 'stk.om')),
				tracking=do.call('combine', lapply(list(...), '[[', 'tracking')),
				oem=do.call('combine', lapply(list(...), '[[', 'oem')))
			}, .packages="mse", .multicombine=TRUE, .errorhandling = "stop", .inorder=TRUE) %dopar% {
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
				ctrl= iters(ctrl, i),
				genArgs=genArgs,
				verbose=verbose)
			out <- do.call(mse:::goFish, call0)
			# RETURN
			list(stk.om=out$stk.om, tracking=out$tracking, oem=out$oem)
		}
	} else {
		cat("Going single core !\n")
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
			ctrl=ctrl,
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
	tracking(res) <- window(tracking, end=fy-genArgs$management_lag)
	genArgs(res) <- genArgs
	# TODO accessors
	res@oem <- oem
	res@control <- ctrl
	return(res)
}

goFish <- function(stk.om, sr.om, sr.om.res, sr.om.res.mult, fb, projection, oem, iem, tracking, ctrl, genArgs, verbose){

	it <- genArgs$it
	y0 <- genArgs$y0 # initial data year
	fy <- genArgs$fy # final year
	iy <- genArgs$iy # initial year of projection (also intermediate)
	nsqy <- genArgs$nsqy # number of years to compute status quo metrics
	vy <- genArgs$vy # vector of years to be projected
	data_lag <- genArgs$data_lag
	# need to rename ctrl to avoid breaking code
	ctrl0 <- ctrl

	#============================================================
	# go fish
	for(i in vy[-length(vy)]) {

		gc()
		if(verbose) cat(i, " > ")
		ay <- genArgs$ay <- an(i)
		dy <- genArgs$dy <- ay-data_lag
		sqy <- genArgs$sqy <- ac((ay-data_lag):(ay-nsqy-data_lag+1)) # years for status quo computations 

   		# TRACK om
		tracking["F.om", ac(ay)] <- fbar(stk.om)[,ac(ay)]    
		tracking["B.om", ac(ay)] <- ssb(stk.om)[,ac(ay)]    
		tracking["C.om", ac(ay)] <- catch(stk.om)[,ac(ay)]    

		#==========================================================
		# OEM
		#==========================================================
		#cat("oem\n")
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
		tracking["C.obs",ac(ay)] <- catch(stk0)[,ac(ay-genArgs$data_lag)]

		#==========================================================
		# MP
		# Estimator of stock statistics
		#==========================================================
		#cat("est\n")
		if (!is.null(ctrl0$ctrl.est)){
			ctrl.est <- args(ctrl0$ctrl.est)
			ctrl.est$method <- method(ctrl0$ctrl.est)
			ctrl.est$stk <- stk0
			ctrl.est$idx <- idx0
			ctrl.est$genArgs <- genArgs #ay <- ay
			ctrl.est$tracking <- tracking
			ctrl.est$ioval <- list(iv=list(t1=flsval, t2=flival), ov=list(t1=flsval))
			out.assess <- do.call("mpDispatch", ctrl.est)
			stk0 <- out.assess$stk
			tracking <- out.assess$tracking
		}
		tracking["C.est",ac(ay)] <- catch(stk0)[,ac(ay-genArgs$data_lag)]
		tracking["F.est",ac(ay)] <- fbar(stk0)[,ac(ay-genArgs$data_lag)]
		tracking["B.est",ac(ay)] <- ssb(stk0)[,ac(ay-genArgs$data_lag)]

		#----------------------------------------------------------
		# HCR parametrization
		#----------------------------------------------------------
		#cat("phcr\n")
		if (!is.null(ctrl0$ctrl.phcr)){
			ctrl.phcr <- args(ctrl0$ctrl.phcr)
			ctrl.phcr$method <- method(ctrl0$ctrl.phcr) 
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
		# EJ: don't like this hack but seems to work ...
		# by default stores the first par in tracking
		if(exists("hcrpars")){
			tracking["metric.phcr", ac(ay)] <- hcrpars[1,1,,drop=TRUE]
		 }

		#----------------------------------------------------------
		# HCR
		#----------------------------------------------------------
		#cat("hcr\n")
		if (!is.null(ctrl0$ctrl.hcr)){
			ctrl.hcr <- args(ctrl0$ctrl.hcr)
			ctrl.hcr$method <- method(ctrl0$ctrl.hcr)
			ctrl.hcr$stk <- stk0
			ctrl.hcr$genArgs <- genArgs #ay <- ay
			ctrl.hcr$tracking <- tracking
			if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars
			ctrl.hcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.hcr)
			ctrl <- out$ctrl
			tracking <- out$tracking
		} else {
			ctrl <- getCtrl(yearMeans(fbar(stk0)[,sqy]), "f", ay+genArgs$management_lag, it)
		}
		tracking["metric.hcr", ac(ay)] <- ctrl@trgtArray[ac(ay+genArgs$management_lag),"val",]
		
		#----------------------------------------------------------
		# Implementation system
		#----------------------------------------------------------
		#cat("is\n")
		if (!is.null(ctrl0$ctrl.is)){
			ctrl.is <- args(ctrl0$ctrl.is)
			ctrl.is$method <- method(ctrl0$ctrl.is)
			ctrl.is$ctrl <- ctrl
			ctrl.is$stk <- stk0
			ctrl.is$genArgs <- genArgs #ay <- ay
			ctrl.is$tracking <- tracking
			ctrl.is$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.is)
			ctrl <- out$ctrl
			tracking <- out$tracking
		}		
		tracking["metric.is", ac(ay)] <- ctrl@trgtArray[ac(ay+genArgs$management_lag),"val",]

		#----------------------------------------------------------
		# Technical measures
		#----------------------------------------------------------
		#cat("tm\n")
		if (!is.null(ctrl0$ctrl.tm)){
			ctrl.tm <- args(ctrl0$ctrl.tm)
			ctrl.tm$method <- method(ctrl0$ctrl.tm)
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
		#cat("iem\n")
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
		tracking["metric.iem",ac(ay)] <- ctrl@trgtArray[ac(ay+genArgs$management_lag),"val",]

		#==========================================================
		# OM
		# fleet dynamics/behaviour
		#==========================================================
		#cat("fb\n")
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
		tracking["metric.fb",ac(ay)] <- ctrl@trgtArray[ac(ay+genArgs$management_lag),"val",]

		#----------------------------------------------------------
		# stock dynamics and OM projections
		#----------------------------------------------------------
		#cat("om\n")
		if(!is.null(attr(ctrl, "snew"))) harvest(stk.om)[,ac(ay+1)] <- attr(ctrl, "snew")
		ctrl.om <- args(projection)
		# update with decision made having into account management lag
		ctrl <- getCtrl(tracking["metric.iem",ac(genArgs$ay-genArgs$management_lag+1)], ac(ctrl@target[,"quantity",]), ay+1, it, ctrl@target[,"rel.year",])   		
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

#mp00 <- function(om, oem=FLoem(), iem="missing", ctrl, genArgs, scenario="test", tracking="missing", verbose=TRUE){


#	#============================================================
#	# prepare the om
#	stk.om <- stock(om)	
#	name(stk.om) <- scenario
#	sr.om <- sr(om)
#	sr.om.res <- residuals(sr.om)
#	sr.om.res.mult <- sr.om@logerror
#	fy <- genArgs$fy # final year
#	# dy0 om$startyr | oem$args$
#	y0 <- genArgs$y0 # initial data year
#	# ay - oem$args$datalag
#	#dy <- genArgs$dy # final data year
#	# TODO mlag
#	iy <- genArgs$iy # initial year of projection (also intermediate)
#	nsqy <- genArgs$nsqy # number of years to compute status quo metrics
#	#ny <- fy - iy + 1 # number of years to project from intial year
#	vy <- ac(iy:fy) # vector of years to be projected
#	# om$its
#	it <- dim(stk.om)[6]

#	# INIT tracking
#	metric <- c("F.est", "B.est", "conv.est", "metric.hcr", "metric.is", "metric.iem", "metric.fb","F.om", "B.om", "C.om")
#	
#	if (!missing(tracking)) metric <- c(metric, tracking)
#	tracking <- FLQuant(NA, dimnames=list(metric=metric, year=c(iy-1,vy), iter=1:it))

#	# GET historical
#	tracking["metric.is", ac(iy)] <- catch(stk.om)[,ac(iy)]

#	# set seed
#	if (!is.null(genArgs$seed)) set.seed(genArgs$seed)
#  
#	#============================================================
#	# go fish
#	for(i in vy[-length(vy)]) {

#		gc()
#		if(verbose) cat(i, " > ")
#		ay <- genArgs$ay <- an(i)
#		genArgs$vy0 <- 1:(ay-y0) # data years (positions vector) - one less than current year
#		sqy <- genArgs$sqy <- ac((ay-1):(ay-nsqy)) # years for status quo computations 
#		
#    		# TRACK om
#		tracking["F.om", ac(ay-1)] <- fbar(stk.om)[,ac(ay-1)]    
#		tracking["B.om", ac(ay-1)] <- ssb(stk.om)[,ac(ay-1)]    
#		tracking["C.om", ac(ay-1)] <- catch(stk.om)[,ac(ay-1)]    
#		
#		#==========================================================
#		# OEM
#		#----------------------------------------------------------
#		# function o()
#		ctrl.oem <- args(oem)
#		ctrl.oem$method <- method(oem)
#		ctrl.oem$deviances <- deviances(oem)
#		ctrl.oem$observations <- observations(oem)
#		ctrl.oem$stk <- stk.om
#		ctrl.oem$genArgs <- genArgs #vy0 <- vy0
#		#ctrl.oem$ay <- ay
#		ctrl.oem$tracking <- tracking
#		ctrl.oem$ioval <- list(iv=list(t1=flsval), ov=list(t1=flsval, t2=flival))
#		o.out <- do.call("mpDispatch", ctrl.oem)
#		stk0 <- o.out$stk
#		idx0 <- o.out$idx
#		observations(oem) <- o.out$observations
#		tracking <- o.out$tracking

#		#==========================================================
#		# MP
#		#----------------------------------------------------------
#		# Estimator of stock statistics
#		# function f()
#		if (!is.null(ctrl$ctrl.est)){
#			ctrl.est <- args(ctrl$ctrl.est)
#			ctrl.est$method <- method(ctrl$ctrl.est)
#			ctrl.est$stk <- stk0
#			ctrl.est$idx <- idx0
#			ctrl.est$genArgs <- genArgs #ay <- ay
#			ctrl.est$tracking <- tracking
#			ctrl.est$ioval <- list(iv=list(t1=flsval, t2=flival), ov=list(t1=flsval))
#			out.assess <- do.call("mpDispatch", ctrl.est)
#			stk0 <- out.assess$stk
#			tracking <- out.assess$tracking
#		}
#		tracking["F.est",ac(ay)] <- fbar(stk0)[,ac(ay-1)]
#		tracking["B.est",ac(ay)] <- ssb(stk0)[,ac(ay-1)]
#	

#		#----------------------------------------------------------
#		# HCR parametrization
#		# function x()
#		if (!is.null(ctrl$ctrl.phcr)){
#			ctrl.phcr <- args(ctrl$ctrl.phcr)
#			ctrl.phcr$method <- method(ctrl$ctrl.phcr) 
#			ctrl.phcr$stk <- stk0
#			ctrl.phcr$genArgs <- genArgs #ay <- ay
#			#ctrl.phcr$iy <- iy
#			ctrl.phcr$tracking <- tracking
#			if(exists("hcrpars")) ctrl.phcr$hcrpars <- hcrpars
#			ctrl.phcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flpval))
#			out <- do.call("mpDispatch", ctrl.phcr)
#			hcrpars <- out$hcrpars
#			tracking <- out$tracking
#		}

#		#----------------------------------------------------------
#		# HCR
#		# function h()
#		if (!is.null(ctrl$ctrl.hcr)){
#			ctrl.hcr <- args(ctrl$ctrl.hcr)
#			ctrl.hcr$method <- method(ctrl$ctrl.hcr)
#			ctrl.hcr$stk <- stk0
#			ctrl.hcr$genArgs <- genArgs #ay <- ay
#			ctrl.hcr$tracking <- tracking
#			if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars
#			ctrl.hcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flfval))
#			out <- do.call("mpDispatch", ctrl.hcr)
#			ctrl <- out$ctrl
#			tracking <- out$tracking
#		} else {
#			ctrl <- getCtrl(yearMeans(fbar(stk0)[,sqy]), "f", ay+1, it)
#		}
#		tracking["metric.hcr", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]
#		
#		#----------------------------------------------------------
#		# Implementation system
#		# function k()
#		if (!is.null(ctrl$ctrl.is)){
#			ctrl.is <- args(ctrl$ctrl.is)
#			ctrl.is$method <- method(ctrl$ctrl.is)
#			ctrl.is$ctrl <- ctrl
#			ctrl.is$stk <- stk0
#			ctrl.is$genArgs <- genArgs #ay <- ay
#			ctrl.is$tracking <- tracking
#			ctrl.is$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flfval))
#			out <- do.call("mpDispatch", ctrl.is)
#			ctrl <- out$ctrl
#			tracking <- out$tracking
#			tracking["metric.is", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]
#		} else {
#			tracking["metric.is", ac(ay)] <- tracking["metric.hcr", ac(ay+1)]
#		}

#		#----------------------------------------------------------
#		# Technical measures
#		# function w()
#		if (!is.null(ctrl$ctrl.tm)){
#			ctrl.tm <- args(ctrl$ctrl.tm)
#			ctrl.tm$method <- method(ctrl$ctrl.tm)
#			ctrl.tm$stk <- stk0
#			ctrl.tm$genArgs <- genArgs #sqy <- sqy
#			ctrl.tm$tracking <- tracking
#			ctrl.tm$ioval <- list(iv=list(t1=flsval), ov=list(t1=flqval))
#			out <- do.call("mpDispatch", ctrl.tm)
#			attr(ctrl, "snew") <- out$flq
#			tracking <- out$tracking
#		}

#		#==========================================================
#		# IEM
#		#----------------------------------------------------------
#		if(!missing(iem)){
#			ctrl.iem <- args(iem)
#			ctrl.iem$method <- method(iem)
#			ctrl.iem$ctrl <- ctrl
#			ctrl.iem$genArgs <- genArgs
#			ctrl.iem$tracking <- tracking
#			ctrl.iem$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
#			out <- do.call("mpDispatch", ctrl.iem)
#			ctrl <- out$ctrl
#			tracking <- out$tracking
#		}
#		tracking["metric.iem",ac(ay)] <- ctrl@trgtArray[,"val",]

#		#==========================================================
#		# OM
#		#----------------------------------------------------------
#		# fleet dynamics/behaviour
#		# function j()
#		if (exists(fleetBehaviour(om))){
#			ctrl.fb <- args(fleetBehaviour(om))
#			ctrl.fb$method <- method(fleetBehaviour(om))
#			ctrl.fb$ctrl <- ctrl
#			ctrl.fb$genArgs <- genArgs
#			ctrl.fb$tracking <- tracking
#			ctrl.fb$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
#			out <- do.call("mpDispatch", ctrl.fb)
#			ctrl <- out$ctrl
#			tracking <- out$tracking
#		}
#	    # TODO value()
#		tracking["metric.fb",ac(ay)] <- ctrl@trgtArray[,"val",]

#		#----------------------------------------------------------
#		# stock dynamics and OM projections
#		# function g()
#		if(!is.null(attr(ctrl, "snew"))) harvest(stk.om)[,ac(ay+1)] <- attr(ctrl, "snew")
#		stk.om <- fwd(stk.om, ctrl=ctrl, sr=sr.om, sr.residuals = sr.om.res, sr.residuals.mult = sr.om.res.mult, maxF=2)
#		stk.om <- fwd(stk.om, ctrl=ctrl, sr=sr.om, sr.residuals = sr.om.res, sr.residuals.mult = sr.om.res.mult, maxF=2)
#	}
#	if(verbose) cat("\n")

#	#============================================================
#	res <- as(om, "FLmse")
#	stock(res) <- window(stk.om, start=iy, end=fy)
#	tracking(res) <- window(tracking, end=fy)
#	genArgs(res) <- genArgs
#	# TODO accessors
#	res@oem <- oem
#	res@control <- ctrl
#	return(res)
#}

