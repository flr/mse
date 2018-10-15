# mp.R - DESC
# mse/R/mp.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# mp {{{

mp <- function(opModel, obsModel=FLoem(), impModel="missing", ctrl.mp, genArgs, 
  scenario="test", tracking="missing"){


	#============================================================
	# prepare the om
	stk.om <- stock(opModel)	
	name(stk.om) <- scenario
	sr.om <- sr(opModel)
	sr.om.res <- residuals(sr.om)
	sr.om.res.mult <- sr.om@logerror
	fy <- genArgs$fy # final year
	y0 <- genArgs$y0 # initial data year
	dy <- genArgs$dy # final data year
	iy <- genArgs$iy # initial year of projection (also intermediate)
	nsqy <- genArgs$nsqy # number of years to compute status quo metrics
	ny <- fy - iy + 1 # number of years to project from intial year
	vy <- ac(iy:fy) # vector of years to be projected

	# init tracking
	tracking0 <- FLQuant(NA, dimnames=list(metric=c("F.est", "B.est", "conv.est",
    "F.hcr", "metric.is", "metric.iem", "metric.fb","F.om", "B.om", "C.om"),
    year=c(iy-1,vy), iter=1:it))
	if (missing(tracking)) tracking <- tracking0 else tracking <- rbind(tracking0, tracking)

	# get historical 	
	tracking["metric.is", ac(iy)] <- catch(stk.om)[,ac(iy)]

	# set seed
	if (!is.null(genArgs$seed)) set.seed(genArgs$seed)
  
	#============================================================
	# go fish
	for(i in vy[-length(vy)]) {

		gc()
		ay <- an(i)
		cat(i, " > ")
		vy0 <- 1:(ay-y0) # data years (positions vector) - one less than current year
		sqy <- ac((ay-1):(ay-nsqy)) # years for status quo computations 
		
		tracking["F.om", ac(ay-1)] <- fbar(stk.om)[,ac(ay-1)]    
		tracking["B.om", ac(ay-1)] <- ssb(stk.om)[,ac(ay-1)]    
		tracking["C.om", ac(ay-1)] <- catch(stk.om)[,ac(ay-1)]    
		
		#==========================================================
		# OEM
		#----------------------------------------------------------
		# function o()
		ctrl.oem <- args(obsModel)
		ctrl.oem$method <- method(obsModel)
		ctrl.oem$deviances <- deviances(obsModel)
		ctrl.oem$observations <- observations(obsModel)
		ctrl.oem$stk <- stk.om
		ctrl.oem$vy0 <- vy0
		ctrl.oem$ay <- ay
		ctrl.oem$tracking <- tracking
		ctrl.oem$ioval <- list(iv=list(t1=flsval), ov=list(t1=flsval, t2=flival))
		o.out <- do.call("mpDispatch", ctrl.oem)
		stk0 <- o.out$stk
		idx0 <- o.out$idx
		observations(obsModel) <- o.out$observations
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
			ctrl.phcr$ay <- ay
			ctrl.phcr$iy <- iy
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
			ctrl.hcr$ay <- ay
			ctrl.hcr$tracking <- tracking
			if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars
			ctrl.hcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.hcr)
			ctrl <- out$ctrl
			tracking <- out$tracking
		} else {
			ctrl <- getCtrl(yearMeans(fbar(stk0)[,sqy]), "f", ay+1, it)
		}
		tracking["F.hcr", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]
		
		#----------------------------------------------------------
		# Implementation system
		# function k()
		if (!is.null(ctrl.mp$ctrl.is)){
			ctrl.is <- args(ctrl.mp$ctrl.is)
			ctrl.is$method <- method(ctrl.mp$ctrl.is)
			ctrl.is$ctrl <- ctrl
			ctrl.is$stk <- stk0
			ctrl.is$ay <- ay
			ctrl.is$tracking <- tracking
			ctrl.is$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.is)
			ctrl <- out$ctrl
			tracking <- out$tracking
			tracking["metric.is", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]
		} else {
			tracking["metric.is", ac(ay)] <- tracking["F.hcr", ac(ay+1)]
		}

		#----------------------------------------------------------
		# Technical measures
		# function w()
		if (!is.null(ctrl.mp$ctrl.tm)){
			ctrl.tm <- args(ctrl.mp$ctrl.tm)
			ctrl.tm$method <- method(ctrl.mp$ctrl.tm)
			ctrl.tm$stk <- stk0
			ctrl.tm$sqy <- sqy
			ctrl.tm$tracking <- tracking
			ctrl.tm$ioval <- list(iv=list(t1=flsval), ov=list(t1=flqval))
			out <- do.call("mpDispatch", ctrl.tm)
			attr(ctrl, "snew") <- out$flq
			tracking <- out$tracking
		}

		#==========================================================
		# IEM
		#----------------------------------------------------------
		if(!missing(impModel)){
			ctrl.iem <- args(impModel)
			ctrl.iem$method <- method(impModel)
			ctrl.iem$ctrl <- ctrl
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
		if (exists(fleetBehaviour(opModel))){
			ctrl.fb <- args(fleetBehaviour(opModel))
			ctrl.fb$method <- method(fleetBehaviour(opModel))
			ctrl.fb$tracking <- tracking
			ctrl.fb$ctrl <- ctrl
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

	}
    cat("\n")

	#============================================================
    mp <- as(opModel, "FLmse")
    stock(mp) <- stk.om
    tracking(mp) <- tracking
    genArgs(mp) <- genArgs
	mp
}

