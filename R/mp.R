# mp.R - DESC
# mse/R/mp.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# mp {{{

#' mp executes a run of a MP
#'
#' @param om The operating model (OM), an object of class *FLom*.
#' @param oem The observation error model (OEM), an object orf class *FLoem*.
#' @param iem The implementation error model (IEM), an object of class *FLiem*.
#' @param ctrl A control structure for the MP run, an object of class *mpCtrl*.
#' @param args MSE arguments, *list*.
#' @param scenario Name of the scenario tested in this ru, *character*.
#' @param tracking Extra elements to add to the standard tracking *FLQuant*.
#' @param verbose Should output be verbose or not, *logical*.
#'
#' @return An object of class *FLmse*.
#'
#' @examples
#' # [TODO:example]

mp <- function(om, oem=NULL, iem=NULL, ctrl, args, scenario="test", tracking="missing", verbose=TRUE){

	# prepare the om
	stk.om <- stock(om)	
	name(stk.om) <- scenario

	sr.om <- sr(om)
	sr.om.res <- residuals(sr.om)
	sr.om.res.mult <- sr.om@logerror
	
  projection <- projection(om)
	
  # EXTRACT from args
	fy <- args$fy # final year
	iy <- args$iy # initial year of projection (also intermediate)
	nsqy <- args$nsqy # sq years
	vy <- args$vy <- ac(iy:fy) # vector of years to be projected
	# om$its
	it <- args$it <- dims(stk.om)$iter

	# SET lags
	# data lag = time in years between the data and the year 
	#  the assessment is performed (ay).
	# management lag = time in years between the year the 
	#  assessment is performed (ay) and the implementation of the management action.
	if (is.null(args$data_lag)) args$data_lag <- 1
	if (is.null(args$management_lag)) args$management_lag <- 1

	# INIT tracking
	metric <- c("C.obs", "F.est", "B.est", "C.est", "conv.est", "metric.phcr",
    "metric.hcr", "metric.is", "metric.iem", "metric.fb","F.om", "B.om", "C.om")

	if (!missing(tracking))
    metric <- c(tracking, metric)
	
  tracking <- FLQuant(NA, dimnames=list(metric=metric,
    year=unique(c((iy-args$management_lag+1):iy,vy)), iter=1:it))
	
  # GET historical from OM 
	# TODO intermediate year function
	catch.inty <- catch(stk.om)[, ac((iy+1):(iy+args$management_lag))]
	fsq.inty <- c(yearMeans(fbar(stk.om)[, ac((iy-1):(iy-nsqy))]))

  #
	if(sum(is.na(catch.inty))==0){
		tracking["metric.is", ac((iy-args$management_lag+1):iy)] <-
      tracking["metric.iem", ac((iy-args$management_lag+1):iy)] <- catch.inty
	} else {
		tracking["metric.is", ac((iy-args$management_lag+1):iy)] <-
      tracking["metric.iem", ac((iy-args$management_lag+1):iy)] <- 1 #fsq.inty
	}
	
	# SET seed
	if (!is.null(args$seed)) set.seed(args$seed)
  
	# PREPARE objects for loop call
  if (exists(fleetBehaviour(om)))
    fb <- fleetBehaviour(om)
  else 
    fb <- NULL 

  # SETUP default oem
	if(is.null(oem)){
		flqdc <- catch.n(stock(om))
		flqdc[] <- 1

		stkDev <- FLQuants(catch.n=flqdc)
		flqdi <- stock.n(stock(om))
		flqdi[] <- 1
		
    idxDev <- FLQuants(index.q=flqdi)
		dev <- list(idx=idxDev, stk=stkDev)
		idx <- FLIndex(index=stock.n(stock(om)))
		range(idx)[c("startf", "endf")] <- c(0, 0)
		
    obs <- list(idx=FLIndices(stkn=idx), stk=stock(om))
		oem <- FLoem(method=perfect.oem, observations=obs, deviances=dev)
	}

	#============================================================
	# PREPARE for parallel if needed
	if(getDoParWorkers() > 1){
		cat("Going parallel with ", getDoParWorkers(), " cores !\n")
		# LOOP and combine
		lst0 <- foreach(j=1:it, 
			.combine=function(...) {
				list(
					stk.om=do.call('combine', lapply(list(...), '[[', 'stk.om')),
					tracking=do.call('combine', lapply(list(...), '[[', 'tracking')),
					oem=do.call('combine', lapply(list(...), '[[', 'oem'))
				)
			}, 
			.packages="mse", 
			.multicombine=TRUE, 
			.errorhandling = "stop", 
			.inorder=TRUE) %dopar% {
				# in case of parallel each core receives one iter
				args$it <- 1
				call0 <- list(
					stk.om = stk.om[,,,,,j],
					sr.om = FLCore::iter(sr.om,j),
					sr.om.res = sr.om.res[,,,,,j],
					oem = iters(oem, j),
					tracking = tracking[,,,,,j],
					sr.om.res.mult=sr.om.res.mult,
					fb=fb, # needs it selection
					projection=projection,
					iem=iem, # needs it selection
					ctrl= iters(ctrl, j),
					args=args,
					verbose=verbose)
				out <- do.call(goFish, call0)
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
				args=args,
				verbose=verbose)
			out <- do.call(goFish, call0)
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
	tracking(res) <- window(tracking, end=fy-args$management_lag)
	args(res) <- args
	
  # TODO accessors
	res@oem <- oem
	res@control <- ctrl
	return(res)
}

# goFish

goFish <- function(stk.om, sr.om, sr.om.res, sr.om.res.mult, fb,
  projection, oem, iem, tracking, ctrl, args, verbose){

	it <- args$it
	y0 <- args$y0 # initial data year
	fy <- args$fy # final year
	iy <- args$iy # initial year of projection (also intermediate)
	nsqy <- args$nsqy # number of years to compute status quo metrics
	vy <- args$vy # vector of years to be projected
	data_lag <- args$data_lag
	# need to rename ctrl to avoid breaking code
	ctrl0 <- ctrl

	#============================================================
	# go fish
	for(i in vy[-length(vy)]) {

		gc()
		if(verbose) cat(i, " > ")
		ay <- args$ay <- an(i)
		dy <- args$dy <- ay-data_lag
    # years for status quo computations 
		sqy <- args$sqy <- ac((ay-data_lag):(ay-nsqy-data_lag+1))

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
		ctrl.oem$args <- args #vy0 <- vy0
		#ctrl.oem$ay <- ay
		ctrl.oem$tracking <- tracking
		ctrl.oem$ioval <- list(iv=list(t1=flsval), ov=list(t1=flsval, t2=flival))
		o.out <- do.call("mpDispatch", ctrl.oem)
		stk0 <- o.out$stk
		idx0 <- o.out$idx
		observations(oem) <- o.out$observations
		tracking <- o.out$tracking
		tracking["C.obs",ac(ay)] <- catch(stk0)[,ac(ay-args$data_lag)]

		#==========================================================
		# EST
		# Estimator of stock statistics
		#==========================================================
		#cat("est\n")
		if (!is.null(ctrl0$est)){
			ctrl.est <- args(ctrl0$est)
			ctrl.est$method <- method(ctrl0$est)
			ctrl.est$stk <- stk0
			ctrl.est$idx <- idx0
			ctrl.est$args <- args #ay <- ay
			ctrl.est$tracking <- tracking
			ctrl.est$ioval <- list(iv=list(t1=flsval, t2=flival), ov=list(t1=flsval))
			out.assess <- do.call("mpDispatch", ctrl.est)
			stk0 <- out.assess$stk
			tracking <- out.assess$tracking
		}
		tracking["C.est",ac(ay)] <- catch(stk0)[,ac(ay-args$data_lag)]
		tracking["F.est",ac(ay)] <- fbar(stk0)[,ac(ay-args$data_lag)]
		tracking["B.est",ac(ay)] <- ssb(stk0)[,ac(ay-args$data_lag)]

		#----------------------------------------------------------
		# HCR parametrization
		#----------------------------------------------------------
		#cat("phcr\n")
		if (!is.null(ctrl0$phcr)){
			ctrl.phcr <- args(ctrl0$phcr)
			ctrl.phcr$method <- method(ctrl0$phcr) 
			ctrl.phcr$stk <- stk0
			ctrl.phcr$args <- args #ay <- ay
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
		if (!is.null(ctrl0$hcr)){
			ctrl.hcr <- args(ctrl0$hcr)
			ctrl.hcr$method <- method(ctrl0$hcr)
			ctrl.hcr$stk <- stk0
			ctrl.hcr$args <- args #ay <- ay
			ctrl.hcr$tracking <- tracking
			if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars
			ctrl.hcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.hcr)
			ctrl <- out$ctrl
			tracking <- out$tracking
		} else {
			ctrl <- getCtrl(yearMeans(fbar(stk0)[,sqy]), "f", ay+args$management_lag, it)
		}
		tracking["metric.hcr", ac(ay)] <- ctrl$value[1,]
		
		#----------------------------------------------------------
		# Implementation system
		#----------------------------------------------------------
		#cat("is\n")
		if (!is.null(ctrl0$isys)){
			ctrl.is <- args(ctrl0$isys)
			ctrl.is$method <- method(ctrl0$isys)
			ctrl.is$ctrl <- ctrl
			ctrl.is$stk <- stk0
			ctrl.is$args <- args #ay <- ay
			ctrl.is$tracking <- tracking
			ctrl.is$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.is)
			ctrl <- out$ctrl
			tracking <- out$tracking
		}		
		tracking["metric.is", ac(ay)] <- ctrl$value[1,]

		#----------------------------------------------------------
		# Technical measures
		#----------------------------------------------------------
		#cat("tm\n")
		if (!is.null(ctrl0$tm)){
			ctrl.tm <- args(ctrl0$tm)
			ctrl.tm$method <- method(ctrl0$tm)
			ctrl.tm$stk <- stk0
			ctrl.tm$args <- args #sqy <- sqy
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
			ctrl.iem$args <- args
			ctrl.iem$tracking <- tracking
			ctrl.iem$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.iem)
			ctrl <- out$ctrl
			tracking <- out$tracking
		}
		tracking["metric.iem",ac(ay)] <- ctrl$value[1,]

		#==========================================================
		# OM
		# fleet dynamics/behaviour
		#==========================================================
		#cat("fb\n")
		if (!is.null(fb)){
			ctrl.fb <- args(fb)
			ctrl.fb$method <- method(fb)
			ctrl.fb$ctrl <- ctrl
			ctrl.fb$args <- args
			ctrl.fb$tracking <- tracking
			ctrl.fb$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
			out <- do.call("mpDispatch", ctrl.fb)
			ctrl <- out$ctrl
			tracking <- out$tracking
		}
	  # TODO value()
		tracking["metric.fb",ac(ay)] <- ctrl$value[1,]

		#----------------------------------------------------------
		# stock dynamics and OM projections
		#----------------------------------------------------------
		#cat("om\n")
		if(!is.null(attr(ctrl, "snew"))) harvest(stk.om)[,ac(ay+1)] <- 
      attr(ctrl, "snew")
		ctrl.om <- args(projection)
		# update with decision made having into account management lag
		#ctrl <- getCtrl(tracking["metric.iem", ac(args$ay-args$management_lag+1)],
    #  ac(ctrl$quant), ay+1, it, ctrl$relYear)   		
		ctrl.om$ctrl <- ctrl
		ctrl.om$stk <- stk.om
		ctrl.om$sr <- sr.om
		ctrl.om$deviances <- sr.om.res
		# ctrl.om$sr.residuals.mult <- sr.om.res.mult
		ctrl.om$method <- method(projection)
		ctrl.om$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flsval))
		stk.om <- do.call("mpDispatch", ctrl.om)$object

	}
	list(stk.om=stk.om, tracking=tracking, oem=oem, args=args)
}
