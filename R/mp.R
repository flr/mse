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

mp <- function(om, oem=NULL, iem=NULL, ctrl, args, scenario="test",
  tracking="missing", verbose=TRUE, parallel=TRUE){

  # --- EXTRACT args

  iy <- args$iy

  if(is.null(iy))
    stop("Intermediate year (iy) missing in 'args'.")

  # y0, defaults to minyear
  y0 <- args$y0 <- if(is.null(args$y0)) dims(om)$minyear else args$y0

  # fy, defaults to maxyear
  fy <- args$fy <- if(is.null(args$fy)) dims(om)$maxyear else args$fy
  
  # nsq, defaults to 3
  nsqy <- args$nsqy <- if(is.null(args$nsqy)) 3 else args$nsqy

  # vector of years to be projected
	vy <- args$vy <- ac(seq(iy, fy))

	# it, om$iter
	it <- args$it <- dims(om)$iter

  # data_lag: time in years between data and assessment year (ay)
  data_lag <- args$data_lag <- if(is.null(args$data_lag)) 1 else args$data_lag

  # management_lag: time in years between ay and implementation
  management_lag <- args$management_lag <-
    if(is.null(args$management_lag)) 1 else args$management_lag
  
  # frq defaults to 1
  frq <- args$frq <- if(is.null(args$frq)) 1 else args$frq

	# --- INIT tracking

	metric <- c("C.obs", "F.est", "B.est", "C.est", "conv.est",
    "F.om", "B.om", "C.om", "iem")
  steps <- c("phcr", "hcr", "isys", "tm", "fb")

	if (!missing(tracking))
    metric <- c(tracking, metric)

  # TODO MULTIPLY for FLombf
  tracking <- FLQuants(FLQuant(NA, dimnames=list(
    metric=c(metric, steps[steps %in% names(ctrl)]),
    year=unique(c((iy - args$management_lag + 1):iy, vy)),
    iter=1:args$it)))

  # GET historical from OM DEBUG different fron original
  hyrs <- ac(c(iy - args$management_lag + 1, iy))
  track(tracking, "F.om", hyrs) <- window(catch(om), start=hyrs[1], end=hyrs[2])
	
	# SET seed
	if (!is.null(args$seed)) set.seed(args$seed)
  
	# PREPARE objects for loop call
  projection <- projection(om)

  # SET fleetBehaviour to NULL if not given
  if (exists(fleetBehaviour(om)))
    fb <- fleetBehaviour(om)
  else 
    fb <- NULL 

  # SETUP default oem
	if(is.null(oem)){
    oem <- default.oem(om)
	}

	# PREPARE for parallel if needed
  cores <- getDoParWorkers()

	if(isTRUE(parallel) & cores > 1){

		cat("Going parallel with ", cores, " cores !\n")

    its <- split(seq(it), sort(seq(it) %% 2))

    # LOOP and combine
		lst0 <- foreach(j=its, 
			.combine=function(...) {
				list(
          # TODO combine(FLom)
					om=do.call('combine', lapply(list(...), '[[', 'om')),
					tracking=do.call('combine', lapply(list(...), '[[', 'tracking')),
					oem=do.call('combine', lapply(list(...), '[[', 'oem'))
				)
			}, 
			.packages="mse", 
			.multicombine=TRUE, 
			.errorhandling = "pass", 
			.inorder=TRUE) %dopar% {

				call0 <- list(
          om = iter(om, j),
					oem = iter(oem, j),
          tracking = iter(tracking[[1]], j),
					fb=fb, # needs it selection
					projection=projection,
					iem=iem, # needs it selection
					ctrl= iters(ctrl, j),
					args=args,
					verbose=verbose)

        browser()

				out <- do.call(goFish, call0)
				list(stk.om=out$stk.om, tracking=out$tracking, oem=out$oem)
			}
		} else {

			cat("Going single core !\n")

			call0 <- list(
				om = om,
				oem = oem,
				tracking = tracking,
				fb=fb,
				projection=projection,
				iem=iem,
				ctrl=ctrl,
				args=args,
				verbose=verbose)
      
			out <- do.call(goFish, call0)
			lst0 <- list(om=out$om, tracking=out$tracking, oem=out$oem)
		}
	
  # GET objects back from loop
	om <- lst0$om
	tracking <- lst0$tracking
	oem <- lst0$oem

	if(verbose) cat("\n")

	# --- RETURN
  res <- new("FLmse", om=om, args=args, oem=oem, control=ctrl,
    tracking = tracking)
	
	return(res)
}

# }}}

setGeneric("goFish", function(om, ...)
    standardGeneric('goFish'))

# goFish(FLombf) {{{

setMethod("goFish", signature(om="FLo"),
  function(om, fb, projection, oem, iem, tracking, ctrl, args, verbose) {
  
  it <- args$it     # number of iterations
	y0 <- args$y0     # initial data year
	fy <- args$fy     # final year
	iy <- args$iy     # initial year of projection (also intermediate)
	nsqy <- args$nsqy # number of years to compute status quo metrics
	vy <- args$vy     # vector of years to be projected
	data_lag <- args$data_lag  # years between assessment and last data

  # COPY ctrl
	ctrl0 <- ctrl

	# go fish
	
  for(i in vy) {
		
    if(verbose) cat(i, " > ")

    ay <- args$ay <- an(i)
		dy <- args$dy <- ay - data_lag
    
    # years for status quo computations 
		sqy <- args$sqy <- ac(seq(ay - nsqy - data_lag + 1, dy))
    
    # TRACK om
    track(tracking, "F.om", ay) <- window(fbar(om), start=dy, end=dy)
    track(tracking, "B.om", ay) <- window(ssb(om), start=dy, end=dy)
    track(tracking, "C.om", ay) <- window(catch(om), start=dy, end=dy)
    
    # --- OEM: Observation Error Model

		ctrl.oem <- args(oem)
		ctrl.oem$method <- method(oem)
		ctrl.oem$deviances <- deviances(oem)
		ctrl.oem$observations <- observations(oem)
		ctrl.oem$om <- om
		ctrl.oem$args <- args
		ctrl.oem$tracking <- tracking
		ctrl.oem$ioval <- list(iv=list(t1=floval), ov=list(t1=flssval, t2=flival))
	
    o.out <- do.call("mpDispatch", ctrl.oem)

    stk0 <- o.out$stk
		idx0 <- o.out$idx
		observations(oem) <- o.out$observations
		tracking <- o.out$tracking

    track(tracking, "C.obs", ay) <- window(catch(om),
      start=ac(ay-args$data_lag), end=ac(ay-args$data_lag))

		# --- EST: Estimator of stock statistics
    if (!is.null(ctrl0$est)) {
			ctrl.est <- args(ctrl0$est)
			ctrl.est$method <- method(ctrl0$est)
			ctrl.est$stk <- stk0
			ctrl.est$idx <- idx0
			ctrl.est$args <- args #ay <- ay
			ctrl.est$tracking <- tracking
			ctrl.est$ioval <- list(iv=list(t1=flssval, t2=flival), ov=list(t1=flssval))
      
      out.assess <- do.call("mpDispatch", ctrl.est)
      
      stk0 <- out.assess$stk
      
      # PASS args generated at est to ctrl
      if (!is.null(out.assess$args)) {
        args(ctrl0$est)[names(out.assess$args)] <-
          out.assess$args
      }
			tracking <- out.assess$tracking
		}

    track(tracking, "F.est", ay) <- window(fbar(stk0), start=dy, end=dy)
    track(tracking, "B.est", ay) <- window(ssb(stk0), start=dy, end=dy)
    track(tracking, "C.est", ay) <- window(catch(stk0), start=dy, end=dy)

		# --- HCR parametrization
		
    if (!is.null(ctrl0$phcr)){
			ctrl.phcr <- args(ctrl0$phcr)
		  ctrl.phcr$method <- method(ctrl0$phcr) 
			ctrl.phcr$stk <- stk0
			ctrl.phcr$args <- args
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

		# --- Harvest Control Rule
		if (!is.null(ctrl0$hcr)){
			ctrl.hcr <- args(ctrl0$hcr)
			ctrl.hcr$method <- method(ctrl0$hcr)
			ctrl.hcr$stk <- stk0
			ctrl.hcr$args <- args #ay <- ay
			ctrl.hcr$tracking <- tracking
			if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars
			ctrl.hcr$ioval <- list(iv=list(t1=flssval), ov=list(t1=flfval))

			out <- do.call("mpDispatch", ctrl.hcr)
			
      ctrl <- out$ctrl
			tracking <- out$tracking
		} else {
			ctrl <- getCtrl(yearMeans(fbar(stk0)[,sqy]), "f", ay+args$management_lag, it)
    }

    track(tracking, "hcr", ay) <- ctrl

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

      track(tracking, "isys", ay) <- ctrl
		}		

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

      track(tracking, "tm", ay) <- ctrl
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
      
      track(tracking, "iem", ay) <- ctrl
		}

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
      
      track(tracking, "fb", ay) <- ctrl
		}

		#----------------------------------------------------------
		# stock dynamics and OM projections
		#----------------------------------------------------------
    
    # DEBUG WHY this?
    if(!is.null(attr(ctrl, "snew"))) harvest(stk.om)[,ac(ay+1)] <- 
      attr(ctrl, "snew")

		# DEBUG update with decision made having into account management lag
		#ctrl <- getCtrl(tracking["metric.iem", ac(args$ay-args$management_lag+1)],
    #  ac(ctrl$quant), ay+1, it, ctrl$relYear)

    # APPLY management_lag
    ctrl$year <- args$ay - args$management_lag + 1
		
		ctrl.om <- args(projection)
    ctrl.om$ctrl <- ctrl
		ctrl.om$om <- om
		ctrl.om$method <- method(projection)
		ctrl.om$ioval <- list(iv=list(t1=floval), ov=list(t1=floval))
    
    om <- do.call("mpDispatch", ctrl.om)$object

		gc()
	}
  
  # RETURN
	list(om=window(om, start=iy, end=fy), tracking=tracking, oem=oem, args=args)

  }
) # }}}
