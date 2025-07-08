# mp.R - DESC
# mse/R/mp.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# TODO: ADD performance calculation

# mp {{{

#' mp executes a single run of a Management Procedure
#'
#' An individual management procedure (MP) is run for a number of years,
#' on an operating model, observation error model, control and arguments.
#'
#' @param om The operating model (OM), an object of class *FLom* or *FLombf*.
#' @param oem The observation error model (OEM), an object of class *FLoem*.
#' @param iem The implementation error model (IEM), an object of class *FLiem*.
#' @param ctrl A control structure for the MP run, an object of class *mpCtrl*.
#' @param args MSE arguments, *list*. Only 'iy', the intermediate (starting) year, is required.
#' @param scenario Name of the scenario tested in this run, *character*.
#' @param tracking Extra elements (rows) to add to the standard tracking *FLQuant* in its first dimensions, *character*.
#' @param verbose Should output be verbose or not, *logical*.
#'
#' @return An object of class *FLmse*.
#'
#' @examples
#' # dataset contains both OM (FLom) and OEM (FLoem)
#' data(sol274)
#' # Set control: sa and hcr
#' control <- mpCtrl(list(
#'   est = mseCtrl(method=shortcut.sa),
#'   hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
#'   trigger=41500, target=0.27))))
#' tes <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2034))
#' tes3 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2034, frq=3))
#' plot(om, list(annual=tes, triannual=tes3))
#' # 'perfect.oem' is used if none is given
#' tes <- mp(om, ctrl=control, args=list(iy=2021, fy=2035))
#' plot(om, tes)

mp <- function(om, oem=NULL, iem=NULL, control=ctrl, ctrl=control, args,
  scenario="NA", tracking="missing", logfile=tempfile(),
  verbose=!handlers(global = NA), parallel=TRUE) {

  # PARSE parallel options
  cores <- nbrOfWorkers()

  # USE parallel is set as numeric
  if(is.numeric(parallel)) {
    cores <- parallel
    parallel <- TRUE
  }

  # SET future.globals.maxSize if not set already
  if(is.null(options('future.globals.maxSize')[[1]])) {
    oldopt <- options(future.globals.maxSize=1500 * 1024 ^ 2)
    on.exit(options(oldopt))
  }
   
  # SETUP default oem
  if(is.null(oem)){
    oem <- default.oem(om)
    missingoem <- TRUE
  } else if(is(oem, "function")){
    oem <- FLoem(method=oem)
    missingoem <- TRUE
  } else {
    missingoem <- FALSE
  }
 
  # APPLY recursively to om list
  if(is(om, 'list')) {
    if(!is(oem, 'list')) {
      oem <- lapply(setNames(nm=names(om)), function(x) oem)
    }
    res <- foreach(i=seq(length(om)), .combine="c") %do% {
      mp(om[[i]], oem=oem[[i]], iem=iem,     
        control=control, args=args, scenario=scenario, tracking=tracking, 
        logfile=logfile, verbose=verbose, parallel=parallel)
    }
    return(FLmses(res))
  }

  # dims & dimnames
  dis <- dims(om)
  dmns <- dimnames(om)

  # --- EXTRACT args

  iy <- args$iy

  if(is.null(iy))
    stop("Intermediate year (iy) missing in 'args'.")

  # y0, defaults to minyear
  y0 <- args$y0 <- if(is.null(args$y0)) dis$minyear else args$y0

  # fy, defaults to maxyear
  fy <- args$fy <- if(is.null(args$fy)) dis$maxyear else args$fy

  # CHECK fy > iy
  if(fy <= iy)
    stop("Final year (fy) must be greater than intermediate year (iy).")
  
  # nsq, defaults to 3
  nsqy <- args$nsqy <- if(is.null(args$nsqy)) 3 else args$nsqy

  # it, om$iter
  it <- args$it <- dis$iter

  # data_lag: time in years between data and assessment year (ay)
  data_lag <- args$data_lag <- if(is.null(args$data_lag)) 1 else args$data_lag

  # management_lag: time in years between ay and implementation
  management_lag <- args$management_lag <-
    if(is.null(args$management_lag)) 1 else args$management_lag
  
  # frq defaults to 1
  frq <- args$frq <- if(is.null(args$frq)) 1 else args$frq

  # vector of years to be projected
  vy <- args$vy <- ac(seq(iy, fy - management_lag, by=frq))
  
  # CHECK proj years do not extend beyond maxyear
  if(an(vy[length(vy)]) + frq > dis$maxyear) {
    args$vy <- vy <- vy[-length(vy)]
  }

  # number of seasons & units
  ns <- args$ns <- dis$season
  nu <- args$nu <- dis$unit

  # --- RUN checks on inputs

  # TODO CHECK control: c('est', 'hcr') %in% names(control)

  # TODO CHECK iy is correct
  # TODO dims stk, idx
  # TODO check deviances years
  # TODO check iters om vs. ctrl
  # TODO stk(om) and units: merge

  # --- INIT tracking
  
  metric <- c(
    # om
    "B.om", "SB.om", "C.om", "F.om",
    # oem
    "B.obs", "SB.obs", "C.obs",
    # est
    "B.est", "SB.est", "C.est", "F.est", "conv.est",
    # iem
    "iem")
  steps <- c("phcr", "hcr", "isys", "tm")

  if (!missing(tracking))
    metric <- c(metric, tracking)

  # SETUP tracking FLQs
  tracking <- FLQuant(NA, dimnames=list(
    metric=c(metric, steps[steps %in% names(ctrl)], "fb", "fwd", "time", "pid"),
    year=ac(seq(iy - data_lag - frq + 1, fy - management_lag + frq)),
    unit="unique",
    season=dmns$season,
    iter=1:args$it))

  # TODO: 
  if(is(om, "FLombf")) {
    tracking <- FLQuants(setNames(rep(list(tracking), length(names(biols(om)))),
      names(biols(om))))
  } else if (is(om, "FLom")){
    tracking <- FLQuants(A=tracking)
  }

  # GET historical from OM DEBUG different from original
  # hyrs <- ac(c(iy - args$management_lag + 1, iy))
  # track(tracking, "F.om", hyrs) <- window(catch(om), start=hyrs[1], end=hyrs[2])
  
  # SET seed
  if (!is.null(args$seed))
    seed <- args$seed
  else
    seed <- TRUE
  
  # CHECK and WARN if fb in control
  if("fb" %in% names(control))
     stop("control contains an 'fb' element, should be fleetBehaviour(om)")

  # SET fleetBehaviour to NULL if not given
  if (exists(fleetBehaviour(om)))
    fb <- fleetBehaviour(om)
  else 
    fb <- NULL 

  # SETUP default oem
  if(is.null(oem)){
    oem <- perfect.oem(om)
    missingoem <- TRUE
  } else {
    missingoem <- FALSE
  }
  
    # RUN goFish

  if(isTRUE(parallel) & cores > 1) {

    # SPLIT iters along cores
    its <- split(seq(it), sort(seq(it) %% cores))
    
    message("Running on ", nbrOfWorkers(), " nodes.")

    # LOOP and combine
    lst0 <- foreach(j=its, 
      .combine=.combinegoFish,
      .multicombine=TRUE, 
      .errorhandling = "remove", 
      .options.future=list(seed=seed, globals=structure(TRUE,
        add=c("ctrl", "module", "om", "oem", "iem", "args"),
        packages=c("FLCore", "FLasher", "mse"))),
      .inorder=TRUE) %dofuture% {

        call0 <- list(
          om = iter(om, j),
          oem = iter(oem, j),
          tracking = iter(tracking, j),
          fb=iter(fb, j),
          projection=projection(om),
          iem=iter(iem, j),
          ctrl= iter(ctrl, j),
          args=c(args[!names(args) %in% "it"], it=length(j)),
          verbose=verbose,
          logfile=logfile)

        out <- do.call(goFish, call0)

        # CHECK output
        if(!all(names(out) == c("om", "tracking", "oem", "args")))
          stop("Output of individual core is not correct")
        out
      }
    } else {
      
      call0 <- list(
        om = om,
        oem = oem,
        tracking = tracking,
        fb=fb,
        projection=projection(om),
        iem=iem,
        ctrl=ctrl,
        args=args,
        verbose=verbose,
        logfile=logfile)

      out <- do.call(goFish, call0)

      lst0 <- list(om=out$om, tracking=out$tracking, oem=out$oem)
    }

  # CHECK outputs
  if(!is(lst0$om, "FLo"))
    stop("goFish returned no results")

  # GET objects back from loop, from iy - 1 up to last projected year

  # om
  om <- window(lst0$om, start=iy - 1, end=an(vy[length(vy)]) + frq)

  # oem
  if(missingoem)
    oem <- FLoem()
  else
    oem <- window(lst0$oem, start=iy, end=an(vy[length(vy)]) + frq)

  # tracking
  tracking <- window(lst0$tracking, start=an(iy) - data_lag,
    end=an(vy[length(vy)]) + frq)

  # END year print
  if(verbose) cat("\n")

  # --- RETURN
  res <- new("FLmse", om=om, args=args, oem=oem, control=ctrl,
    tracking = tracking)
  
  return(res)
}

# }}}

# goFish FLom {{{

setMethod("goFish", signature(om="FLom"),
  function(om, fb, projection, oem, iem, tracking, logfile, ctrl, args,
    verbose) {
  
  # ARGUMENTS
  it <- args$it     # number of iterations
  y0 <- args$y0     # initial data year
  fy <- args$fy     # final year
  iy <- args$iy     # initial year of projection (also intermediate)
  vy <- args$vy     # vector of years to be projected
  nsqy <- args$nsqy # years for status quo calculations
  dlag <- args$data_lag  # years between assessment and last data
  mlag <- args$management_lag # years between assessment and management
  frq <- args$frq   # frequency

  # CHECK inputs
  dom <- dimnames(stock(om))
  dst <- dimnames(observations(oem, "stk"))

  # LOGFILE header
  cat("pid", "year", dimnames(tracking[[1]])[[1]], sep="\t", "\n",
    file=logfile, append=TRUE)

  # COPY ctrl
  ctrl0 <- ctrl
  
  p <- progressor(along=vy, offset=0L)

  # go fish!

  for(i in vy) {

    # time (start)
    stim <- Sys.time()
  
    if(verbose){
      cat(i, " - ")
    }

    # args
    ay <- args$ay <- an(i)
    dy <- args$dy <- ay - dlag
    dys <- seq(ay - dlag - frq + 1, ay - dlag)
    dy0 <- dys[1]
    dyf <- dys[frq]
    mys <- seq(ay + mlag, ay + mlag + frq - 1)
    
    # years for status quo computations 
    sqy <- args$sqy <- ac(seq(ay - nsqy - dlag + 1, dy))
    
    # TRACK om
    track(tracking, "F.om", dys) <- unitMeans(window(fbar(om),
      start=dy0, end=dyf))
    track(tracking, "B.om", dys) <- unitSums(window(tsb(om),
      start=dy0, end=dyf))
    track(tracking, "SB.om", dys) <- unitSums(window(ssb(om),
      start=dy0, end=dyf))
    track(tracking, "C.om", dys) <- unitSums(window(catch(om),
      start=dy0, end=dyf))
    
    # --- OEM: Observation Error Model
    ctrl.oem <- args(oem)
    ctrl.oem$method <- method(oem)
    ctrl.oem$deviances <- deviances(oem)
    ctrl.oem$observations <- observations(oem)
    ctrl.oem$stk <- stock(om)
    ctrl.oem$args <- args
    ctrl.oem$tracking <- tracking
    ctrl.oem$ioval <- list(iv=list(t1=flsval), ov=list(t1=flsval, t2=flival))
    ctrl.oem$step <- "oem"
  
    o.out <- tryCatch(do.call("mpDispatch", ctrl.oem),
      error = function(e){
        message("Call to oem method failed, check inputs")
        print(e)
      }
    )
    
    stk0 <- o.out$stk
    idx0 <- o.out$idx
    observations(oem) <- o.out$observations
    tracking <- o.out$tracking
    
    track(tracking, "B.obs", dys) <- unitSums(window(stock(stk0),
      start=dy0, end=dyf))
    track(tracking, "SB.obs", dys) <- unitSums(window(ssb(stk0),
      start=dy0, end=dyf))
    track(tracking, "C.obs", dys) <- unitSums(window(catch(stk0),
      start=dy0, end=dyf))

    # --- est: Estimator of stock statistics

    if (!is.null(ctrl0$est)) {
      ctrl.est <- unclass(args(ctrl0$est))
      ctrl.est$method <- method(ctrl0$est)
      ctrl.est$stk <- stk0
      ctrl.est$idx <- idx0
      ctrl.est$args <- args #ay <- ay
      ctrl.est$tracking <- tracking
      ctrl.est$ioval <- list(iv=list(t1=flsval, t2=flival), ov=list(t1=flsval))
      ctrl.est$step <- "est"

      # DISPATCH
      out.assess <- tryCatch(do.call("mpDispatch", ctrl.est),
        # ERROR in whole set of iters
        error = function(e){
          message("Call to est method failed, check inputs")
          print(e)
        }
      )

      stk0 <- out.assess$stk
      
      # EXTRACT ind(icators) if returned
      if(!is.null(out.assess$ind)) {
        ind <- out.assess$ind
        # TRACK indicators
      } else {
        ind <- FLQuants()
      }
      
      # PASS args generated at est to ctrl for future runs
      if (!is.null(out.assess$args)) {
        args(ctrl0$est)[names(out.assess$args)] <-
          out.assess$args
      }
      tracking <- out.assess$tracking
    }

    # TODO: DO NOT WRITE if ind
    track(tracking, "F.est", dys) <- unitMeans(window(fbar(stk0),
      start=dy0, end=dyf))
    track(tracking, "B.est", dys) <- unitSums(window(stock(stk0),
      start=dy0, end=dyf))
    track(tracking, "SB.est", dys) <- unitSums(window(ssb(stk0),
      start=dy0, end=dyf))
    track(tracking, "C.est", dys) <- unitSums(window(catch(stk0),
      start=dy0, end=dyf))

    # --- phcr: HCR parameterization
    
    if (!is.null(ctrl0$phcr)){
      
      ctrl.phcr <- args(ctrl0$phcr)
      ctrl.phcr$method <- method(ctrl0$phcr) 
      ctrl.phcr$stk <- stk0
      ctrl.phcr$ind <- ind
      ctrl.phcr$args <- args
      ctrl.phcr$tracking <- tracking
      if(exists("hcrpars")) ctrl.phcr$hcrpars <- hcrpars
      ctrl.phcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flpval))
      ctrl.phcr$step <- "phcr"
      
      out <- do.call("mpDispatch", ctrl.phcr)
      
      hcrpars <- out$hcrpars
      tracking <- out$tracking
    }

    if(exists("hcrpars")){
      # TODO
      track(tracking, "phcr", dys) <- c(hcrpars[1,])
     }

    # --- hcr: Harvest Control Rule

    if (!is.null(ctrl0$hcr)){

      ctrl.hcr <- args(ctrl0$hcr)
      ctrl.hcr$method <- method(ctrl0$hcr)
      ctrl.hcr$stk <- stk0
      ctrl.hcr$args <- args #ay <- ay
      ctrl.hcr$tracking <- tracking
      ctrl.hcr$ind <- ind

      # TODO REVIEW interface
      if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars

      ctrl.hcr$ioval <- list(iv=list(t1=flsval, t2=flqsval), 
        ov=list(t1=flfval))
      
      ctrl.hcr$step <- "hcr"
      
      out.hcr <- tryCatch(do.call("mpDispatch", ctrl.hcr),
        error = function(e){
          message("Call to hcr method failed, check inputs")
          print(e)
        })
      
      ctrl <- out.hcr$ctrl
      tracking <- out.hcr$tracking
    } else {
      # DEFAULTS to F = mean(Fbar) over nsqy years
      ctrl <- as(FLQuants(fbar=expand(yearMeans(fbar(stk0)[, sqy]), 
        year=mys)), "fwdControl")
    }
    
    # tracking multiple targets/limits, one year
    track(tracking, "hcr", mys) <- ctrl

    #----------------------------------------------------------
    # Implementation system
    #----------------------------------------------------------
    if (!is.null(ctrl0$isys)){

      ctrl.is <- args(ctrl0$isys)
      ctrl.is$method <- method(ctrl0$isys)
      ctrl.is$ctrl <- ctrl
      ctrl.is$stk <- stk0
      ctrl.is$args <- args #ay <- ay
      ctrl.is$tracking <- tracking
      ctrl.is$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flfval))
      ctrl.is$step <- "isys"

      out <- do.call("mpDispatch", ctrl.is)
      
      ctrl <- out$ctrl
      tracking <- out$tracking

      track(tracking, "isys", mys) <- ctrl
    }    

    #----------------------------------------------------------
    # Technical measures
    #----------------------------------------------------------
    if (!is.null(ctrl0$tm)){

      ctrl.tm <- args(ctrl0$tm)
      ctrl.tm$method <- method(ctrl0$tm)
      ctrl.tm$stk <- stk0
      ctrl.tm$args <- args #sqy <- sqy
      ctrl.tm$tracking <- tracking
      ctrl.tm$ioval <- list(iv=list(t1=flsval), ov=list(t1=flqval))
      ctrl.ym$step <- "tm"
      
      out <- do.call("mpDispatch", ctrl.tm)
      
      attr(ctrl, "snew") <- out$flq
      tracking <- out$tracking

      track(tracking, "tm", mys) <- ctrl
    }

    #==========================================================
    # IEM
    #==========================================================
    if(!is.null(iem)){
      
      ctrl.iem <- args(iem)
      ctrl.iem$method <- method(iem)
      ctrl.iem$ctrl <- ctrl
      ctrl.iem$args <- args
      ctrl.iem$tracking <- tracking
      ctrl.iem$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
      ctrl.iem$step <- "iem"

      out <- do.call("mpDispatch", ctrl.iem)
      
      ctrl <- out$ctrl
      tracking <- out$tracking
      
      track(tracking, "iem", mys) <- ctrl
    }

    #==========================================================
    # FB
    # fleet dynamics/behaviour
    #==========================================================
    if (!is.null(fb)){
      
      ctrl.fb <- args(fb)
      ctrl.fb$method <- method(fb)
      ctrl.fb$ctrl <- ctrl
      ctrl.fb$args <- args
      ctrl.fb$tracking <- tracking
      ctrl.fb$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
      ctrl.fb$step <- "fb"

      out <- do.call("mpDispatch", ctrl.fb)
      ctrl <- out$ctrl
      tracking <- out$tracking
      
      track(tracking, "fb", mys) <- ctrl
    }

    #----------------------------------------------------------
    # stock dynamics and OM projections
    #----------------------------------------------------------
    ctrl.om <- args(projection)
    ctrl.om$ctrl <- ctrl
    ctrl.om$om <- om
    ctrl.om$method <- method(projection)
    ctrl.om$deviances <- residuals(sr(om))
    ctrl.om$ioval <- list(iv=list(t1=floval), ov=list(t1=floval))
    ctrl.om$step <- "om"
    
    out <- tryCatch(do.call("mpDispatch", ctrl.om),
      error = function(e) {
        message("Call to om projection method failed, check inputs")
        print(e)})

    om <- out$om

    # final control
    track(tracking, "fwd", mys) <- ctrl[1,]
    
    # time (in minutes, per iter)   
    track(tracking, "time", ay) <- as.numeric(difftime(Sys.time(), stim,
      units = "mins")) / args$it

    # CPU process   
    id <- Sys.getpid()
    track(tracking, "pid", ay) <- id

    # OUTPUT summary to logfile
    lapply(dys, function(x)
      cat(id, x, c(iterMeans(tracking[[1]][, ac(x)])), "\n", sep="\t",
        file=logfile, append=TRUE))
    
    # REPORT progress
    p(message = sprintf("year: %s", i))
 
    cat(id, paste0("[", ay, "]"), c(iterMeans(tracking[[1]][, ac(ay)])),
      "\n", sep="\t", file=logfile, append=TRUE)

    # CLEAR memory
    # gc()
  }
  
  # TRACK om in final years
  fys <- seq(dyf, ay + mlag + frq - 1)
  track(tracking, "F.om", fys) <- unitMeans(fbar(om))[, ac(fys)]
  track(tracking, "B.om", fys) <- unitSums(tsb(om))[, ac(fys)]
  track(tracking, "SB.om", fys) <- unitSums(ssb(om))[, ac(fys)]
  track(tracking, "C.om", fys) <- unitSums(catch(om))[, ac(fys)]

  # RETURN
  list(om=om, tracking=tracking, oem=oem, args=args)
  } 
)
# }}}

# goFish FLombf {{{

setMethod("goFish", signature(om="FLombf"),
  function(om, fb, projection, oem, iem, tracking, ctrl, args,
    verbose, logfile) {

  it <- args$it     # number of iterations
  y0 <- args$y0     # initial data year
  fy <- args$fy     # final year
  iy <- args$iy     # initial year of projection (also intermediate)
  vy <- args$vy     # vector of years to be projected
  nsqy <- args$nsqy # years for status quo calculations
  dlag <- args$data_lag  # years between assessment and last data
  mlag <- args$management_lag # years between assessment and management
  frq <- args$frq   # frequency
  bns <- names(biols(om))
  fns <- names(fisheries(om))

  # CHECK oem mode (byfishery)
  byfishery <- isTRUE(args(oem)$byfishery)

  # TODO LOOP every module over stock
  if(is.null(args$stock))
    args$stock <- seq(length(biols(om)))

  # COPY ctrl
  ctrl0 <- ctrl

  # go fish
  if(length(deviances(oem)) == 0)
    deviances(oem) <- rep(list(NULL), length(biols(om)))

  p <- progressor(along=vy, offset=0L)

  for(i in vy) {

    if(verbose) {
      cat(i, " > ")
    }

    # REPORT progress
    p(message = sprintf("year: %s", i))
    
    # time (start)
    stim <- Sys.time()

    # args
    ay <- args$ay <- an(i)
    dy <- args$dy <- ay - dlag
    dys <- seq(ay - dlag - frq + 1, ay - dlag)
    dy0 <- dys[1]
    dyf <- dys[frq]
    mys <- seq(ay + mlag, ay + mlag + frq - 1)
    
    # years for status quo computations 
    sqy <- args$sqy <- ac(seq(ay - nsqy - dlag + 1, dy))
    
    # TRACK om
    track(tracking, "F.om", dys) <- unitMeans(window(fbar(om),
      start=dy0, end=dyf))
    track(tracking, "B.om", dys) <- unitSums(window(tsb(om),
      start=dy0, end=dyf))
    track(tracking, "SB.om", dys) <- unitSums(window(ssb(om),
      start=dy0, end=dyf))
    track(tracking, "C.om", dys) <- unitSums(window(catch(om),
      start=dy0, end=dyf))
    
    # --- OEM: Observation Error Model

    # COMMON elements
    ctrl.oem <- args(oem)
    ctrl.oem$method <- method(oem)
    ctrl.oem$args <- args
    ctrl.oem$ioval <- list(iv=list(t1=flsval), ov=list(t1=flsval, t2=flival))
    ctrl.oem$step <- "oem"

    # GET OM observation
    stk <- window(stock(om, full=TRUE, byfishery=byfishery), end=dy)

    # TODO:
    # names(tracking) <- names(stk)
    # names(observations(oem)) <- names(stk)

    # APPLY oem across stocks
    o.out <- Map(function(stk, dev, obs, tra) {

      obs.oem <- do.call("mpDispatch", c(ctrl.oem, list(stk=stk, deviances=dev,
        observations=obs, tracking=FLQuants(tra))))

      # TODO: PICK UP fbar range from observations
      # range(obs.oem$stk, c("minfbar", "maxfbar")) <- 
      #  range(obs$stk, c("minfbar", "maxfbar")) 

      return(obs.oem)
    }, stk=stk, obs=observations(oem)[names(stk)],
      dev=deviances(oem)[names(stk)], tra=tracking[names(stk)])

    # EXTRACT oem observations TODO: CLEAN & ADD methods
    stk0 <- FLStocks(lapply(o.out, "[[", "stk"))
    idx0 <- lapply(o.out, "[[", "idx")

    tracking <- FLQuants(lapply(o.out, function(x) x$tracking[[1]]))
    
    observations(oem) <- lapply(o.out, "[[", "observations")
    
    # TRACK oem catch
    track(tracking, "B.obs", dys) <- lapply(window(stk0, start=dy0, end=dyf),
      function(x) areaSums(unitSums(stock(x))))
    track(tracking, "SB.obs", dys) <- lapply(window(stk0, start=dy0, end=dyf),
      function(x) areaSums(unitSums(ssb(x))))
    track(tracking, "C.obs", dys) <- lapply(window(stk0, start=dy0, end=dyf),
      function(x) areaSums(unitSums(catch(x))))

    # --- est: Estimator of stock statistics

    if (!is.null(ctrl0$est)) {

      ctrl.est <- args(ctrl0$est)
      ctrl.est$method <- method(ctrl0$est)
      ctrl.est$args <- args
      ctrl.est$step <- "est"

      # SET empty, to be replaced
      ind <- lapply(setNames(nm=bns), function(x) FLQuants())

      # SET stocks to est on
      if(is.null(args$stock)) {
        args$stock <- seq(length(stk0))
      }

      # CALL est with multiple stocks
      if(identical(args$stock, 'all')) {

        # SET dispatch rules
        ctrl.est$ioval <- list(iv=list(t1=flssval, t2=flival), 
          ov=list(t1=flssval))
        
        # DISPATCH
        out.assess <- do.call("mpDispatch", c(ctrl.est,
          list(stk=stk0, idx=idx0[[1]], tracking=tracking)))

        # EXTRACT FLStocks
        stk0 <- out.assess$stk

        # EXTRACT indicators
        ind <- out.assess$ind

        # EXTRACT tracking
        tracking <- out.assess$tracking

      # OR stock by stock
      } else {

        ctrl.est$ioval <- list(iv=list(t1=flsval, t2=flival), 
          ov=list(t1=flsval))
# BUG:
        out.assess <- Map(function(x, y, z) {
          do.call("mpDispatch", c(ctrl.est, list(stk=x, idx=y, tracking=z)))
          }, x=stk0[args$stock], y=idx0[args$stock], z=tracking[args$stock])
      
        stk0 <- FLStocks(lapply(out.assess, "[[", "stk"))
      
        if(!is.null(out.assess[[1]]$ind))
          ind <- lapply(out.assess, "[[", "ind")
        
        tracking[args$stock] <- FLQuants(lapply(out.assess, "[[", "tracking"))
      }

      # TODO: PASS args generated at est to ctrl
      if (!is.null(out.assess$args)) {
        args(ctrl0$est)[names(out.assess$args)] <- out.assess$args
      }
    }

    # TRACK est TODO: MAKE robust
    track(tracking, "F.est", seq(ay, ay + frq - 1)) <- 
      lapply(window(lapply(stk0, fbar), start=dy, end=dy + frq - 1), unitMeans)
    track(tracking, "B.est", seq(ay, ay + frq - 1)) <- 
      lapply(window(lapply(stk0, stock), start=dy, end=dy + frq - 1), unitSums)
    track(tracking, "SB.est", seq(ay, ay + frq - 1)) <- 
      lapply(window(lapply(stk0, ssb), start=dy, end=dy + frq - 1), unitSums)
    track(tracking, "C.est", seq(ay, ay + frq - 1)) <- 
      lapply(window(lapply(stk0, catch), start=dy, end=dy + frq - 1),
        function(x) areaSums(unitSums(x)))

    # --- phcr: HCR parameterization
    
    if (!is.null(ctrl0$phcr)){

      ctrl.phcr <- args(ctrl0$phcr)
      ctrl.phcr$method <- method(ctrl0$phcr) 
      ctrl.phcr$stk <- stk0
      ctrl.phcr$args <- args
      ctrl.phcr$tracking <- tracking
      if(exists("hcrpars")) ctrl.phcr$hcrpars <- hcrpars
      ctrl.phcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flpval))
      ctrl.phcr$step <- "phcr"
      
      out <- do.call("mpDispatch", ctrl.phcr)

      hcrpars <- out$hcrpars
      tracking <- out$tracking
    }

    # TODO REVIEW & TEST
    if(exists("hcrpars")){
      track(tracking, "metric.phcr", seq(ay, ay+frq-1)) <-
        hcrpars[1, 1,, drop=TRUE]
     }

    # --- hcr: Harvest Control Rule
    
    if (!is.null(ctrl0$hcr)){

      ctrl.hcr <- args(ctrl0$hcr) 
      ctrl.hcr$method <- method(ctrl0$hcr)

      # SELECT stock for hcr
      if(args$stock == 'all') {
        ctrl.hcr$stk <- stk0[[1]]
        ctrl.hcr$ind <- ind
      } else if(length(args$stock) == 1) {
        ctrl.hcr$stk <- stk0[[args$stock]]
        ctrl.hcr$ind <- ind[[args$stock]]
      } else {
        ctrl.hcr$stk <- stk0
        ctrl.hcr$ind <- ind
      }

      ctrl.hcr$args <- args
      ctrl.hcr$tracking <- tracking

      if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars
      
      ctrl.hcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flfval))
      ctrl.hcr$step <- "hcr"
      
      out <- do.call("mpDispatch", ctrl.hcr)

      ctrl <- out$ctrl

      # ASSIGN biol BUG: FIX for stock='all', SET as I(1:2)?
      if(all(is.na(ctrl$biol)))
       ctrl$biol <- args$stock
      
      # COUNT targets with 'f' or 'fbar', need minAge, maxAge
      fbis <- target(ctrl)[ctrl$quant %in% c("f", "fbar"),]

      # BUG: SET fbar ages if missing
      if(nrow(fbis) > 0) {
        
        # GET fbar ranges
        frgs <- lapply(stk0, range, c("minfbar", "maxfbar"))
        
        # CHANGE on those missing
        for(i in unique(fbis$biol)) {
          fbis[fbis$biol == i, c("minAge", "maxAge")]  <- frgs[[i]]
        }
        # ASSIGN back into ctrl
        target(ctrl)[ctrl$quant %in% c("f", "fbar"),] <- fbis
      }

     tracking <- out$tracking
    } else {
      # BUG: DROP getCtrl
      ctrl <- getCtrl(yearMeans(fbar(stk0)[,sqy]), "f", ay + args$management_lag, it)
    }
    
    track(tracking, "hcr", mys) <- ctrl
    
    #----------------------------------------------------------
    # Implementation system
    #----------------------------------------------------------
    if (!is.null(ctrl0$isys)){

      ctrl.is <- args(ctrl0$isys)
      ctrl.is$method <- method(ctrl0$isys)
      ctrl.is$ctrl <- ctrl

      # SELECT stock for hcr
      if(args$stock == 'all')
        ctrl.is$stk <- stk0
      else if(length(args$stock) == 1)
        ctrl.is$stk <- stk0[[args$stock]]
      else
        ctrl.is$stk <- stk0

      ctrl.is$args <- args #ay <- ay
      ctrl.is$tracking <- tracking
      #
      if(length(ctrl.is$stk) == 1) {
        ctrl.is$ioval <- list(iv=list(t1=flsval, t2=flfval),
          ov=list(t1=flfval))
      } else {
        ctrl.is$ioval <- list(iv=list(t1=flssval, t2=flfval), 
          ov=list(t1=flfval))
      }
      
      ctrl.is$step <- "isys"

      out <- do.call("mpDispatch", ctrl.is)
      
      ctrl <- out$ctrl

      tracking <- out$tracking

      # BUG: DEAL with multirow ctrl
      track(tracking, "isys", mys) <- ctrl[1,]
    }    

    #----------------------------------------------------------
    # Technical measures
    #----------------------------------------------------------
    if (!is.null(ctrl0$tm)){

      ctrl.tm <- args(ctrl0$tm)
      ctrl.tm$method <- method(ctrl0$tm)
      ctrl.tm$stk <- stk0
      ctrl.tm$args <- args #sqy <- sqy
      ctrl.tm$tracking <- tracking
      ctrl.tm$ioval <- list(iv=list(t1=flsval), ov=list(t1=flqval))
      ctrl.ym$step <- "tm"
      
      out <- do.call("mpDispatch", ctrl.tm)
      
      attr(ctrl, "snew") <- out$flq
      tracking <- out$tracking

      track(tracking, "tm", seq(ay, ay+frq-1)) <- ctrl
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
      ctrl.iem$step <- "iem"

      out <- do.call("mpDispatch", ctrl.iem)
      
      ctrl <- out$ctrl
      tracking <- out$tracking
      # TODO: ADD sum of ctrl      
      track(tracking, "iem", seq(ay, ay+frq-1)) <- ctrl[1,]
    }

    #==========================================================
    # FB
    # fleet dynamics/behaviour
    #==========================================================
    #cat("fb\n")
    if (!is.null(ctrl0$fb)){

      ctrl.fb <- args(ctrl0$fb)
      ctrl.fb$method <- method(ctrl0$fb)
      ctrl.fb$ctrl <- ctrl
      ctrl.fb$args <- args
      ctrl.fb$tracking <- tracking
      ctrl.fb$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
      ctrl.fb$step <- "fb"

      out <- do.call("mpDispatch", ctrl.fb)

      ctrl <- out$ctrl
      tracking <- out$tracking
      
      track(tracking, "fb", seq(ay, ay + frq - 1)) <- ctrl
    }

    #----------------------------------------------------------
    # stock dynamics and OM projections
    #----------------------------------------------------------
    ctrl.om <- args(projection)
    ctrl.om$ctrl <- ctrl
    ctrl.om$om <- om
    ctrl.om$method <- method(projection)
    # DEBUG
    # ctrl.om$deviances <- residuals(sr(om))
    ctrl.om$ioval <- list(iv=list(t1=floval), ov=list(t1=floval))
    ctrl.om$step <- "om"

    om <- do.call("mpDispatch", ctrl.om)$om

    # BUG:
    # track(tracking, "fwd", seq(ay, ay+frq-1)) <- ctrl
    # time (end)   
    track(tracking, "time", ay) <- as.numeric(Sys.time()) -     
      tracking[[1]]["time", i]
    id <- Sys.getpid()
    track(tracking, "pid", ay) <- id

    invisible(gc())
  }

    # RETURN
    list(om=om, tracking=tracking, oem=oem, args=args)
  }
) # }}}

# mps {{{

# TODO: mps(FLmse, oem=oem(), crtrl=control(), args=args(), ...)

# statistics, metrics, years - om=name(om), type, run=names,

mps <- function(om, oem=NULL, iem=NULL, ctrl, args, names=NULL, parallel=TRUE,
  ...) {

  # GET ... arguments
  opts <- list(...)
  
  # SET seed
  if (!is.null(args$seed))
    seed <- args$seed
  else
    seed <- TRUE
  
  # ARE opts being given?
  if(length(opts) == 0)
    return(FLmses(RUN=mp(om=om, oem=oem, iem=iem, ctrl=ctrl, args=args,
      parallel=parallel)))

  # PARSING a single module
  if(length(opts) > 1)
    stop("mps() can only alter a single module, called for: ", names(opts))
  
  # DO options refer to ctrl elements?
  if(!names(opts) %in% names(ctrl))
    stop("options refer to modules not present in ctrl")

  # PARSE options on first element (module)
  module <- names(opts)[[1]]

  # MAX number of values
  largs <- max(unlist(lapply(opts[[module]], length)))
 
  # RECYCLE if shorter
  mopts <- lapply(opts[[module]], function(i) {
    rep(i, length=largs)
  })

  # LOOP over values

  if(parallel) {

    message("Running on ", nbrOfWorkers(), " nodes.")

    p <- progressor(along=seq(largs), offset=0L)

    res <- foreach(i = seq(largs), .errorhandling="pass",
      .options.future=list(globals=structure(TRUE, add=c("ctrl", "module",
      "mopts", "om", "oem", "iem", "args"), seed=seed))) %dofuture% {

      # MODIFY module args
      args(ctrl[[module]])[names(mopts)] <- lapply(mopts, "[", i)

      # CALL mp, parallel left to work along MPs
      run <- mp(om, oem=oem, iem=iem, ctrl=ctrl, args=args, parallel=FALSE,
         verbose=FALSE)
      
      p(message = sprintf("MP: %s", i))

      return(run)
    }
  } else {

    p <- progressor(along=seq(largs), offset=0L)

    res <- lapply(seq(largs), function(i) {

      # MODIFY module args
      args(ctrl[[module]])[names(mopts)] <- lapply(mopts, '[', i)

      # CALL mp, parallel left to work along MPs
      run <- mp(om, oem=oem, iem=iem, ctrl=ctrl, args=args, parallel=TRUE,
         verbose=FALSE)

      p(message = sprintf("MP: %s", i))

      return(run)
    })
  }

  # STOP or WARN if missing runs
  done <- unlist(lapply(res, is, "FLmse"))

  if(sum(done) == 0)
    stop("None of the calls to mp() returned results, check inputs")

  if(sum(done) < largs)
    warning(paste("Some calls to mp() did not run:"), seq(largs)[!done])

  # CREATE standard names 
  onms <- paste(module, names(mopts)[1], round(mopts[[1]], 2), sep='_')

  # RENAME list elements
  if(is.null(names)) {
    # NO names and one element changed? USE standard
    if(length(mopts) == 1)
      names(res) <- onms
    # OR sequence if more than one element
    else
      names(res) <- paste(module, seq(largs), sep='_')
  # IF names given
  } else {
    # PASTE to standard if one
    if(length(names) == 1)
      names(res) <- paste(names, onms, sep="-")
    # USE as supplied
    else
      names(res) <- names
  }

  return(FLmses(res[done]))
}
# }}}
