# mp.R - DESC
# mse/R/mp.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# mp {{{

#' mp executes a single run of a Management Procedure
#'
#' An individual management procedure (MP) defined by the `control` argument, is run
#' under a temporal configuration defined in the `args` list, and on a given operating
#' model (`om`). Particular observation error (`oem`), and implementation error model
#' (`iem`) elements can also be specified.
#'
#' @details
#' Calls to `mp()` can be run in parallel using `%dofuture%`. Iterations in the `om` are
#' split across the number of availabnle workers, as set by a call to `plan()`. This can 
#' specially improve computations for MPs fitting any kind of model or doing other
#' non-vectorizsed calculations.
#'
#' Progress in the simulation is reported via a progress bar as set by the `progressr` 
#' package, if a global handler is set up using `handlers(global=TRUE)`. This bar works
#' when a parallel plan has been set up. Otherwise, a simple print out of the year being 
#' run is shown.
#'
#' The `args` list controls the timing of the simulation and its elements:
#' - iy: the initial year of simulation inn which decisions are made. The only required element in `args`.
#' - fy: final year, defaults to last year in `om` object.
#' - y0: first data year, defaults to first year in `om` object.
#' - nsqy: number of years for status-quo calculations, defaults to 3.
#' - data_lag: number of years between last data point and decision year, defaults to 1.
#' - management_lag: number of years between decision and its application. Must be greater than 0, and defaults to 1.
#' - frq: frequendcy of advice in years, defaults to 1.
#' - vy: vector of years in which advice is givenm, defaults to a sequence between `iy` and `fy` every `frq` years.
#'
#' @param om The operating model (OM), an object of class *FLom* or *FLombf*.
#' @param oem The observation error model (OEM), an object of class *FLoem*.
#' @param iem The implementation error model (IEM), an object of class *FLiem*.
#' @param ctrl A control structure for the MP run, an object of class *mpCtrl*.
#' @param args MSE arguments, *list*. Only 'iy', the intermediate (starting) year, is required.
#' @param scenario Name of the scenario tested in this run, *character*.
#' @param tracking Extra elements (rows) to add to the standard tracking *FLQuant* in its first dimensions, *character*. Elements can also be added by individual control modules.
#' @param verbose Should output be verbose (year being executed) or not, *logical*.
#'
#' @return An object of class *FLmse*, trimmed to start in year `iy` unless `window` is set to`FALSE`. 
#'
#' @examples
#' # dataset contains both OM (FLom) and OEM (FLoem)
#' data(plesim)
#' # Set control: sa and hcr
#' control <- mpCtrl(list(
#'   est = mseCtrl(method=perfect.sa),
#'   hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
#'   trigger=14000, target=0.18))))
#' # Runs mp
#' tes <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2034))
#' # Runs mp with triannual management
#' tes3 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2034, frq=3))
#' # Compare both runs
#' plot(om, list(annual=tes, triannual=tes3))
#' # 'perfect.oem' is used if none is given
#' tes <- mp(om, ctrl=control, args=list(iy=2021, fy=2035))
#' plot(om, tes)

mp <- function(om, oem=NULL, iem=NULL, control=ctrl, ctrl=control, args,
  scenario="NA", tracking="missing", verbose=!handlers(global = NA),   
  progress=handlers(global = NA), parallel=TRUE, 
  window=TRUE, .DEBUG=FALSE) {

  # GET do.future workers
  cores <- nbrOfWorkers()

  # USE parallel if set as numeric
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
        verbose=verbose, parallel=parallel)
    }
    return(FLmses(res))
  }

  # dims & dimnames
  dis <- dims(om)
  dmns <- dimnames(om)

  # --- EXTRACT and BUILD args

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

  # STOP if management_lag < 1, OM won't progress
  if(management_lag < 1)
    stop("'management_lag' must be 1 or greater.")
  
  # frq defaults to 1
  frq <-  args$frq <- if(is.null(args$frq)) 1 else args$frq

  # vector of years on which to run mp
  vy <- args$vy <- ac(seq(iy, fy - management_lag - frq + 1, by=frq))
  
  # CHECK proj years do not extend beyond maxyear
  if(an(vy[length(vy)]) + frq > dis$maxyear) {
    args$vy <- vy <- vy[-length(vy)]
  }

  # number of seasons & units
  ns <- args$ns <- dis$season
  nu <- args$nu <- dis$unit

  # --- RUN checks on inputs

  # TODO: CATCH case of refpts with 1 iter
  # refpts(om) <- propagate(refpts(om), dims(om)$it)

  # TODO CHECK control: c('est', 'hcr') %in% names(control)

  # TODO CHECK iy is correct
  # TODO dims stk, idx
  # TODO check deviances years
  # TODO check iters om vs. ctrl
  # TODO stk(om) and units: merge

  # --- INIT tracking
  
  # TRACKS set in goFish
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

  # USER added tracks
  if (!missing(tracking))
    metric <- c(metric, tracking)

  # GET names of biols / stock
  bnames <- if(is(om, "FLombf")) names(biols(om)) else name(stock(om))
  
  # BUILD data.table
  tracking <- do.call(CJ, list(biol=bnames,
    metric=c(metric, steps[steps %in% names(ctrl)], "fb", "fwd", "time", "pid"),
    year=vy,
    # TODO: SEASONAL management?
    # season="all",
    iter=1:args$it,
    data=as.numeric(NA), sorted=FALSE))

  # SET seed
  if (!is.null(args$seed))
    seed <- args$seed
  else
    seed <- TRUE
  
  # CHECK and WARN if fb in control
  if("fb" %in% names(control))
     stop("control contains an 'fb' element, should be placed in fleetBehaviour(om)")

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
          tracking = tracking[iter %in% j],
          fb=iter(fb, j),
          projection=projection(om),
          iem=iter(iem, j),
          ctrl= iter(ctrl, j),
          args=c(args[!names(args) %in% "it"], it=length(j)),
          verbose=verbose,
          progress=progress,
          .DEBUG=.DEBUG)

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
        progress=progress,
        .DEBUG=.DEBUG)

      out <- do.call(goFish, call0)

      lst0 <- list(om=out$om, tracking=out$tracking, oem=out$oem)
    }

  # CHECK outputs
  if(!is(lst0$om, "FLo"))
    stop("goFish returned no results")

  # GET objects back from loop, from iy - 1 up to last projected year

  # om
  if(window)
    om <- window(lst0$om, start=iy - 1, end=an(vy[length(vy)]) + frq)
  else
    om <- lst0$om

  # oem
  if(missingoem)
    oem <- FLoem()
  else
    if(window)
      oem <- window(lst0$oem, start=iy, end=an(vy[length(vy)]) + frq)
    else
      oem <- lst0$oem

  # tracking
  tracking <- lst0$tracking[!is.na(data),]

  # END year print
  if(verbose) cat("\n")

  # --- RETURN
  res <- new("FLmse", om=om, args=args, oem=oem, control=ctrl,
    tracking = tracking)
  
  return(res)
}

# }}}

# goFish(FLom) {{{

setMethod("goFish", signature(om="FLom"),
  function(om, fb, projection, oem, iem, tracking, ctrl, args,
    verbose, progress, .DEBUG) {

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
  args$stock <- 1

  # CHECK inputs
  dom <- dimnames(stock(om))
  dst <- dimnames(observations(oem, "stk"))

  # COPY ctrl
  ctrl0 <- ctrl
  
  if(progress)
    p <- progressor(steps=length(vy) + 1)

  # go fish!

  for(i in vy) {

  if(.DEBUG)
    browser()

    # time (start)
    stim <- Sys.time()
  
    # REPORT progress
    if(progress)
      p(message = sprintf("year: %s", i))

    if(verbose){
      cat(i, " - ")
    }

    # args
    ay <- args$ay <- an(i)
    # data years
    dy <- args$dy <- ay - dlag
    dys <- args$dys <- seq(ay - dlag - frq + 1, ay - dlag)
    dy0 <- args$dy0 <- dys[1]
    # management years
    mys <- args$mys <- seq(ay + mlag, ay + mlag + frq - 1)
    
    # years for status quo computations 
    sqy <- args$sqy <- ac(seq(ay - nsqy - dlag + 1, dy))

    # TRACK om, status in ay
    track(tracking, "F.om", ay) <- unitMeans(window(fbar(om),
      start=ay, end=ay))
    track(tracking, "B.om", ay) <- unitSums(window(tsb(om),
      start=ay, end=ay))
    track(tracking, "SB.om", ay) <- unitSums(window(ssb(om),
      start=ay, end=ay))
    track(tracking, "C.om", ay) <- unitSums(window(catch(om),
      start=ay, end=ay))
    
    # --- OEM: Observation Error Model
    ctrl.oem <- args(oem)
    ctrl.oem$method <- method(oem)
    ctrl.oem$deviances <- deviances(oem)
    ctrl.oem$observations <- observations(oem)
    
    # MATCH dim(stk) to observations$stk
    sdi <- dim(observations(oem)$stk)[c(3, 4, 5)] == 1
    ctrl.oem$stk <- simplify(stock(om), c("unit", "season", "area")[sdi])

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

    track(tracking, "B.obs", ay) <- unitSums(window(tsb(stk0),
      start=dy, end=dy))
    track(tracking, "SB.obs", ay) <- unitSums(window(ssb(stk0),
      start=dy, end=dy))
    track(tracking, "C.obs", ay) <- unitSums(window(catch(stk0),
      start=dy, end=dy))

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

      # GET tracking
      tracking <- out.assess$tracking
      
      # EXTRACT ind(icators) if returned
      if(!is.null(out.assess$ind)) {
        
        ind <- out.assess$ind

        # TRACK indicators
        for(i in names(ind))
          track(tracking, paste0(i, ".ind"), ay) <- window(ind[[i]], start=dy, end=dy)

      } else {
        ind <- FLQuants()
      }
      
      # PASS args generated at est to ctrl for future runs
      if (!is.null(out.assess$args)) {
        args(ctrl0$est)[names(out.assess$args)] <-
          out.assess$args
      }
    } else {
      stop("'control' must contain an 'est' mseCtrl element")
    }

    track(tracking, "F.est", ay) <- unitMeans(window(fbar(stk0),
      start=dy, end=dy))
    track(tracking, "B.est", ay) <- unitSums(window(tsb(stk0),
      start=dy, end=dy))
    track(tracking, "SB.est", ay) <- unitSums(window(ssb(stk0),
      start=dy, end=dy))
    track(tracking, "C.est", ay) <- unitSums(window(catch(stk0),
      start=dy, end=dy))

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
      track(tracking, "phcr", ay) <- c(hcrpars[1,])
    }

    # --- hcr: Harvest Control Rule

    if (!is.null(ctrl0$hcr)){

      ctrl.hcr <- args(ctrl0$hcr)
      ctrl.hcr$method <- method(ctrl0$hcr)
      ctrl.hcr$stk <- stk0
      ctrl.hcr$args <- args #ay <- ay
      ctrl.hcr$tracking <- tracking
      ctrl.hcr$ind <- ind

      # ADD hcrpars to ctrl.hcr
      if(exists("hcrpars")) {
        hcrplist <- as(hcrpars, 'list')
        ctrl.hcr[names(hcrplist)] <- hcrplist
      }

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
      stop("'control' must contain a 'hcr' mseCtrl element")
    }
    
    # tracking
    track(tracking, "hcr", ay) <- ctrl

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

      # TODO: DEAL with ctrl rows in tracking
      track(tracking, "isys", ay) <- ctrl
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
      ctrl.tm$step <- "tm"
      
      out <- do.call("mpDispatch", ctrl.tm)
      
      attr(ctrl, "snew") <- out$flq
      tracking <- out$tracking

      track(tracking, "tm", ay) <- ctrl
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
      
      track(tracking, "iem", ay) <- ctrl
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
      
      track(tracking, "fb", ay) <- ctrl
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
    track(tracking, "fwd", ay) <- ctrl
    
    # time (in minutes per iter)   
    track(tracking, "time", ay) <- as.numeric(difftime(Sys.time(), stim,
      units = "mins")) / args$it

    # CPU process   
    id <- Sys.getpid()
    track(tracking, "pid", ay) <- id

    # CLEAR memory
    #gc()
  }
  
  # RETURN
  list(om=om, tracking=tracking, oem=oem, args=args)
  } 
)
# }}}

# goFish(FLombf) {{{

setMethod("goFish", signature(om="FLombf"),
  function(om, fb, projection, oem, iem, tracking, ctrl, args,
    verbose, progress, .DEBUG) {

  # CONSTANT args
  it <- args$it     # number of iterations
  y0 <- args$y0     # initial data year
  fy <- args$fy     # final year
  iy <- args$iy     # initial year of projection (also intermediate)
  vy <- args$vy     # vector of years to be projected
  nsqy <- args$nsqy # years for status quo calculations
  dlag <- args$data_lag  # years between assessment and last data
  mlag <- args$management_lag # years between assessment and management
  frq <- args$frq   # frequency
  bns <- args$ns <- names(biols(om))
  fns <- args$fns <- names(fisheries(om))

  # CHECK oem mode (byfishery)
  byfishery <- isTRUE(args(oem)$byfishery)

  # GET stock if not set
  if(is.null(args$stock))
    args$stock <- seq(biols(om))

  # COPY ctrl
  ctrl0 <- ctrl

  # SET default oem deviances
  if(length(deviances(oem)) == 0)
    deviances(oem) <- rep(list(NULL), length(biols(om)))

  # SET progressor
  if(progress)
    p <- progressor(steps=length(vy) + 1)

  # Go FISH!
  for(i in vy) {

    if(.DEBUG)
      browser()

    if(verbose) {
      cat(i, " > ")
    }

    # REPORT progress
    if(progress)
      p(message = sprintf("year: %s", i))
   
    # time (start)
    stim <- Sys.time()

    # ANNUAL args
    ay <- args$ay <- an(i)
    dy <- args$dy <- ay - dlag
    dys <- args$dys <- seq(ay - dlag - frq + 1, ay - dlag)
    dy0 <- args$dy0 <- dys[1]
    mys <- args$mys <- seq(ay + mlag, ay + mlag + frq - 1)
    
    # years for status quo computations 
    sqy <- args$sqy <- ac(seq(ay - nsqy - dlag + 1, dy))

    # TRACK om
    track(tracking, "F.om", ay) <- window(lapply(fbar(om),
      function(x) seasonMeans(unitMeans(x))), start=ay, end=ay)
    track(tracking, "B.om", ay) <- window(lapply(tsb(om),
      function(x) unitSums(x)[,,,1]), start=ay, end=ay)
    track(tracking, "SB.om", ay) <- window(lapply(ssb(om),
      function(x) unitSums(x)[,,,1]), start=ay, end=ay)
    track(tracking, "C.om", ay) <- window(lapply(catch(om),
      function(x) seasonSums(unitSums(x))), start=ay, end=ay)
    
    # --- OEM: Observation Error Model

    # COMMON elements
    ctrl.oem <- args(oem)
    ctrl.oem$method <- method(oem)
    ctrl.oem$args <- args
    ctrl.oem$ioval <- list(iv=list(t1=flsval), ov=list(t1=flsval, t2=flival))
    ctrl.oem$step <- "oem"

    # GET OM observation
    stk <- window(stock(om, full=TRUE, byfishery=byfishery), end=dy)

    # DROP units (sex, birth cohorts)
    stk <- lapply(stk, nounit)

    # APPLY oem across stocks
    o.out <- Map(function(stk, dev, obs) {

      obs.oem <- do.call("mpDispatch", c(ctrl.oem, list(stk=stk, deviances=dev,
        observations=obs, tracking=tracking)))

      # TODO: PICK UP fbar range from observations
      # range(obs.oem$stk, c("minfbar", "maxfbar")) <- 
      #  range(obs$stk, c("minfbar", "maxfbar")) 

      return(obs.oem)

    }, stk=stk, obs=observations(oem)[names(stk)],
      dev=deviances(oem)[names(stk)])

    # EXTRACT oem observations
    stk0 <- FLStocks(lapply(o.out, "[[", "stk"))
    idx0 <- lapply(o.out, "[[", "idx")

    # EXTRACT tracking
    tracking <- o.out[[length(o.out)]]$tracking
    
    # GET observations
    observations(oem) <- lapply(o.out, "[[", "observations")
    
    # TRACK oem observations
    track(tracking, "B.obs", ay) <- lapply(window(stk0, start=dy, end=dy),
      function(x) areaSums(unitSums(stock(x)))[,,,1])
    track(tracking, "SB.obs", ay) <- lapply(window(stk0, start=dy, end=dy),
      function(x) areaSums(unitSums(stock(x)))[,,,1])
    track(tracking, "C.obs", ay) <- lapply(window(stk0, start=dy, end=dy),
      function(x) seasonSums(areaSums(unitSums(catch(x)))))

    # --- est: Estimator of stock statistics

    if (!is.null(ctrl0$est)) {

      # - BY stock

      ctrl.est <- list(method=ctrl0$est@method, step="est",
        ioval = list(iv=list(t1=flsval, t2=flival), ov=list(t1=flsval)))

      # REPLICATE module args if needed
      if(!identical(names(args(ctrl0$est)), bns[args$stock]))
        args.est <- lapply(setNames(nm=bns[args$stock]), function(x) args(ctrl0$est))

      # SET empty, to be replaced
      ind <- lapply(setNames(nm=bns[args$stock]), function(x) FLQuants())

      # DISPATCH over stocks, alter args$stock, subset indices
      out.assess <- lapply(setNames(args$stock, nm=bns[args$stock]),
        function(x) {
          do.call("mpDispatch", c(ctrl.est, args.est[[bns[x]]], 
            list(args=c(args[-match("stock", names(args))], stock=x),
            stk=stk0[[bns[x]]], idx=idx0[[bns[x]]], tracking=tracking)))
      })

      # EXTRACT FLStocks
      stk0 <- FLStocks(lapply(out.assess, '[[', 'stk'))

      # EXTRACT indicators
      ind <- lapply(out.assess, '[[', 'ind')

      # EXTRACT tracking, already merged
      tracking <- out.assess[[1]][['tracking']]

      # PASS args generated at est to ctrl
      if (!is.null(out.assess$args)) {
        args(ctrl0$est)[names(out.assess$args)] <- out.assess$args
      }
    } else {
      stop("'control' must contain an 'est' mseCtrl element.")
    }

    # TRACK est
    track(tracking, "F.est", ay) <- lapply(stk0, function(x)
        unitMeans(seasonMeans(window(fbar(x), start=dy, end=dy))))
    track(tracking, "B.est", ay) <- lapply(stk0, function(x)
        unitSums(window(stock(x)[,,,1], start=dy, end=dy)))
    track(tracking, "SB.est", ay) <- lapply(stk0, function(x)
        unitSums(window(ssb(x)[,,,1], start=dy, end=dy)))
    track(tracking, "C.est", ay) <- lapply(stk0, function(x)
        unitSums(areaSums(seasonSums(window(catch(x), start=dy, end=dy)))))

    # --- phcr: HCR parameterization
   
    if (!is.null(ctrl0$phcr)) {

      ctrl.phcr <- args(ctrl0$phcr)
      ctrl.phcr$method <- method(ctrl0$phcr) 
      ctrl.phcr$stk <- stk0
      ctrl.phcr$args <- args
      ctrl.phcr$ind <- ind
      ctrl.phcr$tracking <- tracking
      if(exists("hcrpars")) ctrl.phcr$hcrpars <- hcrpars
      ctrl.phcr$ioval <- list(iv=list(t1=flssval), ov=list(t1=flpval))
      ctrl.phcr$step <- "phcr"
      
      out <- do.call("mpDispatch", ctrl.phcr)

      hcrpars <- out$hcrpars
      tracking <- out$tracking
    }

    # TODO: ADD tracking

    # --- hcr: Harvest Control Rule
    
    if (!is.null(ctrl0$hcr)) {

      # - BY stock

      ctrl.hcr <- list(method=ctrl0$hcr@method, step="hcr",
        ioval = list(iv=list(t1=flsval), ov=list(t1=flfval)))

      # REPLICATE module args if needed
      if(!identical(names(args(ctrl0$hcr)), bns[args$stock]))
        args.hcr <- lapply(setNames(nm=bns[args$stock]), function(x) args(ctrl0$hcr))

      # TODO: SEPARATE args

      # DISPATCH over stocks, alter args$stock, subset indices
      out <- lapply(setNames(args$stock, nm=bns[args$stock]),
        function(x) {
          do.call("mpDispatch", c(ctrl.hcr, args.hcr[[bns[x]]],
            list(args=c(args[-match("stock", names(args))], stock=x),
            stk=stk0[[bns[x]]], ind=ind[[bns[x]]], tracking=tracking)))
      })

      # - TODO: COMBINED stocks
      # CALL single hcr with FLStocks and indicators as inputs

      # EXTRACT fwdControls
      ctrl <- lapply(out, '[[', 'ctrl')

      # SUBSET if only one
      if(length(ctrl) == 1)
        ctrl <- ctrl[[1]]

      # EXTRACT tracking, already merged
      tracking <- out[[1]][['tracking']]

      # PASS args generated at est to ctrl
      if (!is.null(out.assess$args)) {
        args(ctrl0$est)[names(out.assess$args)] <- out.assess$args
      }
    } else {
      stop("'control' must contain an 'est' mseCtrl element.")
    }

    # TODO: ADAPT to ctrl list by stock
    track(tracking, "hcr", ay, biol=args$stock) <- ctrl
    
    #----------------------------------------------------------
    # Implementation system
    #----------------------------------------------------------
    if (!is.null(ctrl0$isys)) {

      ctrl.is <- args(ctrl0$isys)
      ctrl.is$method <- method(ctrl0$isys)
      ctrl.is$ctrl <- ctrl

      # SELECT stock for hcr
      if(args$stock == 'all')
        ctrl.is$stk <- stk0
      else if(length(args$stock) == 1)
        ctrl.is$stk <- stk0[[bns[args$stock]]]
      else
        ctrl.is$stk <- stk0

      ctrl.is$args <- args #ay <- ay
      ctrl.is$tracking <- tracking
      
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

      track(tracking, "isys", ay, biol=args$stock) <- ctrl
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

      track(tracking, "tm", ay, biol=args$stock) <- ctrl
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

      track(tracking, "iem", ay, biol=args$stock) <- ctrl
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
      
      track(tracking, "fb", ay, biol=args$stock) <- ctrl
    }

    # TODO: SET correct biol from FCB if needed
    # ctrl$biol <- FCB(om)[, 'B']

    #----------------------------------------------------------
    # stock dynamics and OM projections
    #----------------------------------------------------------
    ctrl.om <- args(projection)
    ctrl.om$ctrl <- ctrl
    ctrl.om$om <- om
    ctrl.om$method <- method(projection)
    # DEBUG
    ctrl.om$ioval <- list(iv=list(t1=floval), ov=list(t1=floval))
    ctrl.om$step <- "om"

    om <- do.call("mpDispatch", ctrl.om)$om

    track(tracking, "fwd", ay, biol=args$stock) <- ctrl

    # time (end)   
    track(tracking, "time", ay) <- as.numeric(Sys.time() - stim)
    track(tracking, "pid", ay) <- Sys.getpid()

    # invisible(gc())
  }

  # RETURN
  return(list(om=om, tracking=tracking, oem=oem, args=args))
  }
) # }}}

# mps {{{

# TODO: mps(FLmse, oem=oem(), ctrl=control(), args=args(), ...)

mps <- function(om, oem=NULL, iem=NULL, control=ctrl, ctrl=control, args,
  statistics=mse::statistics, metrics=NULL, type=character(1), names=NULL, 
  perf=FALSE, ...) {

  # GET ... arguments
  opts <- list(...)

  # SET seed
  if (!is.null(args$seed)) {
    seed <- args$seed
  } else {
    seed <- TRUE
  }

  # SET statistics to default if TRUE
  if(isTRUE(statistics))
    statistics <- mse::statistics
  
  # ARE opts being given?
  if(length(opts) == 0) {
    return(FLmses(RUN=mp(om=om, oem=oem, iem=iem, control=control, args=args,
      parallel=parallel)))
  }

  # PARSING a single module
  if(length(opts) > 1)
    stop("mps() can only alter a single module, called for: ", names(opts))
  
  # DO options refer to control elements?
  if(!names(opts) %in% names(control))
    stop("options refer to modules not present in control")

  # PARSE options on first element (module)
  module <- names(opts)[[1]]

  # MAX number of values
  largs <- max(unlist(lapply(opts[[module]], length)))
 
  # RECYCLE if shorter
  mopts <- lapply(opts[[module]], function(i) {
    rep(i, length=largs)
  })

  # CREATE standard names for single argument
  if(length(mopts) == 1)
    onms <- paste(module, names(mopts)[1], round(mopts[[1]], 2), sep='_')
  else
    onms <- paste(paste(names(mopts), collapse='-'), seq(largs), sep='_')
  
  # RENAME list elements
  if(is.null(names)) {
      names <- onms
  # IF names given
  } else {
    # PASTE to standard if one
    if(length(names) == 1)
      names <- paste(names, onms, sep="-")
  }

  message("Running mps() over module '", module, "' with ", largs,
    " argument sets.")

  message("Running on ", nbrOfWorkers(), " nodes.")

  # SET parallelization
  if(nbrOfWorkers() >= largs) {
    parallel <- FALSE
    progress <- TRUE
  } else {
    p <- progressor(steps=largs)
    parallel <- TRUE
    progress <- FALSE
  }

  res <- foreach(i = seq(largs), .errorhandling="pass", .inorder=TRUE,
      .options.future=list(globals=structure(TRUE, add=c("control", "module",
      "mopts", "om", "oem", "iem", "args"), seed=seed))) %dofuture% {

    # MODIFY module args
    args(control[[module]])[names(mopts)] <- lapply(mopts, "[", i)

    # PROGRESS by mp
    if(!progress)
      p(message = sprintf("MP: %i / %i", i, largs))

    # CALL mp, parallel left to work along MPs
    run <- mp(om, oem=oem, iem=iem, control=control, args=args, parallel=parallel,
       progress=progress, verbose=FALSE)

    # COMPUTE performance
    perftab <- performance(run, statistics=statistics, metrics=metrics, type=type,
      run=names[i])

    # CHOOSE return
    if(perf)
      run <- perftab
    else
      performance(run) <- perftab

    return(run)
  }
 
  names(res) <- names

  # STOP or WARN if missing runs
  done <- unlist(lapply(res, function(x) any(c("FLmse", "data.table") %in% is(x))))

  if(sum(done) == 0)
    stop("None of the calls to mp() returned results, check inputs")

  if(sum(done) < largs)
    warning(paste("Some calls to mp() did not run:"), seq(largs)[!done])

  # ASSEMBLE return object
  if(perf) {

    perftab <- rbindlist(res[done])
    
    perftab[, mp := paste(om, type, run, sep="_")]
    
    return(perftab[])

  } else {

    perftab <- rbindlist(lapply(res[done], performance))
    
    # ADD mp
    perftab[, mp := paste(om, type, run, sep="_")]
    
    return(FLmses(res[done], performance=perftab))
  }
}
# }}}
