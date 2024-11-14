# FLombf-class.R - DESC
# mse/R/FLombf-class.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# CLASS FLombf {{{

FLombf <- setClass("FLombf", 
	representation("FLo",
    biols="FLBiols",
    fisheries="FLFisheries",
		refpts="FLPars",
    FCB="array")
) 

#' @rdname FLombf-class
#' @template bothargs
#' @aliases FLombf FLombf-methods
setGeneric("FLombf")

setMethod("initialize", "FLombf",
    function(.Object, ..., biols, fisheries, name, refpts, fleetBehaviour,
      projection, FCB) {
      if (!missing(name)) .Object@name <- name
      if (!missing(biols)) .Object@biols <- biols
      if (!missing(fisheries)) .Object@fisheries <- fisheries
      if (!missing(refpts)) .Object@refpts <- refpts
      if (!missing(fleetBehaviour)) .Object@fleetBehaviour <- fleetBehaviour
      if (!missing(projection)) .Object@projection <- projection
      if(missing(FCB))
        .Object@FCB <- FLFishery::FCB(fcb2int(guessfcb(biols, fisheries),
          biols, fisheries))
      else
        .Object@FCB <- FCB
      .Object <- callNextMethod(.Object, ...)
      .Object
})
  
setValidity("FLombf",
  function(object) {

    # CHECK FCB
    # unique(F) = length(fisheries)
    # C matches Cs per F
    # unique(B) = length(biols)

  # EXTRACT dims
  dis <- c("min", "max", "minyear", "maxyear", "unit", "season",
    "area", "iter")

  bdims <- rbindlist(lapply(biols(object), function(x)
    as.data.table((dims(x)[dis]))
  ), idcol="biol")

  fdims <- rbindlist(lapply(fisheries(object), function(x)
    as.data.table((dims(x)[dis]))
  ), idcol="fishery")

  cdims <- rbindlist(lapply(fisheries(object), function(f)
    rbindlist(lapply(f, function(c) as.data.table(dims(c)[dis])),
      idcol="catch")), idcol="fishery")

  odims <- rbindlist(list(bdims, fdims, cdims), fill=TRUE)

  # CHECK all years match
  if(max(c(length(unique(odims$minyear)), length(unique(odims$maxyear)))) > 1)
    return("'year' dimension does not match on different slots")

  # CHECK iters match
  if(length(unique(odims$iter)) > 1)
    return("'iter' dimension does not match on different slots")

  for (i in unique(odims$biol)[!is.na(unique(odims$biol))])
    if(any(unlist(odims[catch == i, .(max)]) > c(odims[biol == i, .(max)])))
      return("'age' dimensions in biol ", i, " and its catches do not match")
    if(any(unlist(odims[catch == i, .(min)]) < c(odims[biol == i, .(min)])))
      return("'age' dimensions in biol ", i, " and its catches do not match")

  return(TRUE)
})

# }}}

# accessors: biols, fisheries, refpts, sr, FCB {{{

setGeneric("biol", function(object, ...) standardGeneric("biol"))

setMethod("biol", signature(object="FLombf"),
  function(object, stock="missing") {

    if (!missing(stock))
      return(object@biols[[stock]])
    else if(length(object@biols) == 1)
      return(object@biols[[1]])
    else
      stop("FLombf object contains more than one biol, but none was selected.")
  })

setGeneric("biol<-", function(object, ..., value) standardGeneric("biol<-"))

setReplaceMethod("biol", signature(object="FLombf", value="FLBiol"),
  function(object, stock="missing", value) {

    if (!missing(stock)) {
      object@biols[[stock]] <- value
      return(object)
    } else if(length(object@biols) == 1) {
      object@biols[[1]] <- value
      return(object)
    } else
      stop("FLombf object contains more than one biol, but none was selected.")
  })

setGeneric("biols", function(object, ...) standardGeneric("biols"))

setMethod("biols", signature(object="FLombf"),
  function(object, stock="missing") {
    return(object@biols)
  })

setGeneric("biols<-", function(object, value) standardGeneric("biols<-"))

setReplaceMethod("biols", signature(object="FLombf", value="list"),
  function(object, value) {
    object@biols <- FLBiols(value)
    return(object)
  })

setGeneric("fisheries", function(object, ...) standardGeneric("fisheries"))

setMethod("fisheries", signature(object="FLombf"),
  function(object) {
    return(object@fisheries)
  })

setGeneric("fisheries<-", function(object, value) standardGeneric("fisheries<-"))

setReplaceMethod("fisheries", signature(object="FLombf", value="list"),
  function(object, value) {
    object@fisheries <- FLFisheries(value)
    return(object)
  })

setMethod("refpts", signature(object="FLombf"),
  function(object, biol="missing") {
    if(missing(biol))
      if(length(object@refpts) == 1)
        return(object@refpts[[1]])
      else
        return(object@refpts)
    else
      return(object@refpts[[biol]])
  })

setGeneric("refpts<-", function(object, ..., value) standardGeneric("refpts<-"))

setReplaceMethod("refpts", signature(object="FLombf", value="FLPars"),
  function(object, value) {
    object@refpts <- value
    return(object)
  })

setReplaceMethod("refpts", signature(object="FLombf", value="FLPar"),
  function(object, biol="missing", value) {
    
    if(length(object@refpts) == 1)
      object@refpts[1] <- FLPars(value)
    else if(missing(biol))
      stop("Object contains more than one refpts FLPar, but no element selected")
    else
      object@refpts[[biol]] <- value
    return(object)
  }) 

setMethod("FCB", signature(object="FLombf"),
  function(object) {
    return(object@FCB)
})

setReplaceMethod("FCB", signature(object="FLombf"),
  function(object, value) {
    object@FCB <- value
    return(object)
})
# }}}

# accessors: biols slots {{{

.get_biolslotFLombf <- function(object, slot, biol) {
  
  # IF no biol, return FLQuants
  if(is.null(biol))
    return(lapply(biols(object), slot))

  # IF length(biol) == 1
  if(length(biol) == 1)
    return(do.call(slot, list(biols(object)[[biol]])))

  # IF length(biol) > 1, return FLQuants
  return(lapply(biols(object)[biol], slot))
}

# n
setMethod("n", signature(object="FLombf"),
  function(object, biol=NULL) {
    .get_biolslotFLombf(object, "n", biol=biol)
  }
)

# m
setMethod("m", signature(object="FLombf"),
  function(object, biol="missing") {
    .get_biolslotFLombf(object, "m", biol)
  }
)

# wt
setMethod("wt", signature(object="FLombf"),
  function(object, biol="missing") {
    .get_biolslotFLombf(object, "wt", biol)
  }
)

# mat
setMethod("mat", signature(object="FLombf"),
  function(object, biol="missing") {
    .get_biolslotFLombf(object, "mat", biol)
  }
)

# fec
setMethod("fec", signature(object="FLombf"),
  function(object, biol="missing") {
    .get_biolslotFLombf(object, "fec", biol)
  }
)

# rec
setMethod("rec", signature(object="FLombf"),
  function(object, biol="missing") {
    .get_biolslotFLombf(object, "rec", biol)
  }
)

# rec.obs
setMethod("rec.obs", signature(object="FLombf"),
  function(object, biol="missing") {
    .get_biolslotFLombf(object, "rec.obs", biol)
  }
)

# sr
setMethod("sr", signature(object="FLombf"),
  function(object, biol="missing") {
    .get_biolslotFLombf(object, "sr", biol)
  }
)
# }}}

# -- fishery metrics

# catch, landings, discards {{{

setMethod("catch", signature(object="FLombf"),
  function(object, by="catch") {
    return(catch(fisheries(object), by=by))
  }
)

setMethod("landings", signature(object="FLombf"),
  function(object, by="catch") {
    return(landings(fisheries(object), by=by))
  }
)

setMethod("discards", signature(object="FLombf"),
  function(object, by="catch") {
    return(discards(fisheries(object), by=by))
  }
)
# }}}

# -- biol metrics

# ssb, tsb {{{

setMethod("ssb", signature(object="FLombf"),
  function(object, biol=NULL) {
    return(FLQuants(Map(ssb, object=biols(object),
      harvest=harvest(object))))

    if(length(biols(object)) == 1)
      return(ssb(biol(object), harvest=harvest(object)))
    else
      return(FLQuants(Map(ssb, object=biols(object),
        harvest=harvest(object))))
  }
)

setMethod("tsb", signature(object="FLombf"),
  function(object, biol=NULL) {

    res <- FLQuants(Map(tsb, biols(object),
      f=harvest(object)))
    
    return(res)
  }
)

setMethod("tb", signature(object="FLombf"),
  function(object, biol=NULL) {

    res <- FLQuants(Map(tb, biols(object),
      f=harvest(object)))
    
    return(res)
  }
)
# }}}

# rec, tsb {{{

setMethod("rec", signature(object="FLombf"),
  function(object, biol=NULL) {
    
    res <- FLQuants(lapply(biols(object), rec))

    return(res)
  }
)

setMethod("tsb", signature(object="FLombf"),
  function(object, biol=NULL) {

    res <- FLQuants(mapply(tsb, biols(object),
      f=harvest(object), SIMPLIFY=FALSE))
    
    return(res)
  }
)

setMethod("tb", signature(object="FLombf"),
  function(object, biol=NULL) {

    res <- FLQuants(mapply(tb, biols(object),
      f=harvest(object), SIMPLIFY=FALSE))
    
    return(res)
  }
)
# }}}

# harvest, partialF, computeHarvest {{{

setMethod("harvest", signature(object="FLombf", catch="missing"),
  function(object, biol=seq(biols(object))) {

    # GET partial Fs by biol - fishery
    res <- FLQuants(harvest(biols(object)[biol], fisheries(object)))
    return(res)
  }
)

setMethod("partialF", signature(object="FLombf", fisheries="missing"),
  function(object, biol=seq(biols(object))) {
    
    res <- partialF(biols(object), fisheries=fisheries(object),
      biol=biol, fcb=FCB(object))

      return(res)
  }
)

setMethod("computeHarvest", signature(object="FLombf", catch="missing"),
  function(object) {
    
    # FIND empty years
    sumna <- lapply(biols(object), function(x)
      c(apply(n(x), c(2), function(y) sum(is.na(y)))) == 0)

    # EXTRACT n, m and catch
    ns <- lapply(biols(object), n)
    ms <- lapply(biols(object), m)
    can <- catch.n(fisheries(object))

    fcb <- FCB(object)

    # APPLY over biols
    res <- lapply(setNames(seq(ns), nm=names(biols(object))), function(b) {
      
      # GET n and m
      nb <- n(biols(object)[[b]])[,sumna[[b]]]
      mb <- m(biols(object)[[b]])[,sumna[[b]]]

      # SUBSET fcb for biol
      idx <- fcb[fcb[, "B"] == b,, drop=FALSE]
      
      # GET FLCatches for biol
      can <- mapply(function(x, y) catch.n(x[[y]])[,sumna[[b]]],
        x=fisheries(object)[idx[, "F"]], y=idx[,"C"], SIMPLIFY=FALSE)

      window(harvest(nb, Reduce("+", can), mb), end=dims(ns[[1]])$maxyear)
      }
    )
    
   return(res)
  }
) 

# }}}

# fbar {{{

setMethod("fbar", signature(object="FLombf"),
  function(object, minfbar="missing", maxfbar="missing", biol=NULL) {

    har <- harvest(object)

    range <- lapply(biols(object), function(x) range(x)[c("min", "max")])

    if(!missing(minfbar)) {
      for(i in seq(range))
        range[[i]]["min"] <- rep(minfbar, i)[i]
    }
    
    if(!missing(maxfbar)) {
      for(i in seq(range))
        range[[i]]["max"] <- rep(maxfbar, i)[i]
    }

    res <- FLQuants(Map(function(x, y) quantMeans(x[ac(seq(y[1], y[2])),]),
      x=har, y=range))

    if(!is.null(biol))
      if(length(biol) == 1)
        return(res[[biol]])
      else
        return(res[biol])

    return(res)
  }
) # }}}

# summary {{{

setMethod("summary", signature(object="FLombf"),
  function(object) {

    cat("An object of class \"FLombf\"\n")
		cat("Name:", object@name, "\n")

    # biols
    bis <- biols(object)
    nmb <- names(bis)
    pad <- max(nchar(paste0(nmb, ":")))

    cat("-- biols\n")
    cat(strrep(" ", pad), paste("min\tmax\tfrom\tto\n"))
    for(i in names(bis)) {
      cat(i, strrep(" ", pad - nchar(i) - 1) ,": ", sep="")
		  cat(range(bis[[i]])[-3], "\n", sep="\t")
    }

    # fisheries
    fis <- fisheries(object)
    nmf <- names(fis)
    pad <- max(nchar(paste0(nmf, ":")))

    cat("\n-- fisheries\n")
    cat(strrep(" ", pad), paste("from\tto\tcatches\n"))
    for(i in names(fis)) {
      cat(i, strrep(" ", pad - nchar(i) - 1) ,": ", sep="")
      cat(unname(unlist(dims(fis[[i]])
        [c("minyear", "maxyear")])), sep="\t")
      cat("\t", paste0(names(fis[[i]]), collapse=", "), "\n", sep="")
    }
    cat("\n")
    
    # other slots
    callNextMethod()
    invisible()
  }
) # }}}

# window {{{
setMethod("window", signature(x="FLombf"),
  function(x, ...) {

    x@biols <- lapply(x@biols, window, ...)
    x@fisheries <- lapply(x@fisheries, window, ...)
    x@fisheries <- lapply(x@fisheries, function(y) {
      y@.Data <- lapply(y, window, ...)
      return(y)})

    return(x)
  })
# }}}

# fwdWindow {{{

setMethod("fwdWindow", signature(x="FLombf", y="missing"),
  function(x, end=dims(x)$maxyear + 10, nsq=3, ...) {

    # RETURN if no new years
    if(end == dims(x)$maxyear)
      return(x)
  
    # EXTEND biols
    biols(x) <- lapply(biols(x), fwdWindow, end=end, nsq=nsq, ...)

    # EXTEND fisheries
    fisheries(x) <- lapply(fisheries(x), fwdWindow, end=end, nsq=nsq)

    return(x)

  }
) # }}}

# dims {{{
setMethod("dims", signature(obj="FLombf"),
  function(obj) {
    dis <- lapply(biols(obj), dims)

    list(
      minyear=min(unlist(lapply(dis, "[[", "minyear"))),
      maxyear=max(unlist(lapply(dis, "[[", "maxyear"))),
      unit=max(unlist(lapply(dis, "[[", "unit"))),
      season=max(unlist(lapply(dis, "[[", "season"))),
      iter=max(unlist(lapply(dis, "[[", "iter")))
    )
  }
) # }}}

# dimnames {{{
setMethod("dimnames", signature(x="FLombf"),
  function(x) {
    dimnames(biols(x)[[1]])
  }
) # }}}

# fwd (FLombf) {{{

setMethod("fwd", signature(object="FLombf", fishery="missing", 
  control="fwdControl"), 
  function(object, control, deviances="missing", window=FALSE, ...) {
    
    # ADD object FCB if missing
    if(all(is.na(FCB(control))))
      FCB(control) <- FCB(object)

    # deviances
    if(missing(deviances)) {
      deviances <- lapply(biols(object), function(x) {
        if(!is.null(x@rec$deviances))
          x@rec$deviances
        else
          n(x)[1,] %=% 1
      })
    }

    # COERCE to FLQuants if given as FLQuant and a single stock
    if(is(deviances, "FLQuant") & length(biols(object)) == 1) {
      deviances <- FLQuants(deviances)
    }

    # CALL fwd(FLBiols, FLFisheries)
    res <- fwd(object@biols, object@fisheries, control=control,
      deviances=deviances, ...)
    
    # EXTRACT results
    object@biols <- res$biols
    object@fisheries <- res$fisheries

    if(window)
      object <- window(object, start=min(control$year) - 1)

    return(object)
  })
# }}}

# index.hat {{{

setGeneric('index.hat', function(object, stock, ...)
		standardGeneric('index.hat'))

#' Predicted index of abundance from abundance estimates
#'
#' @param object An FLIndexBiomass object.
#' @param stock An FLStock object with estimated abundances, *stock.n*.
#'
#' @return An FLQuant for the precited index of abundance in biomass.

setMethod("index.hat", signature(object="FLIndexBiomass", stock="FLStock"),
  function(object, stock) {
    
    # SELECT years by index
    yrs <- dimnames(object)$year
    stock <- stock[, yrs]

    quantSums(stock.n(stock) * catch.wt(object) * sel.pattern(object) *
    exp(-z(stock) * mean(range(object, c("startf", "endf"))))) * index.q(object)
}) # }}}

# project {{{
setGeneric("project", function(object, ...) standardGeneric("project"))

setMethod("project", signature(object="FLombf"),
  function(object, control) {
    return(projection(object)@method(object, control))
  }
) # }}}

# [ {{{
setMethod("[", signature(x="FLombf"),
    function(x, i, j, k, l, m, n, ..., drop=FALSE) {

      if(missing(j))
        stop("'[' for 'FLombf' currently only implemented for 'year' dimension.")

      # BIOLS
      biols(x) <- lapply(biols(x), function(x) x[,j])

      # FISHERIES
      fisheries(x) <- lapply(fisheries(x),
        function(x) {
          x <- x[,j]
          x@.Data <- lapply(x, function(y) y[,j])
          return(x)
        }
      )
      return(x)
    }
) # }}}

# metrics {{{
setMethod("metrics", signature(object="FLombf", metrics="missing"),
  function(object) {
    
    # CALL for metrics by biol
    mets <- lapply(metrics, do.call, list(object))

    # SET list with biols' metrics
    mets <- lapply(setNames(nm=names(biols(x))),
      function(i) FLQuants(lapply(X=mets, FUN="[[", i)))

    if(length(mets) == 1)
      return(mets[[1]])

    return(mets)
})
# }}}

# stock {{{

setMethod("stock", signature(object="FLombf"),
  function(object, full=TRUE, byfishery=FALSE) {

    bios <- names(biols(object))

    # Map fisheries and biols
    fbmap <- lapply(fisheries(object), names)

    # COERCE to FLStock(s)
    res <- FLStocks(
      # LAPPLY over biols
      lapply(setNames(nm=names(biols(object))), function(x) {
        # CHOOSE matching fisheries
        fisheries <- fisheries(object)[unlist(lapply(fbmap, function(i)
          x %in% i))]
        stk <- as.FLStock(biols(object)[[x]], fisheries=fisheries,
          catch=x, full=full)
        # ADD fisheries catch data as areas, if requested
        if(byfishery) {
          dat <- FLQuants(lapply(setNames(nm=c('landings.n', 'landings.wt',
            'discards.n', 'discards.wt')),
            function(s) abind(lapply(fisheries, s, x))))
          stk@landings.n <- dat$landings.n
          stk@landings.wt <- dat$landings.wt
          stk@discards.n <- dat$discards.n
          stk@discards.wt <- dat$discards.wt
          catch(stk) <- computeCatch(stk, 'all')
        }
      return(stk)
      }
    ))

    return(res)
  }
)

# }}}

# propagate {{{
setMethod("propagate", signature(object="FLombf"),
	function(object, iter, fill.iter=TRUE) {

    biols(object) <- lapply(biols(object), propagate, iter=iter,
      fill.iter=fill.iter)

    fisheries(object) <- lapply(fisheries(object), propagate, iter=iter,
      fill.iter=fill.iter)
    
    refpts(object) <- FLPars(lapply(object@refpts, propagate, iter=iter,
      fill.iter=fill.iter))
    
    return(object)
  }
) # }}}

# iter {{{
setMethod("iter", signature(obj="FLombf"),
  function(obj, iter) {

    # biols
    biols(obj) <- lapply(biols(obj), 'iter', iter)

    # fisheries
    fisheries(obj) <- lapply(fisheries(obj), 'iter', iter)

    # refpts
    obj@refpts <- FLPars(lapply(obj@refpts, 'iter', iter))

    return(obj)
  }
) 
# }}}

# combine {{{

setMethod("combine", signature(x = "FLombf", y = "FLombf"), function(x, y, ...){
	
  args <- c(list(x, y), list(...))

	if(length(args) > 2) {

		return(combine(combine(x, y), ...))
	
  } else {

    # biols
    biols(x) <- Map(combine, x=biols(x), y=biols(y))

    # fisheries
    fisheries(x) <- Map(function(i, j) {
      combine(i, j)
    }, i=fisheries(x), j=fisheries(y))

    # refpts
    slot(x, 'refpts') <- do.call(combine, lapply(args, slot, 'refpts'))

    return(x)
	}
})
# }}}

# deviances {{{

setMethod("deviances", signature(object="FLombf"),
  function(object, biol=names(biols(object))) {

    res <- lapply(biols(object), deviances)

    if(length(biol) == 1)
      return(res[[biol]])
    else
      return(res[biol])
  })

setReplaceMethod("deviances", signature(object="FLombf", value="FLQuant"),
  function(object, biol=names(biols(object)), value) {
    
    # HACK
    if(missing(value)) {
      value <- biol
      biol <- names(biols(object))
    }

    if(length(biol) > 1) {
      warning("A single FLQuant will be assigned to more than one biol.")
    }

    value <- FLQuants(setNames(rep(list(value), length(biol)), nm=biol))

    deviances(object, biol=biol) <- value

    return(object)
  })

setReplaceMethod("deviances", signature(object="FLombf", value="FLQuants"),
  function(object, biol=names(biols(object)), value) {

    # HACK
    if(missing(value)) {
      value <- biol
      biol <- names(biols(object))
    }

    biols(object) <- Map(function(x,y) {
      deviances(x) <- y
      return(x)}, x=biols(object), y=value)

    return(object)
  })
# }}}
