# FLombf-class.R - DESC
# /FLombf-class.R

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
        .Object@FCB <- FLasher:::FCB(FLasher:::fcb2int(FLasher:::guessfcb(biols,
          fisheries), biols, fisheries))
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

    # TODO
    TRUE
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
  function(object, value, stock="missing") {

    if (!missing(stock)) {
      object@biols[[stock]] <- value
      return(object)
    } else if(length(object@biols) == 1) {
      object@biols[[1]] <- value
      return(object)
    } else
      stop("FLombf object contains mora than one biol, but none was selected.")
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

# catch, landings, discards {{{

setMethod("catch", signature(object="FLombf"),
  function(object) {
    return(catch(fisheries(object)))
  }
)

setMethod("landings", signature(object="FLombf"),
  function(object) {
    return(landings(fisheries(object)))
  }
)

setMethod("discards", signature(object="FLombf"),
  function(object) {
    return(discards(fisheries(object)))
  }
)
# }}}

# ssb, tsb {{{

setMethod("ssb", signature(object="FLombf"),
  function(object, biol=NULL) {
    
    res <- FLQuants(mapply(ssb, biols(object),
      f=harvest(object), SIMPLIFY=FALSE))

    if(!is.null(biol))
      if(length(biol) == 1)
        return(res[[biol]])
      else
        return(res[biol])
    return(res)
  }
)

setMethod("tsb", signature(object="FLombf"),
  function(object, biol=NULL) {

    res <- FLQuants(mapply(tsb, biols(object),
      f=harvest(object), SIMPLIFY=FALSE))
    
    if(!is.null(biol))
      if(length(biol) == 1)
        return(res[[biol]])
      else
        return(res[biol])
    return(res)
  }
)


# }}}

# harvest {{{

setMethod("harvest", signature(object="FLombf", catch="missing"),
  function(object, biol=seq(biols(object))) {
    
    # GET partial Fs by biol - fishery
    pfs <- partialF(object, biol=biol)
    
    # ADD UP by biol
    res <- FLQuants(lapply(pfs, Reduce, f="+"))

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

setGeneric("computeHarvest", function(object, catch, ...)
		standardGeneric("computeHarvest"))

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

    res <- FLQuants(mapply(function(x, y) quantMeans(x[ac(seq(y[1], y[2])),]),
      x=har, y=range, SIMPLIFY=FALSE))
 
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
  function(x, end=dims(x)$maxyear, nsq=3) {

    biols(x) <- lapply(biols(x), fwdWindow, end=end, nsq=nsq)
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
      iter=max(unlist(lapply(dis, "[[", "iter")))
    )
  }
) # }}}

# fwd (FLombf) {{{

setMethod("fwd", signature(object="FLombf", fishery="missing", control="fwdControl"), 
  function(object, control, ...) {

    # ADD object FCB if missing
    if(all(is.na(FCB(control))))
      FCB(control) <- FCB(object)

    # CALL fwd(FLBiols, FLFisheries)
    res <- fwd(object@biols, object@fisheries, control=control, ...)
    
    # EXTRACT results
    object@biols <- res$biols
    object@fisheries <- res$fisheries

    return(object)
  })
# }}}

# plot {{{
setMethod("plot", signature(x="FLombf", y="missing"),
  function(x, metrics=list(SBMSY=ssb ~ SBmsy, FMSY=fbar ~ Fmsy)) {

    # 1. SB/SBMSY + F/FMSY
    ssbs <- ubind(ssb(x))
    p1 <- plot(ssbs) + ylim(c(0,NA)) +
      ylab(paste0("SSB (", units(ssbs) , ")"))

    # 2. catch by fleet
    cas <- ubind(catch(fisheries(x)))
    p2 <- plot(cas) + ylim(c(0, NA)) +
      ylab(paste0("Catch (", units(cas), ")")) +
      theme(legend.position="bottom")

    # TODO COMBINE p1 + p2
    return(p1)

 }) # }}}

# plot(FLombf, fwdControl) {{{

setMethod("plot", signature(x="FLombf", y="fwdControl"),
  function(x, y, fill="#E69F00", ...) {

    yrs <- range(y$year)

    # CREATE standard plot
    p <- plot(x, ...)

    # GET x variable
    if(rlang::as_name(p$mapping$x) == "date") {
      yrs <- range(p$data[with(p$data, year %in% yrs), "date"])
    }

    p + geom_vline(xintercept=yrs[1], alpha=0.4) +
      geom_vline(xintercept=yrs[2], alpha=0.2) +
      annotate("rect", xmin = yrs[1], xmax = yrs[2], ymin = -Inf, ymax = Inf,
        fill = fill, alpha=0.1)
  }
) # }}}

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
    metrics(object, list(SB=ssb, C=catch, F=fbar))
}) # }}}

# stock {{{
setMethod("stock", signature(object="FLombf"),
  function(object) {

    # DEBUG SLOW!
    return(window(suppressWarnings(as.FLStock(om@biols[[1]], om@fisheries,
    full=TRUE))))
  }
)
# }}}
