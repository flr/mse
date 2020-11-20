# FLomBF-class.R - DESC
# /FLomBF-class.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# CLASS FLomBF {{{

FLomBF <- setClass("FLomBF", 
	representation("FLo",
    biols="FLBiols",
		refpts="FLPars",
    fisheries="FLFisheries")
) 

#' @rdname FLomBF-class
#' @template bothargs
#' @aliases FLomBF FLomBF-methods
setGeneric("FLomBF")

setMethod("initialize", "FLomBF",
    function(.Object,
             ...,
             biols, fisheries, refpts, fleetBehaviour, projection) {
      if (!missing(biols)) .Object@biols <- biols
      if (!missing(fisheries)) .Object@fisheries <- fisheries
      if (!missing(refpts)) .Object@refpts <- refpts
      if (!missing(fleetBehaviour)) .Object@fleetBehaviour <- fleetBehaviour
      if (!missing(projection)) .Object@projection <- projection
      .Object <- callNextMethod(.Object, ...)
      .Object
})

setValidity("FLomBF",
  function(object) {
    TRUE
})


# }}}

# accessors: biols, fisheries, refpts, sr {{{

setGeneric("biol", function(object, ...) standardGeneric("biol"))

setMethod("biol", signature(object="FLomBF"),
  function(object, stock="missing") {

    if (!missing(stock))
        return(object@biols[[stock]])
    else if(length(object@biols) == 1)
        return(object@biols[[1]])
      else
        stop("FLomBF object contains two biols, but none was selected.")
  })

setGeneric("biol<-", function(object, ..., value) standardGeneric("biol<-"))

setReplaceMethod("biol", signature(object="FLomBF", value="FLBiol"),
  function(object, value, stock="missing") {

    if (!missing(stock)) {
      object@biols[[stock]] <- value
      return(object)
    } else if(length(object@biols) == 1) {
      object@biols[[1]] <- value
      return(object)
    } else
      stop("FLomBF object contains two biols, but none was selected.")
  })

setGeneric("biols", function(object, ...) standardGeneric("biols"))

setMethod("biols", signature(object="FLomBF"),
  function(object, stock="missing") {
    return(object@biols)
  })

setGeneric("biols<-", function(object, value) standardGeneric("biols<-"))

setReplaceMethod("biols", signature(object="FLomBF", value="list"),
  function(object, value) {
    object@biols <- FLBiols(value)
    return(object)
  })

setGeneric("fisheries", function(object, ...) standardGeneric("fisheries"))

setMethod("fisheries", signature(object="FLomBF"),
  function(object) {
    return(object@fisheries)
  })

setGeneric("fisheries<-", function(object, value) standardGeneric("fisheries<-"))

setReplaceMethod("fisheries", signature(object="FLomBF", value="list"),
  function(object, value) {
    object@fisheries <- FLFisheries(value)
    return(object)
  })

setMethod("refpts", signature(object="FLomBF"),
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

setReplaceMethod("refpts", signature(object="FLomBF", value="FLPars"),
  function(object, value) {
    object@refpts <- value
    return(object)
  })

setReplaceMethod("refpts", signature(object="FLomBF", value="FLPar"),
  function(object, biol="missing", value) {
    
    if(length(object@refpts) == 1)
      object@refpts[1] <- FLPars(value)
    else if(missing(biol))
      stop("Object contains more than one refpts FLPar, but no element selected")
    else
      object@refpts[[biol]] <- value
    return(object)
  }) 

setMethod("sr", signature(object="FLomBF"),
  function(object) {
    res <- lapply(biols(object), rec, FALSE)
    if(length(res) == 1)
      res <- res[[1]]
    return(res)
  })
# }}}

# summary {{{

setMethod("summary", signature(object="FLomBF"),
  function(object) {

    # biols
    bis <- biols(om)
    nmb <- names(bis)
    pad <- max(nchar(paste0(nmb, ":")))

    cat("-- biols\n")
    cat(strrep(" ", pad), paste("min\tmax\tfrom\tto\n"))
    for(i in names(bis)) {
      cat(i, strrep(" ", pad - nchar(i) - 1) ,": ", sep="")
		  cat(range(bis[[i]])[-3], "\n", sep="\t")
    }

    # fisheries
    fis <- fisheries(om)
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
  }
) # }}}

# window {{{
setMethod("window", signature(x="FLomBF"),
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

setMethod("fwdWindow", signature(x="FLomBF", y="missing"),
  function(x, end=dims(x)$maxyear, nsq=3) {

    biols(x) <- lapply(biols(x), fwdWindow, end=end, nsq=nsq)
    fisheries(x) <- lapply(fisheries(x), fwdWindow, end=end, nsq=nsq)

    return(x)

  }
) # }}}

# dims {{{
setMethod("dims", signature(obj="FLomBF"),
  function(obj) {
    dis <- lapply(biols(obj), dims)

    list(
      minyear=min(unlist(lapply(dis, "[[", "minyear"))),
      maxyear=max(unlist(lapply(dis, "[[", "maxyear"))),
      iter=max(unlist(lapply(dis, "[[", "iter")))
    )
  }
) # }}}

# metrics: catch, landings, discards, ssb, tsb, rec {{{

setMethod("catch", signature(object="FLomBF"),
  function(object) {
    return(catch(fisheries(object)))
  }
)

setMethod("landings", signature(object="FLomBF"),
  function(object) {
    return(landings(fisheries(object)))
  }
)

setMethod("discards", signature(object="FLomBF"),
  function(object) {
    return(discards(fisheries(object)))
  }
)

setMethod("ssb", signature(object="FLomBF"),
  function(object) {
    return(FLQuants(mapply(ssb, biols(object), catch.n=catch.n(fisheries(object)),
      SIMPLIFY=FALSE)))
  }
)

setMethod("tsb", signature(object="FLomBF"),
  function(object) {
    return(lapply(biols(object), tsb))
  }
)

setMethod("rec", signature(object="FLomBF"),
  function(object) {
    return(lapply(biols(object), rec))
  }
)
# }}}

# fwd (FLomBF) {{{

setMethod("fwd", signature(object="FLomBF", fishery="missing", control="fwdControl"), 
  function(object, control, ...) {

    # CALL fwd(FLBiols, FLFisheries)
    res <- fwd(object@biols, object@fisheries, control=control, ...)
    
    # EXTRACT results
    object@biols <- res$biols
    object@fisheries <- res$fisheries

    return(object)
  })
# }}}

# plot {{{
setMethod("plot", signature(x="FLomBF", y="missing"),
  function(x, metrics=list(SBMSY=ssb ~ SBmsy, FMSY=fbar ~ Fmsy)) {

    plot(ssb(x)) + ylim(c(0,NA))

    # TODO 1. SB/SBMSY + F/FMSY

    # 2. catch by fleet

  }) # }}}

# plot(FLomBF, fwdControl) {{{

setMethod("plot", signature(x="FLomBF", y="fwdControl"),
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

# harvest {{{
setMethod("harvest", signature(object="FLomBF", catch="missing"),
  function(object) {

    # EXTRACT n, m and catch
    ns <- lapply(biols(object), n)
    ms <- lapply(biols(object), m)
    can <- catch.n(fisheries(object))

    # SINGLE biol
      res <- mapply(function(x, y, z) harvest(x, catch=y, m=z), x=ns, y=can,
        z=ms, SIMPLIFY=FALSE)

    return(res)
  }
) # }}}

# fbar {{{

setMethod("fbar", signature(object="FLomBF"),
  function(object, fisheries, minfbar="missing", maxfbar="missing") {

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

    return(FLQuants(mapply(function(x,y ) quantMeans(x[ac(y[1], y[2]),]), har, range,
      SIMPLIFY=FALSE)))
  }
) # }}}

# project {{{
setGeneric("project", function(object, ...) standardGeneric("project"))

setMethod("project", signature(object="FLomBF"),
  function(object, control) {
    return(projection(object)@method(object, control))
  }
) # }}}
