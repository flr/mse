# FLom-class.R - DESC
# mse/R/FLom-class.R

# Copyright European Union, 2018
# Author: Ernesto JARDIM (MSC) <ernesto.jardim@msc.org>
#         Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# FLom {{{

#' A class for an operating model (OM)
#'
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#' Suspendisse eu sapien vitae arcu lobortis ultrices vitae ac velit. Curabitur id 
#' 
#' @name FLom
#' @rdname FLom-class
#' @docType class
#' @aliases FLom-class
#'
#' @slot stock The population and catch history, `FLStock`.
#' @slot sr The stock-recruitment relationship, `FLSR`.
#' @slot refpts The estimated reference points, `FLPar`.
#' @slot fleetBehaviour Dynamics of the fishing fleet to be used in projections, `mseCtrl`.
#'
#' @section Validity:
#'
#'   \describe{
#'     \item{stock and sr dimensions}{Dimensions 2:6 of the `stock` and `sr` slots must match.}
#'     \item{rec age}{Stock and stock recruitment residuals must use the recruitment age.}
#' }
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLom'))}
#'
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#'
#' @section Constructor:
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All unspecified slots are then created to match the
#' requirements of the class validity function.
#'
#' @section Methods:
#' Methods exist for various calculations based on values stored in the class:
#'
#' \describe{
#'     \item{METHOD}{Neque porro quisquam est qui dolorem ipsum.}
#' }
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#'

FLom <- setClass("FLom", 
	representation("FLo",
    stock="FLStock",
		sr="FLSR",
    refpts="FLPar")
)

#' @rdname FLom-class
#' @template bothargs
#' @aliases FLom FLom-methods
setGeneric("FLom")

setMethod("initialize", "FLom",
    function(.Object,
             ...,
             stock, sr, refpts, fleetBehaviour, projection) {
      if (!missing(stock)) .Object@stock <- stock 
      if (!missing(sr)) .Object@sr <- sr
      if (!missing(refpts)) .Object@refpts <- refpts
      if (!missing(fleetBehaviour)) .Object@fleetBehaviour <- fleetBehaviour
      if (!missing(projection)) .Object@projection <- projection
      .Object <- callNextMethod(.Object, ...)
      .Object
})

setValidity("FLom",
  function(object) {
    # stk and sr must be compatible
	sd <- dim(object@stock)
	rd <- dim(object@sr@residuals)
    if (!all.equal(sd[-1], rd[-1])) "Stock and stock recruitment residuals must have the same dimensions." else TRUE
	# recruitment must be the same age
	sd <- dimnames(object@stock@stock.n)$age[1]
    rd <- dimnames(object@sr@residuals)$age[1]
    if (!all.equal(sd, rd)) "Stock and stock recruitment residuals must use the recruitment age." else TRUE
})
# }}}

#  accessor methods {{{

#' @rdname FLom-class

setMethod("stock", "FLom", function(object) object@stock)

#' @rdname FLom-class

setReplaceMethod("stock", signature("FLom", "FLStock"), function(object, value){
	object@stock <- value
	object
})

#' @rdname FLom-class

setMethod("sr", "FLom", function(object) object@sr)

#' @rdname FLom-class
#' @param value Object to assign in slot

setReplaceMethod("sr", signature("FLom", "FLSR"), function(object, value){
	object@sr <- value
	object
})

# }}}

# accessors to stock slots {{{

setMethod("catch", signature(object="FLom"),
  function(object) {
    return(catch(stock(object)))
  }
)

setMethod("catch.n", signature(object="FLom"),
  function(object) {
    return(catch.n(stock(object)))
  }
)

setMethod("catch.wt", signature(object="FLom"),
  function(object) {
    return(catch.wt(stock(object)))
  }
)

setMethod("landings", signature(object="FLom"),
  function(object) {
    return(landings(stock(object)))
  }
)

setMethod("landings.n", signature(object="FLom"),
  function(object) {
    return(landings.n(stock(object)))
  }
)

setMethod("landings.wt", signature(object="FLom"),
  function(object) {
    return(landings.wt(stock(object)))
  }
)

setMethod("discards", signature(object="FLom"),
  function(object) {
    return(discards(stock(object)))
  }
)

setMethod("discards.n", signature(object="FLom"),
  function(object) {
    return(discards.n(stock(object)))
  }
)

setMethod("discards.wt", signature(object="FLom"),
  function(object) {
    return(discards.wt(stock(object)))
  }
)

setMethod("m", signature(object="FLom"),
  function(object) {
    return(m(stock(object)))
  }
)

setMethod("m.spwn", signature(object="FLom"),
  function(object) {
    return(m.spwn(stock(object)))
  }
)

setMethod("mat", signature(object="FLom"),
  function(object) {
    return(mat(stock(object)))
  }
)

setMethod("harvest", signature(object="FLom"),
  function(object) {
    return(harvest(stock(object)))
  }
)

setMethod("harvest.spwn", signature(object="FLom"),
  function(object) {
    return(harvest.spwn(stock(object)))
  }
)

setMethod("stock.n", signature(object="FLom"),
  function(object) {
    return(stock.n(stock(object)))
  }
)

setMethod("stock.wt", signature(object="FLom"),
  function(object) {
    return(mat(stock.wt(object)))
  }
)
# }}}

# ssb, tsb {{{

setMethod("ssb", signature(object="FLom"),
  function(object) {

    return(ssb(stock(object)))
  }
)

setMethod("tsb", signature(object="FLom"),
  function(object) {

    return(tsb(stock(object)))
  }
)
# }}}

# fbar {{{

setMethod("fbar", signature(object="FLom"),
  function(object) {
    return(fbar(stock(object)))
  }
) 

# }}}

# rec {{{

setMethod("rec", signature(object="FLom"),
  function(object) {
    return(rec(stock(object)))
  }
) 

# }}}

# summary {{{

setMethod("summary", signature(object="FLom"),
  function(object) {

		cat("An object of class \"", class(object), "\"\n\n", sep="")
    
    # name
		cat("name:", object@name, "\n")
    
    # stock: dims, ages, years ...
    dms <- dims(stock(object))
    dm <- dim(stock(object))
		cat("stock:\n")
		cat("  dims: ", dms$quant, "\tyear\tunit\tseason\tarea\titer\n", sep="")
    cat("  ", dm, "\n", sep="\t")
    cat("  ages: ", dms$min, " - ", dms$max,
      ifelse(is.na(dms$plusgroup), "", "+"), "\n", sep="")
    cat("  years: ", dms$minyear, " - ", dms$maxyear, "\n", sep="")

    # ... & metrics
    metrics <- c("rec", "ssb", "catch", "fbar")

    for(i in metrics) {
      met <- try(iterMedians(do.call(i, list(object))), silent=TRUE)
      if(is(met, "FLQuant"))
        cat(" ", paste0(i, ":"),
          paste(format(range(met), trim=TRUE, digits=2), collapse=' - '),
          paste0(" (", units(met), ")"),
          "\n")
      else
        cat(" ", paste0(i, ": NA - NA (NA)\n"))
    }

    # sr
    cat("sr: \n")
    
    cat("  model:  ")
    nm <- SRModelName(model(sr(object)))
    if(!is.null(nm)) {
      cat(nm, "\n")
    } else {
      print(model(sr(object)), showEnv=FALSE)
    }
    
    cat("  params: \n")
    cat("  ", dimnames(params(sr(object)))$param, "\n", sep="\t")
    cat("  ", apply(params(sr(object)), 1, median), "\n", sep="\t")

    # refpts
    cat("  refpts: \n")
    cat("  ", dimnames(refpts(object))$param, "\n", sep="\t")
    cat("  ", apply(refpts(object), 1, median), "\n", sep="\t")

    # projection
    cat("projection: \n")
    cat("  method: ")
    cat(find.original.name(method(projection(object))), "\n")

    # fleetBehaviour

  }
)
# }}}

# plot {{{

setMethod("plot", signature(x="FLom", y="missing"),
  function(x, window=TRUE, ...) {

    # PARSE args for FLmse objects
    args <- list(...)
    cls <- unlist(lapply(args, is, "FLmse"))

    if(any(cls)) {
      stocks <- lapply(c(list(x), args[cls]), stock)

      # SORT OUT names
      if(is.null(names(stocks)))
        names(stocks) <- rep(character(1), length(stocks))
      idx <- names(stocks) == character(1)
      names(stocks)[idx] <- unlist(lapply(stocks, name))[idx]
      idx <- names(stocks) == character(1)
      names(stocks)[idx] <- 
        c("OM", paste0("MP", seq(length(cls))))[idx]

    # WINDOW om
    if(window)
      maxyear <- min(unlist(lapply(stocks[-1], function(x) dims(x)$minyear)))
    else
      maxyear <- min(unlist(lapply(stocks[-1], function(x) dims(x)$maxyear)))
    
    stocks[[1]] <- window(stocks[[1]], end=maxyear)
    
    do.call("plot", c(list(x=FLStocks(stocks)), args[!cls]))
    
    } else {
    
      plot(stock(x), ...)
    
    }
  }
) # }}}

# dims {{{
setMethod("dims", signature(obj="FLom"),
  function(obj) {
    dims(stock(obj))
  }
) # }}}

# dimnames {{{
setMethod("dimnames", signature(x="FLom"),
  function(x) {
    dimnames(stock(x))
  }
) # }}}

# window {{{
setMethod("window", signature(x="FLom"),
  function(x, ...) {

    x@stock <- window(x@stock, ...)

    return(x)
  })
# }}}

# fwdWindow {{{

setMethod("fwdWindow", signature(x="FLom", y="missing"),
  function(x, end=dims(x)$maxyear, nsq=3, ...) {

    stock(x) <- fwdWindow(stock(x), end=end, nsq=nsq, ...)

    sr(x) <- window(sr(x), end=end, ...)

    return(x)

  }
) # }}}

# fwd {{{

#' @examples
#' data(p4om)
#' res <- fwd(om, control=fwdControl(year=2018:2030, quant="f",
#'   value=rep(c(refpts(om)$FMSY), 13)))

setMethod("fwd", signature(object="FLom", fishery="missing", control="fwdControl"),
  function(object, control, maxF=4, deviances=residuals(sr(object)), ...) {
    
    stock(object) <- fwd(stock(object), sr=sr(object), control=control,
      maxF=maxF, deviances=deviances, ...)

    return(object)
  })

# }}}

# iter {{{

setMethod("iter", signature(obj="FLom"),
  function(obj, iter) {

    # stock
    stock(obj) <- iter(stock(obj), iter)
    
    # refpts
    refpts(obj) <- iter(refpts(obj), iter)

    # sr
    sr(obj) <- iter(sr(obj), iter)

    # fleeBehaviour

    # projection

    return(obj)
  }
) # }}}

# combine {{{

#' @rdname FLom-class
#' @examples
#' data(sol274)
#' comb <- combine(iter(om, 1:50), iter(om, 51:100))
#' all.equal(om, comb)

setMethod("combine", signature(x = "FLom", y = "FLom"), function(x, y, ...){
	
  args <- c(list(x, y), list(...))

	if(length(args) > 2) {

		return(combine(combine(x, y), ...))
	
  } else {

    stock(x) <- combine(stock(x), stock(y))
    sr(x) <- combine(sr(x), sr(y))
    refpts(x) <- cbind(refpts(x), refpts(y))

    return(x)
	}
})
# }}}

# metrics {{{
setMethod("metrics", signature(object="FLom", metrics="missing"),
  function(object) {
    FLQuants(metrics(object, list(SB=ssb, C=catch, F=fbar)))
}) # }}}

# propagate {{{
setMethod("propagate", signature(object="FLom"),
	function(object, iter, fill.iter=TRUE) {

    stock(object) <- propagate(stock(object), iter=iter, fill.iter=fill.iter)
    sr(object) <- propagate(sr(object), iter=iter, fill.iter=fill.iter)

    return(object)
  }
) # }}}

# dim {{{
setMethod("dim", signature(x="FLom"),
  function(x) {
    return(dim(stock(x)))
  }
) # }}}
