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

setGeneric("sr<-", function(object, value) standardGeneric("sr<-"))

#' @rdname FLom-class
#' @param value Object to assign in slot

setReplaceMethod("sr", signature("FLom", "FLSR"), function(object, value){
	object@sr <- value
	object
})

# }}}

# catch, landings, discards {{{

setMethod("catch", signature(object="FLom"),
  function(object) {
    return(catch(stock(object)))
  }
)

setMethod("landings", signature(object="FLom"),
  function(object) {
    return(landings(stock(object)))
  }
)

setMethod("discards", signature(object="FLom"),
  function(object) {
    return(discards(stock(object)))
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

# harvest {{{

setMethod("harvest", signature(object="FLom", catch="missing"),
  function(object) {
    return(harvest(stock(object)))
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

# show, summary {{{

#' @rdname FLom-class
setMethod("show", signature(object = "FLom"),
  function(object)
  {
  cat('An object of class "FLom"\n')

	cat("\n--- stock:\n")
  summary(object@stock)

	cat("\n--- sr:\n")
  summary(object@sr)

	cat("\n--- refpts:\n")
  show(object@refpts)

	cat("\n--- fleetBehaviour:\n")
  show(object@fleetBehaviour)

	cat("\n--- projection:\n")
  show(object@projection)

 })
 
setMethod("summary", signature(object="FLom"),
  function(object) {

    cat("An object of class \"", class(object), "\"\n\n", sep="")

    # stock
    cat("-- stock\n")
    stock <- stock(object)
		dms <- dims(stock)
    
		cat("Name:", name(stock), "\n")
    cat("Quant:", dms$quant, "\n")
		cat("Dims: ", dms$quant, "\tyear\tunit\tseason\tarea\titer\n")
		cat("", unname(unlist(dms[c(dms$quant, "year", "unit", "season",
    "area", "iter")])), "\n", sep="\t")
    cat("Range: ", paste(sub('plusgroup', 'pgroup', names(stock@range)),
      collapse="\t"), "\n")
		cat("", stock@range, "\n", sep="\t")

    # sr
    cat("-- sr\n")
    sr <- sr(object)
   
    cat("Model: \t")
    print(model(sr), showEnv=FALSE)
    # params
    print(params(sr), reduced=TRUE)
    
    cat("\n")

    # refpts
    cat("-- refpts\n")
    refpts <- refpts(object)

    rows <- as.logical(c(apply(refpts, c(1,2),
      function(x) sum(is.na(x)) < length(x))))
    cols <- as.logical(c(apply(refpts, c(2,1),
      function(x) sum(is.na(x)) < length(x))))

    print(refpts[rows, cols,])

    cat("\n")

    # fleetBehaviour
    cat("-- fleetBehaviour\n")
    behaviour <- fleetBehaviour(object)
    
    cat("Method: ", find.original.name(method(behaviour)), "\n")
    cat("Args: ", names(unlist(args(behaviour))), "\n", sep="\t")
    cat("", unlist(args(behaviour)), "\n", sep="\t")

    # projection
    cat("-- projection\n")
    projection <- projection(object)

    cat("Method: ", find.original.name(method(projection)), "\n")
    cat("Args: ", names(unlist(args(projection))), "\n", sep="\t")
    cat("", unlist(args(projection)), "\n", sep="\t")
  }
) # }}}

# plot {{{

setMethod("plot", signature(x="FLom", y="list"),
  function(x, y) {

    do.call("plot", c(list(x=x), y))
  }
)

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

    return(x)

  }
) # }}}

# fwd {{{

#' @examples
#' data(p4om)
#' res <- fwd(om, control=fwdControl(year=2018:2030, quant="f",
#'   value=rep(c(refpts(om)$FMSY), 13)))

setMethod("fwd", signature(object="FLom", fishery="missing", control="fwdControl"),
  function(object, control, maxF=4, ...) {
    
    stock(object) <- fwd(stock(object), sr=sr(object), control=control,
      maxF=maxF, ...)

    return(object)
  })

# }}}

# fwd.om {{{

#' A method to project the operating model (OM)
#'
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#' Suspendisse eu sapien vitae arcu lobortis ultrices vitae ac velit. Curabitur id 
#'
#' @name fwd.om
#' @rdname fwd.om
#' @aliases fwd.om
#' @param object the OM as a FLStock
#' @param ctrl the fwdControl object with objectives and constraints
#' @param sr a FLSR with the stock-recruitment model
#' @param sr.residuals a FLQuant with S/R residuals
#' @param sr.residuals.mult logical about residuals being multiplicative
 
fwd.om <- function(om, ctrl, ...){
	
  args <- list(...)

	args$object <- om
	args$control <- ctrl
	
  om <- do.call("fwd", args)

	list(om=om)
}
# }}}

# iter {{{

setMethod("iter", signature(obj="FLo"),
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
#' data(ple4om)
#' comb <- combine(iter(om, 1:10), iter(om, 11:25))
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
    metrics(object, list(SB=ssb, C=catch, F=fbar))
}) # }}}
