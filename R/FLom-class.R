# FLom-class.R - DESC
# mse/R/FLom-class.R

# Copyright European Union, 2018
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

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
	slots=c(
		stock="FLStock",
		sr="FLSR",
		refpts="FLPar",
		fleetBehaviour="mseCtrl",
		projection="mseCtrl"
	)
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

#
#  accessor methods
#

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
#' @param value the new object
#' @aliases sr<- sr<--methods
setGeneric("sr<-", function(object, value) standardGeneric("sr<-"))
#' @rdname FLom-class
setReplaceMethod("sr", signature("FLom", "FLSR"), function(object, value){
	object@sr <- value
	object
})

#' @rdname FLom-class
#' @aliases refpts refpts-methods
#' @rdname FLom-class
setMethod("refpts", "FLom", function(object) object@refpts)

#' @rdname FLom-class
#' @param value the new object
setReplaceMethod("refpts", signature("FLom", "FLPar"), function(object, value){
	object@refpts <- value
	object
})

#' @rdname FLom-class
#' @aliases fleetBehaviour fleetBehaviour-methods
setGeneric("fleetBehaviour", function(object, ...) standardGeneric("fleetBehaviour"))
#' @rdname FLom-class
setMethod("fleetBehaviour", "FLom", function(object) object@fleetBehaviour)

#' @rdname FLom-class
#' @param value the new object
#' @aliases fleetBehaviour<- fleetBehaviour<--methods
setGeneric("fleetBehaviour<-", function(object, value) standardGeneric("fleetBehaviour<-"))
#' @rdname FLoem-class
setReplaceMethod("fleetBehaviour", signature("FLom", "mseCtrl"), function(object, value){
	object@fleetBehaviour <- value
	object
})

#' @rdname FLom-class
#' @aliases projection projection-methods
setGeneric("projection", function(object, ...) standardGeneric("projection"))
#' @rdname FLom-class
setMethod("projection", "FLom", function(object) object@projection)

#' @rdname FLom-class
#' @param value the new object
#' @aliases projection<- projection<--methods
setGeneric("projection<-", function(object, value) standardGeneric("projection<-"))
#' @rdname FLoem-class
setReplaceMethod("projection", signature("FLom", "mseCtrl"), function(object, value){
	object@projection <- value
	object
})


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
 
fwd.om <- function(stk, ctrl, sr, ...){
	args <- list(...)
	args$object <- stk
	args$control <- ctrl
	args$sr <- sr
	stk <- do.call("fwd", args)
	list(object=stk)
}

# summary {{{

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

    rows <- as.logical(c(apply(refpts, c(1,3),
      function(x) sum(is.na(x)) < length(x))))
    cols <- as.logical(c(apply(refpts, c(2,3),
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
setMethod("plot", signature(x="FLom", y="missing"),
  function(x, ...) {

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

    stocks[[1]] <- window(stocks[[1]], end=dims(stocks[[2]])$minyear)

    do.call("plot", c(list(x=FLStocks(stocks)), args[!cls]))
    
    } else {
    
      plot(stock(x), ...)
    
    }
  }
) # }}}
