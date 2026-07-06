# utilities.R - DESC
# mse/R/utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# merge (FLQuant, data.table) {{{

setMethod("merge", signature(x="FLQuant", y="data.table"),
  function(x, y, by="iter", ...) {

  # CONVERT FLQ to df, all options
  xd <- as.data.frame(x, cohort=TRUE, date=TRUE)

  # MERGE by
  return(merge(xd, y, by=by))
})

# }}}

# .combinegoFish {{{
.combinegoFish <- function(...) {
 
  res <- list(...)

  return(
  list(
    om = Reduce("combine", lapply(res, '[[', "om")),
    tracking = rbindlist(lapply(res, '[[', "tracking")),
    oem = Reduce("combine", lapply(res, '[[', "oem"))
		)
  )
}
# }}}

# stamp.fun {{{

stamp.fun <- function(fun) {
  if(!is.null(attr(fun, ".name")))
    return(fun)
  attr(fun, ".name") <- find_original_name(fun)
  fun
}

# }}}

# combinations {{{

combinations <- function(...) {

  # GET all inputs
  args <- list(...)
  
  # GENERATE all combinations
  combs <- as.list(do.call(expand.grid, args))

  # DELETE attributes and RENAME
  attributes(combs) <- NULL
  names(combs) <- names(args)

  return(combs)
}
# }}}

# loadlist {{{
loadlist <- function(file) {
  mget(load(file, verbose=FALSE, envir=(.NE <- new.env())), envir=.NE)
}
# }}}

# setFCB {{{
setFCB <- function(output=c("catch", "landings", "discards", "fbar", "f",
  "effort"), relative=FALSE, element=1) {

  # SELECT output from possible values
  output <- match.arg(output)

  # EXTRACT valid FCB for output
  targets <- subset(FLasher:::.vfcb, quant == output)[1,]

  # BUILD FCB list
  fcb <- ifelse(unlist(targets[c("fishery", "catch", "biol")]),
    element, as.numeric(NA))

  # ADD relative if needed
  if(relative)
    fcb <- c(fcb, setNames(fcb, nm=c("relFishery", "relCatch", "relBiol")))

  fcb
}
# }}}

# selecMetric {{{

#' Select and/or Compute a Metric from the *stk* and *ind* inputs
#'
#' A metric, defined here as a time series, commonly age-aggregated, is computed or extracted
#' from the input FLStock (*stk*) and FLQuants (*ind*). These have been returned by the call to the
#' *est*inmation step in a call to `mp()`.
#'
#' If *metric* is a character string and matches a name in *ind*, then that *FLQuant* element
#' is returned. Otherwise, or if *metric* is a function, it is called on *stk*. See examples below.
#'
#' @param metric A metric to use, which can be one of the following:
#'   - `missing`: Defaults to the first element of the `ind` object if only one element is present.
#'   - `character`: The name of a metric in `ind` to extract, or the name of a function to compute the metric.
#'   - `function`: A function to compute the metric with *stk* as input.
#' @param stk The stock object, used for computing the metric (if applicable).
#' @param ind A FLQuants object containing potential metrics, used for extraction based on `metric`.
#' @param ... Additional arguments passed to the function `metric` (if it is a function or callable).
#'
#' @return The selected or computed metric, either extracted from `ind` or computed using `stk` and `metric`.
#' @author Iago Mosqueira (WUR)
#' @examples
#' data(ple4)
#' # Computes 'catch' metric from 'stk', 'ind' is empty
#' selectMetric("catch", stk=ple4, ind=FLQuants())
#'
#' # Computes own rfunction (ratio of discards to landings) as metric from 'stk'
#' selectMetric(function(x) discards(x) / landings(x), stk=ple4, ind=FLQuants())
#'
#' # Returns 'catch' metric from 'ind' (defined as log), takes precedence over 'stk'
#' selectMetric("catch", stk=ple4, ind=FLQuants(catch=log(catch(ple4))))
#'
#' # Any function available for 'stk' works
#' selectMetric(ssb, stk=ple4, ind=FLQuants())

selectMetric <- function(metric="missing", stk, ind, ...) {

  # MISSING metric? ind
  if(missing(metric)) {
    if(length(ind) == 1) {
      met <- ind[[1]]
    } else {
      met <- ind
    }
  # CHARACTER?
  } else if (is(metric, "character")) {
    # EXTRACT from ind,
    if(metric %in% names(ind))
      met <- ind[[metric]]
    # or COMPUTE from stk
    else
      met <- do.call(metric, c(list(stk), list(...)))
  # FUNCTION?
  } else if(is(metric, "function")) {
    met <- do.call(metric, c(list(stk), list(...)))
  }

  return(met)
}
# }}}

# setmethod {{{

setmethod <- function(x, method) {
  method(x) <- method
  return(x)
}
# }}}

# .print_args {{{
.print_args <- function(x, width = getOption("width")) {

  fmt <- function(obj) {

    ## Character scalar
    if (is.character(obj) && length(obj) == 1)
      return(sprintf("'%s'", obj))

    if (is.character(obj) && length(obj) > 1)
      return(paste(unname(obj), collapse = ", "))

    ## Numeric scalar (named or unnamed)
    if (is.numeric(obj) && length(obj) == 1)
      return(format(unname(obj), trim = TRUE))

    ## Other atomic scalars
    if (is.atomic(obj) && length(obj) == 1)
      return(as.character(unname(obj)))

    ## Numeric vectors
    if (is.numeric(obj) && is.null(dim(obj)))
      return(paste(format(unname(obj), trim = TRUE), collapse = " "))

    ## Everything else
    cl <- paste(class(obj), collapse = "/")

    if (!is.null(dim(obj)))
      return(sprintf("<%s [%s]>", cl, paste(dim(obj), collapse = "x")))

    sprintf("<%s len=%d>", cl, length(obj))
  }

  items <- mapply(function(nm, obj)
    sprintf("%s: %s", nm, fmt(obj)),
    names(x), x,
    USE.NAMES = FALSE)

  cat(paste(strwrap(paste(items, collapse = "; "), width = width),
    collapse = "\n"), "\n")

  invisible(x)
}
# }}}
