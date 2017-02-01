# performance.R - DESC
# ioalbmse/R/performance.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

globalVariables("indicator")

# performance {{{

#' Compute performance indicators
#'
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#' Suspendisse eu sapien vitae arcu lobortis ultrices vitae ac velit. Curabitur id 
#' nunc euismod ante fringilla lobortis. Aliquam ullamcorper in diam non placerat. 
#'
#' Aliquam sagittis feugiat felis eget consequat. Praesent eleifend dolor massa, 
#' vitae faucibus justo lacinia a. Cras sed erat et magna pharetra bibendum quis in 
#' mi. Sed sodales mollis arcu, sit amet venenatis lorem fringilla vel. Vivamus vitae 
#' ipsum sem. Donec malesuada purus at libero bibendum accumsan. Donec ipsum sapien, 
#' feugiat blandit arcu in, dapibus dictum felis. 
#'
#' @param run Object holding the results of forward projections, as a named \code{FLQuants}
#' @param refpts Reference points for calculations, \code{list}
#' @param indicators Indicators to be computed, as formula, name and description, \code{list}
#' @param years Years on which indicators should be computed, defaults to last year of input FLQuants
#'
#' @return data.frame Results of computing performance indicators 
#'
#' @name performance
#' @rdname performance
#' @aliases performance
#'
#' @author Iago Mosqueira, EC JRC
#' @seealso \code{\link{FLQuants}}
#' @keywords utilities
#' @examples
#'
#' data(cod)
#' indicators <- list(T1=list(~yearMeans(C[, -1]/C[, -dims(C)$year]),
#'   name="mean(C_t / C_t-1)", desc="Mean absolute proportional change in catch"),
#'   T2=list(~yearVars(C), name="var(C)", desc="Variance in catch"),
#'   T3=list(~yearVars(F), name="var(F)", desc="Variance in fishing mortality"))
#' run <- window(cod, start=20)
#' performance(run, indicators, refpts=FLPar(MSY=0))

setGeneric("performance", function(x, ...) standardGeneric("performance"))

setMethod("performance", signature(x="FLStock"),
  function(x, indicators, refpts, years=dims(x[[1]])$maxyear, probs=NULL) {
  
    # TODO Generalize
    x <- metrics(x, SB=ssb, B=stock, C=catch, F=fbar)

    return(performance(x, refpts=refpts, indicators=indicators,
      years=years, probs=probs))
  }
)

setMethod("performance", signature(x="FLQuants"),
  function(x, indicators, refpts, years=dims(x[[1]])$maxyear,
    probs=c(0.1, 0.25, 0.50, 0.75, 0.90)) {
  
    # CREATE years list, numeric names
    years <- as.list(years)
    names(years) <- unlist(years)

    # LOOP over years
    res <- data.table::rbindlist(lapply(years, function(i)
      # LOOP over indicators
      data.table::rbindlist(lapply(indicators, function(j)
        # EVAL indicator
        as.data.frame(eval(j[names(j) == ""][[1]][[2]],
          c(window(x, end=i), as(refpts, 'list'))), drop=TRUE)),
          idcol="indicator")), idcol="year")
    # Set DT keys
    setkey(res, indicator, year)
    
    # ADD indicator name(s)
    inds <- lapply(indicators, '[[', 'name')
    inds <- data.table(indicator=names(inds), name=unlist(inds))
    setkey(inds, indicator)

    res <- merge(res, inds, by='indicator')

    if(!is.null(probs))
      res <- res[, as.list(quantile(data, probs=probs, na.rm=TRUE)),
        keyby=list(indicator, name, year)]

    # TODO Return quantiles if asked, NULL or FALSE for full?
#    if(length(probs) > 0)
#      res <- res[, as.list(quantile(data, probs=probs, na.rm=TRUE)),
#  keyby=list(indicator, name, year)]

	  return(res)
  }
)

setMethod("performance", signature(x="FLStocks"),
  function(x, indicators, refpts, years=dims(x[[1]])$maxyear, probs=NULL) {

    return(data.table::rbindlist(lapply(x, performance,
      indicators, refpts, years, probs), idcol='run'))
  }
) # }}}

# metrics {{{
setGeneric("metrics", function(x, ...) standardGeneric("metrics"))

setMethod("metrics", signature(x="FLStock"),
  function(x, ...) {
    
    args <- list(...)

    return(FLQuants(lapply(args, function(y)
      if(is(y, "formula"))
        if(is(y[[length(y)]], "name"))
          do.call(as.character(y[[length(y)]]), list(x))
        else
          eval(y[[length(y)]], list(x))
      else if (is(y, "function"))
        do.call(y, list(x))
      else
        stop("metrics expression cannot be evaluated"))))
  }
) # }}}
