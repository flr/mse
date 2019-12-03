# performance.R - DESC
# ioalbmse/R/performance.R

# Copyright European Union, 2016-17
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
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
#'
#' @author Iago Mosqueira, EC JRC
#' @seealso \code{\link{FLQuants}}
#' @keywords utilities
#' @examples
#'
#' data(cod)
#' indicators <- list(
#'   T1=list(~yearMeans(C[, -1]/C[, -dims(C)$year]), name="mean(C[t] / C[t-1])",
#'     desc="Mean absolute proportional change in catch"),
#'   T2=list(~yearVars(C), name="var(C)", desc="Variance in catch"),
#'   T3=list(~yearVars(F), name="var(F)", desc="Variance in fishing mortality"))
#' run <- window(cod, start=20)
#' performance(run, indicators, refpts=FLPar(MSY=0),
#'   metrics=list(C=catch, F=fbar), years=list(20:25, 20:30))

setMethod("performance", signature(x="FLStock"),
  function(x, indicators, refpts=FLPar(),
    years=as.character(seq(dims(x)$minyear, dims(x)$maxyear)),
    metrics=FLCore::metrics(x), probs=NULL, mp=NULL) {
      
      if(is.list(metrics))
        flqs <- do.call(FLCore::metrics, list(object=x, metrics))

      else if(is.function(metrics))
        flqs <- do.call(metrics, list(x))

      return(performance(flqs, refpts=refpts,
      indicators=indicators, years=years, probs=probs, mp=mp))
  }
)

#' @rdname performance

setMethod("performance", signature(x="FLQuants"),
  function(x, indicators, refpts=FLPar(), years=dims(x[[1]])$maxyear,
    probs=c(0.1, 0.25, 0.50, 0.75, 0.90), mp=NULL) {
    
    # TODO CHECK indicators

    # CREATE years list
    if(!is.list(years))
      years <- setNames(as.list(years), as.character(years))

    # SET names if not present
    if(is.null(names(years)))
      names(years) <- as.character(unlist(lapply(years,
        function(x) x[length(x)])))
    
    # LOOP over years
    res <- data.table::rbindlist(lapply(years, function(i) {
      # LOOP over indicators
      data.table::rbindlist(lapply(indicators, function(j) {
        # EVAL indicator
        # TODO CHECK colnames, sort or add if necessary [data, iter]
        as.data.frame(eval(j[names(j) == ""][[1]][[2]],
          c(FLCore::window(x, start=i[1], end=i[length(i)]), as(refpts, 'list'))),
          drop=FALSE)
      }), idcol="indicator", fill=TRUE)[,c("indicator", "data", "iter")]
    }), idcol="year")
    
    # Set DT keys
    setkey(res, indicator, year)
    
    # ADD indicator name(s)
    inds <- lapply(indicators, '[[', 'name')
    inds <- data.table(indicator=names(inds), name=unlist(inds))
    setkey(inds, indicator)

    # MERGE
    res <- merge(res, inds, by='indicator')
    
    # QUANTILES if probs
    if(!is.null(probs)) {
      res <- res[, as.list(quantile(res, probs=probs, na.rm=TRUE)),
        keyby=list(indicator, name, year)]
    }
    
    # mp if not NULL
    if(!is.null(mp))
      res[, mp:=paste0(mp, run)]
	  
    return(res)
  }
)

#' @rdname performance

setMethod("performance", signature(x="FLStocks"),
  function(x, indicators, refpts=FLPar(), years=dims(x[[1]])$maxyear,
    metrics=FLCore::metrics, probs=NULL, grid=missing, mp=NULL, mc.cores=1) {

    if(mc.cores > 1) {
      res <- data.table::rbindlist(parallel::mclapply(x, performance,
        indicators, refpts, years, metrics=metrics, probs=probs, mp=mp,
        mc.cores=mc.cores), idcol='run')
    } else {

      res <- data.table::rbindlist(lapply(x, performance,
        indicators, refpts, years, metrics=metrics, probs=probs, mp=mp),
        idcol='run')
    }
    
    # mp=run if NULL
    if(is.null(mp))
      res[, mp:=run]
    
    # IF grid, ADD columns
    if(!missing(grid)) {

        if(is(grid, "list"))
          dgrid <- data.table(expand.grid(grid))
        if(!"run" %in% colnames(dgrid))
          dgrid[, run:=paste0("R", seq(nrow(dgrid)))]
        res <- merge(res, dgrid, by="run")
      }
    return(res)
  }
) 

#' @rdname performance

setMethod("performance", signature(x="list"),
  function(x, indicators, refpts=FLPar(), years=dims(x[[1]])$maxyear,
    probs=NULL, grid=missing, mp=NULL, mc.cores=1) {

    if(!all(unlist(lapply(x, is, 'FLQuants'))))
      stop("input list must contain objects of class FLQuants")

    if(mc.cores > 1) {
      res <- data.table::rbindlist(parallel::mclapply(x, performance,
        indicators, refpts, years, probs=probs, mp=mp, mc.cores=mc.cores),
        idcol='run')
    } else {
      res <- data.table::rbindlist(lapply(x, performance,
        indicators, refpts, years, probs=probs, mp=mp), idcol='run')
    }
    
    # mp=run if not NULL
    if(is.null(mp))
      res[, mp:=run]
    
    # IF grid, ADD columns
    if(!missing(grid)) {

      if(is(grid, "list"))
        dgrid <- data.table(expand.grid(grid))
      if(!"run" %in% colnames(dgrid))
        dgrid[, run:=paste0("R", seq(nrow(dgrid)))]
      res <- merge(res, dgrid, by="run")
    }
    return(res)
  }
) # }}}
