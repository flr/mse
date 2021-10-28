# performance.R - DESC
# ioalbmse/R/performance.R

# Copyright European Union, 2016-17
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

globalVariables("statistic")

setGeneric("performance", function(x, ...) standardGeneric("performance"))

# performance {{{

#' Compute performance statistics
#'
#' TODO
#'
#' Each statistics is an object of class list object, with three elements, the
#' first two of them compulsory:
#'
#' - An unnamed element of class *formula*, e.g. `yearMeans(SB/SB0)`.
#' - name: A short name to be output on tables and plots, of class character,
#' e.g. "SB/SB[0]".
#' - desc: A longer description of the statistics, of class character, e.g. "Mean
#' spawner biomass relative to unfished"
#'
#' Each statistic `formula` is evaluated against the *metrics* and *refpts* used
#' in the function call. Formulas can thus use (i) the names of the `FLQuants`
#' object or of the object returned by the call to `metrics()`, (ii) of the
#' *params* in the *refpts* object and, for all classes but `FLQuants`, (iii)
#' functions that can be called on *object*. See examples below for the
#' necessary matching between *metrics*, *refpts* and the statistics formulas.
#'
#' @param run Object holding the results of forward projections, as a named
#' \code{FLQuants}
#' @param refpts Reference points for calculations, \code{list}
#' @param statistics statistics to be computed, as formula, name and description, \code{list}
#' @param years Years on which statistics should be computed, defaults to last year of input FLQuants
#'
#' @return data.table Results of computing performance statistics.
#'
#' @name performance
#' @rdname performance
#'
#' @author Iago Mosqueira, EC JRC
#' @seealso \code{\link{FLQuants}}
#' @keywords utilities
#' @examples
#'
#' # LOAD example FLmse object
#' data(ple4om)
#' # GENERATE run from last 16 years of OM
#' run <- window(stock(om), start=2000, end=2015)
#' # DEFINE statistics
#' statistics <- list(
#'   dCatch=list(~yearMeans(C[, -1]/C[, -dims(C)$year]),
#'     name="mean(C[t] / C[t-1])",
#'     desc="Mean absolute proportional change in catch"),
#'   varCatch=list(~yearVars(C),
#'     name="var(C)",
#'     desc="Variance in catch"),
#'   varF=list(~yearVars(F),
#'     name="var(F)",
#'     desc="Variance in fishing mortality"))
#' # COMPUTE performance
#' performance(run, statistics, refpts=FLPar(MSY=110000),
#'   metrics=list(C=catch, F=fbar), years=list(2000:2015))
#' # Minimum statistic, named list with formula and name
#' performance(run, statistics=list(CMSY=list(~C/MSY, name="CMSY")),
#'   refpts=FLPar(MSY=110000), metrics=list(C=catch, F=fbar),
#'   years=list(2000:2015))
#' # return quantiles
#' performance(run, statistics, refpts=FLPar(MSY=110000),
#'   metrics=list(C=catch, F=fbar), years=list(2000:2015),
#'   probs =  c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
#' # DEFINE statistics without summaries
#' statistics <- list(
#'   CMSY=list(~C/MSY,
#'     name="CMSY",
#'     desc="Catch over MSY"))
#' # COMPUTE performance
#' perf <- performance(run, statistics, refpts=FLPar(MSY=110000),
#'   metrics=list(C=catch), years=list(2000:2015))
#' # COMPUTE summaries
#' perf[, .(CMSY=mean(data))]

setMethod("performance", signature(x="FLQuants"),
  function(x, statistics, refpts=FLPar(),
    years=setNames(list(dimnames(x[[1]])$year), nm=dims(x[[1]])$maxyear),
    probs=c(0.1, 0.25, 0.50, 0.75, 0.90), mp=NULL) {

    # CHECK x /refpts names cover all required by statistics
    stats.names <- unique(unlist(lapply(statistics,
      function(x) all.vars(x[[1]][[2]]))))

    # DROP functions
    stats.names <- stats.names[!unlist(lapply(stats.names, exists))]

    # GET names in refpts and metrics, plus FLQuant dimnames
    valid.names <- c(dimnames(refpts)$params, names(x),
      c("age", "year", "unit", "season", "area"))

    if(!all(stats.names %in% valid.names))
      stop("Name of metric, refpt or function in statistics might not be found: ",
        paste(stats.names[!stats.names %in% valid.names], collapse=", "))
    
    # CREATE years list
    if(!is.list(years))
      years <- setNames(as.list(years), as.character(years))

    # CHECK years
    if(any(unlist(lapply(years,
      function(y) !all(as.character(y) %in% dimnames(x[[1]])$year)))))
      stop("years must be present in input object 'x' dimensions.")

    # SET names if not present
    if(is.null(names(years)))
      names(years) <- as.character(unlist(lapply(years,
        function(x) x[length(x)])))

    # CHECK dimensions
    if(any(unlist(lapply(x, function(y) any(dim(y)[c(1,3,4,5)] != 1)))))
      warning("metrics have length > 1 for 'quant', 'unit', 'season' or 'area', recycling over refpts might be wrong.")

    # CHECK statistics are unique
    if(length(names(statistics)) != length(unique(names(statistics))))
      stop("'statistics' must have unique names.")

    # LOOP over years
    res <- data.table::rbindlist(lapply(years, function(i) {
      # LOOP over statistics
      data.table::rbindlist(lapply(statistics, function(j) {
        # EVAL statistic
        as.data.frame(eval(j[names(j) == ""][[1]][[2]],
          c(FLCore::window(x, start=i[1], end=i[length(i)]),
            # REPEAT refpts by year because recycling goes year first
            lapply(as(refpts, 'list'), rep, each=length(i)))), drop=FALSE)
      }), idcol="statistic", fill=TRUE)[,c("statistic", "data", "iter")]
    }), idcol="year")
    
    # Set DT keys
    setkey(res, statistic, year)
    
    # ADD statistic name(s)
    inds <- lapply(statistics, '[[', 'name')
    inds <- data.table(statistic=names(inds), name=unlist(inds))
    setkey(inds, statistic)

    # MERGE
    res <- merge(res, inds, by='statistic')
    
    # QUANTILES if probs
    if(!is.null(probs)) {
      # statistic year name data prob
      res <- res[, .(data=quantile(data, probs=probs, na.rm=TRUE), prob=probs),
        by=.(statistic, year, name)]
    }
    
    # mp if not NULL
    if(!is.null(mp))
      res[, mp:=mp]
	  
    return(res)
  }
)

#' @rdname performance

setMethod("performance", signature(x="FLStock"),
  function(x, statistics, refpts=FLPar(),
    years=as.character(seq(dims(x)$minyear, dims(x)$maxyear)),
    metrics=FLCore::metrics(x), probs=NULL, mp=NULL) {
      
      # CREATE or PASS FLQuants
      if(is(metrics, "FLQuants"))
        flqs <- metrics
      else if(is.list(metrics) & is.function(metrics[[1]]))
        flqs <- do.call(FLCore::metrics, list(object=x, metrics))
      else if(is.function(metrics))
        flqs <- do.call(metrics, list(x))

      return(performance(flqs, refpts=refpts,
      statistics=statistics, years=years, probs=probs, mp=mp))
  }
)

#' @rdname performance

setMethod("performance", signature(x="FLStocks"),
  function(x, statistics, refpts=FLPar(), years=dims(x[[1]])$maxyear,
    metrics=FLCore::metrics, probs=NULL, grid=missing, mp=NULL, mc.cores=1) {

    if(mc.cores > 1) {
      res <- data.table::rbindlist(parallel::mclapply(x, performance,
        statistics, refpts, years, metrics=metrics, probs=probs, mp=mp,
        mc.cores=mc.cores), idcol='run')
    } else {
      res <- data.table::rbindlist(lapply(x, performance,
        statistics, refpts, years, metrics=metrics, probs=probs, mp=mp),
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
  function(x, statistics, refpts=FLPar(), years=dims(x[[1]])$maxyear,
    probs=NULL, grid="missing", mp=NULL, mc.cores=1, ...) {
    
    # HANDLE list(mse)
    if(all(unlist(lapply(x, is, 'FLmse')))) {
      return(rbindlist(lapply(x, function(i) do.call(performance,
        c(list(x=i, refpts=refpts(i), statistics=statistics, years=years,
        probs=probs), list(...)))), idcol="mp"))
    }

    # ELSE assume list of FLQuants

    if(!all(unlist(lapply(x, is, 'FLQuants'))))
      stop("input list must contain objects of class FLQuants")

    if(mc.cores > 1) {
      res <- data.table::rbindlist(parallel::mclapply(x, performance,
        statistics, refpts, years, probs=probs, mp=mp, mc.cores=mc.cores),
        idcol='run')
    } else {
      res <- data.table::rbindlist(lapply(x, performance,
        statistics, refpts, years, probs=probs, mp=mp), idcol='run')
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
) 

#' @rdname performance

setMethod("performance", signature(x="FLom"),
  function(x, refpts=x@refpts, ...) {

    return(performance(stock(x), refpts=refpts, ...))
    
  }
)

setMethod("performance", signature(x="FLombf"),
  function(x, statistics, refpts=x@refpts, metrics,
    years=as.character(seq(dims(x)$minyear, dims(x)$maxyear)),
    probs=NULL, mp=NULL) {
    
    mets <- metrics(x, metrics)

    mets <- lapply(setNames(nm=names(mets[[1]])),
      function(i) FLQuants(lapply(X=mets, FUN="[[", i)))

    res <- mapply(function(xx, rr) {
      performance(x=xx, statistics=statistics, refpts=rr, years=years,
        probs=probs, mp=mp)
      }, xx=mets, rr=x@refpts, SIMPLIFY=FALSE)

    return(rbindlist(res, idcol="biol"))
  }
)

setMethod("performance", signature(x="FLmse"),
  function(x, ...) {
    performance(x@om, ...)
  }
)

# }}}
