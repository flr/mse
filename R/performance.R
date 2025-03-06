# performance.R - DESC
# ioalbmse/R/performance.R

# Copyright European Union, 2016-17
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

globalVariables("statistic")

# performance(FLQuants) {{{

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
#' data(sol274)
#' # GENERATE pseudo-run from last 20 years of OM
#' run <- window(stock(om), start=2012, end=2021)
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
#'   metrics=list(C=catch, F=fbar), years=list(short=2016:2018, long=2016:2021))
#' # Minimum statistic, named list with formula and name
#' performance(run, statistics=list(CMSY=list(~yearMeans(C/MSY), name="CMSY")),
#'   refpts=FLPar(MSY=110000), metrics=list(C=catch, F=fbar),
#'   years=list(2012:2021))
#' # return quantiles
#' performance(run, statistics, refpts=FLPar(MSY=110000),
#'   metrics=list(C=catch, F=fbar), years=list(2012:2021),
#'   probs =  c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
#' # DEFINE statistics without summaries
#' statistics <- list(
#'   CMSY=list(~yearMeans(C/MSY),
#'     name="CMSY",
#'     desc="Catch over MSY"))
#' # COMPUTE performance
#' perf <- performance(run, statistics, refpts=FLPar(MSY=110000),
#'   metrics=list(C=catch), years=list(2012:2021))
#' # COMPUTE summaries
#' perf[, .(CMSY=mean(data))]

setMethod("performance", signature(x="FLQuants"),
  function(x, statistics, refpts=FLPar(),
    years=setNames(list(dimnames(x[[1]])$year), nm=dims(x[[1]])$maxyear),
    probs=c(0.1, 0.25, 0.50, 0.75, 0.90),
    om=NULL, type=NULL, run=NULL, mp=paste(c(om, type, run), collapse="_"), ...) {

    # CHECK x /refpts names cover all required by statistics
    stats.names <- unique(unlist(lapply(statistics,
      function(x) all.vars(x[[1]][[2]]))))

    # DROP functions
    stats.names <- stats.names[!unlist(lapply(stats.names, exists))]

    # GET names in refpts and metrics, plus FLQuant dimnames
    valid.names <- c(dimnames(refpts)$params, names(x),
      c("age", "year", "unit", "season", "area"))
    
    if(!all(stats.names %in% valid.names))
      stop("Name of metric, refpt or function in statistics not found: ",
        paste(stats.names[!stats.names %in% valid.names], collapse=", "))
    
    # CREATE years list
    if(!is.list(years))
      years <- setNames(as.list(years), nm=as.character(years))

    # ADD names if NULL
    if(is.null(names(years))) {
      names(years) <- as.character(unlist(lapply(years, tail, 1)))
    } else {
      # ADD name by element on missing, use last year
      names(years)[names(years) == ""] <- as.character(unlist(lapply(years,
        tail, 1)))[names(years) == ""]
    }

    # CHECK years names are unique
    if(length(names(years)) > length(unique(names(years))))
      stop(paste0("Names in 'years' are not unique: ",
        paste(names(years)[duplicated(names(years))], collapse=',')))

    # CHECK years are present
    if(!all(as.character(unlist(years)) %in% dimnames(x[[1]])$year))
      stop("years must be present in input object 'x' dimensions.")

    # CHECK dimensions
    if(any(unlist(lapply(x, function(y) any(dim(y)[c(1,3,4,5)] != 1)))))
      warning("metrics have length > 1 for 'quant', 'unit', 'season' or 'area', recycling over refpts might be wrong.")

    # CHECK statistics are unique
    if(length(names(statistics)) != length(unique(names(statistics))))
      stop("'statistics' must have unique names.")

    # ADD name as desc if missing
    statistics <- lapply(statistics, function(x) {
      if(is.null(x$desc)) x$desc <- x$name
      return(x)
    })

    # LOOP over years
    res <- data.table::rbindlist(lapply(years, function(i) {
      # LOOP over statistics
      data.table::rbindlist(lapply(statistics, function(j) {
        # ADD previous year when 1 used and stats is for change
        if(grepl("change|variability", j$desc) & length(i) == 1) {
          i <- seq(an(i) - 1, an(i))
        }
        # EVAL statistic
        as.data.frame(eval(j[names(j) == ""][[1]][[2]],
          c(lapply(x, '[' , j=ac(i)),
            # REPEAT refpts by year because recycling goes year first
            lapply(as(refpts, 'list'), rep, each=length(i)), list(...))), drop=FALSE)

      }), idcol="statistic", fill=TRUE)[,c("statistic", "data", "iter")]
    }), idcol="year")

    # Set DT keys
    setkey(res, statistic, year)
    
    # ADD statistic name(s)
    inds <- lapply(statistics, '[[', 'name')
    descs <- lapply(statistics, '[[', 'desc')
    inds <- data.table(statistic=names(inds), name=unlist(inds),
      desc=unlist(descs))
    setkey(inds, statistic)

    # MERGE
    res <- merge(res, inds, by='statistic')
    
    # QUANTILES if probs
    if(!is.null(probs)) {
      # statistic year name data prob
      res <- res[, .(data=quantile(data, probs=probs, na.rm=TRUE), prob=probs),
        by=.(statistic, year, name, desc)]
    }
    
    # ASSIGN names (om, type, run, mp)
    res[ , `:=` (om = 'NA', type = 'NA', run = 'NA', mp = 'NA')]
    set(res, j=c('om', 'type', 'run', 'mp'), value=list(om, type, run, mp))

    return(res[])
  }
)

# }}}

# performance(FLStock) {{{

#' @rdname performance

setMethod("performance", signature(x="FLStock"),
  function(x, statistics, refpts=FLPar(),
    years=as.character(seq(dims(x)$minyear, dims(x)$maxyear)),
    metrics=FLCore::metrics(x), probs=NULL, ...) {

      # CREATE or PASS FLQuants
      if(is(metrics, "FLQuants"))
        flqs <- metrics
      else if(is.list(metrics) & is.function(metrics[[1]]))
        flqs <- do.call(FLCore::metrics, list(object=x, metrics))
      else if(is.function(metrics))
        flqs <- do.call(metrics, list(x))
      else
        stop("metrics could not be computed")

    return(performance(flqs, refpts=refpts,
      statistics=statistics, years=years, probs=probs, ...))
  }
)
# }}}

# performance(FLStocks) {{{

#' @rdname performance
#' @examples
#' perf <- performance(FLStocks(B=run, A=run), statistics, 
#'   refpts=FLPar(MSY=110000), metrics=list(C=catch), years=list(2012:2015))

setMethod("performance", signature(x="FLStocks"),
  function(x, statistics, refpts=FLPar(), years=dims(x[[1]])$maxyear,
    metrics=FLCore::metrics, probs=NULL, grid=missing, mc.cores=1, ...) {

    if(mc.cores > 1) {
      res <- data.table::rbindlist(parallel::mclapply(x, performance,
        statistics, refpts, years, metrics=metrics, probs=probs,
        mc.cores=mc.cores, ...), idcol='biol')
    } else {
      res <- data.table::rbindlist(lapply(x, performance,
        statistics, refpts, years, metrics=metrics, probs=probs, ...),
        idcol='biol')
    }
    
    # IF grid, ADD columns
    if(!missing(grid)) {
        if(is(grid, "list"))
          dgrid <- data.table(expand.grid(grid))
        if(!"run" %in% colnames(dgrid))
          dgrid[, run:=paste0("R", seq(nrow(dgrid)))]
        res <- merge(res, dgrid, by="run")
      }

    return(res[])
  }
)
# }}}

# performance(list) FLmse / FLQuants {{{

#' @rdname performance

setMethod("performance", signature(x="list"),
  function(x, statistics, refpts=FLPar(), years=dims(x[[1]])$maxyear,
    probs=NULL, grid="missing", mp=NULL, mc.cores=1, ...) {
    
    # COERCE OMs added as reference
    x <- lapply(x, function(i) {
      if(is(i, 'FLo'))
        return(FLmse(om=i))
      else
        return(i)
    })

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
    
    # mp=run if NULL
    if(is.null(mp))
      res[, mp:=run]
    res[, mp:=factor(mp, levels=unique(mp))]
    
    # IF grid, ADD columns
    if(!missing(grid)) {

      if(is(grid, "list"))
        dgrid <- data.table(expand.grid(grid))
      if(!"run" %in% colnames(dgrid))
        dgrid[, run:=paste0("R", seq(nrow(dgrid)))]
      res <- merge(res, dgrid, by="run")
    }
    return(res[])
  }
) 
# }}}

# performance(FLom) {{{

#' @rdname performance

setMethod("performance", signature(x="FLom"),
  function(x, refpts=x@refpts, metrics=NULL, statistics=NULL, ...) {

    # BUG: metrics argument hides metrics method
    if(is.null(metrics))
      metrics <- metrics(x)

    # DEFAULT statistics
    if(is.null(statistics))
      statistics <- mse::statistics[c("SB0", "SBMSY", "FMSY", "C", "AAVC")]
    
    return(performance(stock(x), refpts=refpts, metrics=metrics, 
      statistics=statistics, ...))
  }
)
# }}}

# performance(FLombf) {{{

setMethod("performance", signature(x="FLombf"),
  function(x, statistics, refpts=x@refpts, metrics=NULL,
    years=as.character(seq(dims(x)$minyear + 1, dims(x)$maxyear)),
    probs=NULL, ...) {

    # COMPUTE metrics
    if(is.null(metrics))
      mets <- do.call('metrics', list(object=x))
    else
      mets <- do.call('metrics', list(object=x, metrics=list(C=catch)))

    # CALL performance by biol
    res <- mapply(function(me, rp) {
      performance(x=me, statistics=statistics, refpts=rp, years=years,
        probs=probs, ...)
      }, me=mets, rp=x@refpts, SIMPLIFY=FALSE)

    res <- rbindlist(res, idcol="biol")

    return(res)
  }
)

# }}}

# performance(FLmse) {{{

#' @examples
#' data(sol274)
#' data(statistics)

setMethod("performance", signature(x="FLmse"),
  function(x, ...) {

    args <- list(...)

    # IF no extra args, return 'performance' attribute ...
    if(length(args) == 0) {

      res <- attr(x, 'performance')

      # ... ONLY if it exists
      if(is.null(res))
        stop("'performance' table has not yet been stored in object")
    
      return(res)
    } else {

      # GET hcr args
      control_args <- Filter(is.numeric, args(control(x)$hcr))

      # COMPUTE on x@om
      do.call(performance, c(list(x=x@om), list(...), control_args))
    }
  }
)

setMethod('performance<-', signature(x='FLmse', value="data.frame"),
  function(x, value){
    attr(x, "performance") <- value
    return(x)
})

# }}}

# performance(FLmses) {{{

setMethod("performance", signature(x="FLmses"),
  function(x, ...) {
    # RETURN slot if no other args
    if(length(list(...)) == 0)
      return(slot(x, 'performance'))
    # CALL performance(list)
    else
      callNextMethod()
  } 
)

setMethod('performance<-', signature(x='FLmses', value="data.frame"),
  function(x, value){
    slot(x, 'performance') <- value
    return(x)
})

# }}}


# performance
# FLmses -- FLmse -- FLom   -- FLStock -- FLQuants
#                  - FLombf -- FLQuants
# FLStocks -- FLStock -- FLQuants

# om type mp run
