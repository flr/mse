# performance.R - DESC
# mse/R/performance.R

# Copyright European Union, 2016-17
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.
 
globalVariables(c(".", "data", "mp", "om", "run", "statistic"))

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
  function(x, statistics=mse::statistics[c("C", "F", "SB", "AAVC")],
    refpts=FLPar(), years=setNames(nm=dimnames(x[[1]])$year[-1]), probs=NULL,
    om=NULL, type=NULL, run=NULL, mp=paste(c(om, type, run), collapse="_"), ...) {

    # TODO: CHECK statistics are all valid

    # CHECK x /refpts names cover all required by statistics
    stats.names <- unique(unlist(lapply(statistics,
      function(x) all.vars(x[[1]][[2]]))))

    # DROP functions
    stats.names <- stats.names[!unlist(lapply(stats.names, exists))]

    # GET names in refpts and metrics, plus FLQuant dimnames
    valid.names <- c(dimnames(refpts)$params, names(x),
      c("age", "year", "unit", "season", "area"))
    
    #if(!all(stats.names %in% valid.names))
    #  stop("Name of metric, refpt or function in statistics not found: ",
    #    paste(stats.names[!stats.names %in% valid.names], collapse=", "))
    
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
    res <- data.table::rbindlist(Map(function(i, ni) {
      # LOOP over statistics
    data.table::rbindlist(lapply(statistics, function(j) {

        # ADD previous year when 1 used and stats is for change
        if(grepl("change|variability", j$desc) & length(i) == 1) {
          i <- seq(an(i) - 1, an(i))
        }

        # ASSEMBLE inputs
        inp <- c(lapply(x, '[' , j=ac(i)),
          # REPEAT refpts by year because recycling goes year first
          lapply(lapply(as(refpts, 'list'), function(x)
            x[!is.na(x)]), rep, each=length(i)), list(...))

        # EVAL statistic if names match with existing object (function) or in inp
        if(all(unlist(lapply(all.vars(j[names(j) == ""][[1]][[2]]), function(x)
          exists(x) | x %in% names(inp))))) {

          res <- data.table(as.data.frame(eval(j[names(j) == ""][[1]][[2]], inp),
            drop=FALSE))

          # COMPUTE mean and LABEL
          res <- res[, .(data=mean(data), year=ni), by=.(age, unit, season, area, iter)]

          return(res[, .(age, unit, season, area, iter, data)])
          
        } else {

          # WARN and return empty
          warning(paste0("statistic '", j$name, "' could not be computed, check metrics or tracking."))

          return(NULL)
        }
      }), idcol="statistic", fill=TRUE)[,c("statistic", "data", "iter")]
    }, i=years, ni=names(years)), idcol="year")

    # SET year as numeric, TODO:combine with periods
    if(!any(is.na(suppressWarnings(as.numeric(res[, year])))))
      res[, year := as.numeric(year)]

    # Set DT keys
    setkey(res, statistic, year)
    
    # ADD statistic name(s) &description(s)
    inds <- lapply(statistics, '[[', 'name')
    descs <- lapply(statistics, '[[', 'desc')

    inds <- data.table(statistic=names(inds), name=unlist(inds), desc=unlist(descs))
    setkey(inds, statistic)

    # MERGE
    res <- merge(res, inds, by='statistic')
    
    # QUANTILES if probs
    if(!is.null(probs)) {
      # statistic year name data prob
      res <- res[, .(data=quantile(data, probs=probs, na.rm=TRUE), prob=probs),
        by=.(statistic, year, name, desc)]
    }

    # CREATE empty cols
    set(res, j=c('om', 'type', 'run', 'mp'), value=as.list(rep(character(1), 4)))

    # ASSIGN names (om, type, run, mp)
    set(res, j=c('om', 'type', 'run', 'mp'), value=list(om, type, run, mp))

    return(res[])
  }
)

# }}}

# performance(FLStock) {{{

#' @rdname performance

setMethod("performance", signature(x="FLStock"),
  function(x, statistics, metrics=FLCore::metrics(x), ...) {

      # CREATE or PASS FLQuants
      if(is(metrics, "FLQuants"))
        flqs <- metrics
      else if(is.list(metrics) & is.function(metrics[[1]]))
        flqs <- do.call(FLCore::metrics, list(object=x, metrics))
      else if(is.function(metrics))
        flqs <- do.call(metrics, list(x))
      else
        stop("metrics could not be computed")

    return(performance(flqs, statistics=statistics, ...)[])
  }
)
# }}}

# performance(FLStocks) {{{

#' @rdname performance
#' @examples
#' perf <- performance(FLStocks(B=run, A=run), statistics, 
#'   refpts=FLPar(MSY=110000), metrics=list(C=catch), years=list(2012:2015))

setMethod("performance", signature(x="FLStocks"),
  function(x, statistics, metrics=FLCore::metrics, refpts=FLPar(),
    years=seq(dims(x[[1]])$minyear + 1, dims(x[[1]])$maxyear),
    probs=NULL, mc.cores=1, ...) {

    if(mc.cores > 1) {
      res <- data.table::rbindlist(parallel::mclapply(x, performance,
        statistics=statistics, metrics=metrics, refpts=refpts, years=years,
        probs=probs, mc.cores=mc.cores, ...), idcol='biol')
    } else {
      res <- data.table::rbindlist(lapply(x, performance,
        statistics=statistics, metrics=metrics, refpts=refpts, years=years,
        probs=probs, ...), idcol='biol')
    }
    
    return(res[]) 
  })
# }}}

# performance(list) FLmse / FLQuants {{{

#' @rdname performance

setMethod("performance", signature(x="list"),
  function(x, statistics, refpts=FLPar(),
    years=seq(dims(x[[1]])$minyear + 1, dims(x[[1]])$maxyear),
    probs=NULL, mc.cores=1, ...) {

    # HANDLE list(FLom | FLombf)
    if(all(unlist(lapply(x, is, 'FLo')))) {
      if(missing(statistics))
        statistics <- mse::statistics[c('C', 'F', 'SB')]

      # TODO: ADD om if missing, via idcol or :=, DROP Map
      res <- rbindlist(Map(function(i, j) do.call(performance, c(list(x=i,
        refpts=refpts(i), statistics=statistics, years=years, probs=probs),
        list(...))), i=x, j=names(x)))

      return(res[])
    }
 
    # COERCE OMs added as reference
    if(any(unlist(lapply(x, is, 'FLo')))) {
      x <- lapply(x, function(i) {
        if(is(i, 'FLo'))
          return(FLmse(om=i))
        else
          return(i)

      })
    }

    # HANDLE list(FLmses), assumes performance is stored
    if(all(unlist(lapply(x, is, 'FLmses')))) {
      return(rbindlist(lapply(x, function(i) performance(i))))
    }

    # HANDLE list(mse) | FLmses
    if(all(unlist(lapply(x, is, 'FLmse')))) {

      res <- rbindlist(Map(function(i, j) do.call(performance, c(list(x=i,
        refpts=refpts(i), statistics=statistics, years=years, probs=probs, 
        run=j, om=name(om(i))), list(...))), i=x, j=names(x)))

      # SET mp if possible
      # TODO: IF type, run missing
      if(!"mp" %in% colnames(res) & all(c("type", "run") %in% colnames(res)))
        res[, mp:=paste(om, type, run, sep="_")]

      return(res[])
    }
         
    # ELSE assume list of FLQuants
    if(!all(unlist(lapply(x, is, 'FLQuants'))))
      stop("input list must contain objects of class FLQuants")

    if(mc.cores > 1) {
      res <- data.table::rbindlist(parallel::mclapply(x, performance,
        statistics=statistics, refpts=refpts, years=years, probs=probs,
        mp=mp, mc.cores=mc.cores), idcol='run')
    } else {
      res <- data.table::rbindlist(lapply(x, performance,
        statistics=statistics, refpts=refpts, years=years, probs=probs, mp=mp),
        idcol='run')
    }
    
    return(res[])
  }
) 
# }}}

# performance(FLom) {{{

#' @rdname performance

setMethod("performance", signature(x="FLom"),
  function(x, refpts=x@refpts, statistics=mse::statistics[c('C', 'F', 'SB')],
    metrics=NULL, om=name(x), ...) {

    # BUG: metrics argument hides metrics method
    if(is.null(metrics))
      metrics <- metrics(x)

    # SET NULL name if missing
    if(length(om) == 0)
      om <- NULL

    return(performance(stock(x), refpts=refpts, metrics=metrics, 
      statistics=statistics, om=om, ...))
  }
)
# }}}

# performance(FLombf) {{{
setMethod("performance", signature(x="FLombf"),
  function(x, statistics=NULL, refpts=x@refpts,
    metrics=NULL, years=as.character(seq(dims(x)$minyear + 1, dims(x)$maxyear)),
    probs=NULL, om=name(x), ...) {

    # COMPUTE metrics
    if(is.null(metrics))
      mets <- do.call('metrics', list(object=x))
    else
      mets <- do.call('metrics', list(object=x, metrics=metrics))

    if(is.null(statistics))
      statistics <- mse_statistics()[c("C", "SB", "F")]

    # CALL performance by biol
    res <- mapply(function(me, rp) {
      performance(x=me, statistics=statistics, refpts=rp, years=years,
        probs=probs, ...)
      }, me=mets, rp=x@refpts, SIMPLIFY=FALSE)

    res <- rbindlist(res, idcol="biol")

    # ADD om and drop mp
    res[, om:=get('om')]
    res[, mp:=NULL]

    return(res[])
  }
)

# }}}

# performance(FLmse) {{{

#' @examples
#' data(sol274)
#' data(statistics)

setMethod("performance", signature(x="FLmse"),
  function(x, om=name(x@om), control=FALSE, type="MP", run="1", ...) {

    args <- list(...)

    res <- attr(x, 'performance')

    # IF no extra args, return 'performance' attribute ...
    if(length(args) == 0 & !is.null(res)) {

      return(res)

    } else {

      # GET hcr args
      control_args <- Filter(is.numeric, args(control(x)$hcr))

      # GET tracking elements as FLQuants
      if(length(tracking(x)) > 0) {
        tracks <- copy(tracking(x))
        setnames(tracks, 1:2, c('quant', 'qname'))
        tracks <- as(tracks, 'FLQuants')
      } else {
        tracks <- NULL
      }

      # COMPUTE on x@om      
      # TODO: MISSING iter
      res <- do.call(performance, c(list(x=x@om), args, control_args,
        tracks, om=name(x@om), type=type, run=run))

      # NAME mp if  possible
      # TODO: IF type, run missing
      if(!"mp" %in% colnames(res) & all(c("type", "run") %in% colnames(res)))
        res[, mp:=paste(om, type, run, sep="_")]

      # ADD control$hcr@args
      if(control) {
          
        hcrargs <- args(control(x, 'hcr'))

        # USE only numeric or FLQuant/FLPar with single dims but 'iter', and that 1 | iter(om)
        hcrargs <- lapply(hcrargs, function(i) {
        
          if(is(i, "FLQuant")) {
            if(any(dim(i)[-6] > 1) | !dim(i)[6] %in% c(1, dims(x)$iter))
              return(NULL)
            else
              return(c(i))
          } else if (is(i, "FLPar")) {
            if(any(dim(i)[-length(dim(i))] > 1 |
              !dim(i)[length(dim(i))] %in% c(1, dims(x)$iter)))
              return(NULL)
          else
              return(c(i))
          } else {
            return(i)
          }
        })

        res <- cbind(res, as.data.table(hcrargs))
      }

      # SET key
      setkeyv(res, c("mp", "type", "statistic", "year"))

      # SET column order
      if(is(x@om, "FLombf")) 
        setcolorder(res, c("om", "biol", "statistic", "name", "desc", "year",
          "iter", "data", "type", "run", "mp"))
      else
        setcolorder(res, c("om", "statistic", "name", "desc", "year",
          "iter", "data", "type", "run", "mp"))

      return(res[])
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

    args <- list(...)

    # RETURN slot if no other args
    if(length(args) == 0)
      return(slot(x, 'performance'))
    else {
      if(all(unlist(lapply(args, is, "FLmses"))))
        return(rbindlist(c(list(performance(x)), lapply(args, performance))))
      else
        # CALL performance(list)
        callNextMethod()
    }
  } 
)

setMethod('performance<-', signature(x='FLmses', value="data.frame"),
  function(x, value){
    slot(x, 'performance') <- data.table(value)
    return(x)
})

# }}}
