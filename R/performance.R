# performance.R - DESC
# mse/R/performance.R

# Copyright European Union, 2016-17
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.
 
globalVariables(c(".", "data", "mp", "om", "run", "statistic", "age", "unit", "season"))

# .functions {{{

# .validStatistics: returns statistics valid for the input object
.validStatistics <- function(x, statistics=mse::statistics) {

  # FIND needed elements
  needs <- lapply(statistics, function(x) {
    vars <- all.vars(x[[1]][[2]])
    vars[!unlist(lapply(vars, exists))]
  })

  # EXTRACT refpts in object
  if(is(x, 'FLom'))
    rdnms <- dimnames(refpts(x))$params
  else
    rdnms <- dimnames(refpts(x)[[1]])$params

  # GET names in refpts and metrics, plus FLQuant dimnames
  available <- c(rdnms, c("C", "SB", "B", "F", "HR"),
    c("age", "year", "unit", "season", "area"))

  # WHAT statistics can be computed (needs match available)?
  valid <- unlist(lapply(needs, function(x) {
    all(x %in% available)
  }))

  return(statistics[valid])
}

# }}}

# performance {{{

#' Compute Performance Statistics for Management Procedure Evaluation
#'
#' Evaluates the performance of a management procedure by computing statistical 
#' metrics across simulated projections. Supports multiple input types (FLQuants, 
#' FLStock, FLStocks, FLom, FLmse, FLmses, and lists) and computes custom statistics
#' defined by formulas that reference metrics and reference points.
#'
#' Each statistic is defined as a named list containing:
#' - A formula (unnamed element) using metric and reference point names, 
#'   e.g., `~yearMeans(SB/SB0)`
#' - name: Short name for tables/plots, e.g., "SB/SB[0]"
#' - desc: Longer description, e.g., "Mean spawner biomass relative to unfished"
#'
#' Statistics formulas can reference:
#' \enumerate{
#'   \item Names of `FLQuants` elements (metrics from estimation)
#'   \item Parameter names in the `refpts` object
#'   \item FLQuant dimension names (age, year, unit, season, area)
#'   \item Functions callable on the source object (for non-FLQuants input)
#' }
#'
#' @param x An object holding simulation results. Supported classes:
#'   `FLQuants`, `FLStock`, `FLStocks`, `FLom`, `FLmse`, `FLmses`, or `list`.
#' @param statistics A list of statistics to compute. Each element must be a named list
#'   with a formula and metadata (name, desc). See Details.
#' @param refpts Reference points for calculations, typically an `FLPar` object.
#'   Defaults to `FLPar()` (empty).
#' @param metrics Optional metrics object for FLStock/FLStocks input. Can be:
#'   - An `FLQuants` object with pre-computed metrics
#'   - A list of metric functions
#'   - A single function to compute metrics
#' @param years Years on which statistics should be computed. Can be:
#'   - A vector of years to use
#'   - A named list of year vectors (names become year labels in output)
#'   Defaults to last year of input if omitted.
#' @param probs Optional numeric vector of quantiles (0-1) to compute on statistic
#'   distributions across iterations. If NULL (default), returns mean values.
#' @param om Optional name for the operating model.
#' @param type Optional name for the MP type.
#' @param run Optional name for the model run.
#' @param mp Optional combined MP name. Auto-generated if not provided.
#' @param control Logical. For FLmse input, include HCR control arguments in output?
#'   Defaults to FALSE.
#' @param mc.cores Integer. Number of cores for parallel processing when handling
#'   lists or FLStocks. Defaults to 1 (sequential).
#' @param ... Additional arguments passed through (e.g., custom metrics, tracking data).
#'
#' @return A `data.table` containing computed performance statistics with columns:
#'   - statistic: Name of the computed statistic
#'   - year: Year or period for which statistic was computed
#'   - name: Display name of statistic
#'   - desc: Description of statistic
#'   - iter: Iteration number (or median/quantile if probs specified)
#'   - data: The computed value
#'   - om, type, run, mp: Identifiers for the analysis
#'
#' @seealso
#' [statistics], [refpts()], [metrics()]
#'
#' @author
#' Iago Mosqueira (WMR)
#'
#' @keywords utilities
#' @name performance
#' @rdname performance
NULL

# }}}

# performance(FLQuants) {{{

#' @rdname performance
#' @examples
#' # LOAD example FLmse object
#' data(plesim)
#' # Extract FLQuants using metrics
#' x <- metrics(om)
#' performance(x, statistics=statistics[c("SB", "SBMSY", "F", "FMSY")],
#'   refpts=refpts(om), om="ple", run="r00", type="test")

setMethod("performance", signature(x="FLQuants"),
  function(x, statistics=mse::statistics[c("C", "F", "SB", "AAVC")],
    refpts=FLPar(), years=setNames(nm=dimnames(x[[1]])$year[-1]),
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
        if(grepl("change|variability|difference", j$desc) & length(i) == 1) {
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
    
    # CREATE empty cols
    set(res, j=c('om', 'type', 'run', 'mp'), value=as.list(rep(character(1), 4)))

    # ASSIGN names (om, type, run, mp)
    set(res, j=c('om', 'type', 'run', 'mp'), value=list(om, type, run, mp))

    return(res[])
  }
)

# }}}

# performance(FLom) {{{

#' @rdname performance
#' @examples
#' # Compute on OM, name taken from slot
#' performance(om, statistics=statistics[c("SB", "SBMSY", "F", "FMSY")],
#'   run="r00", type="test")

setMethod("performance", signature(x="FLo"),
  function(x, refpts=x@refpts, statistics=mse::statistics[c('C', 'F', 'HR', 'SB')],
    metrics=NULL, om=name(x), ...) {

    # COMPUTE metrics
    if(is.null(metrics))
      metrics <- metrics(x)
    else
      metrics <- metrics(x, metrics)

    # SET NULL name if missing
    if(length(om) == 0)
      om <- NULL

    return(performance(metrics, refpts=refpts, statistics=statistics, om=om, ...))
  }
)
# }}}

# performance(FLmse) {{{

#' @rdname performance
#' @examples
#' # Setup an example MSE
#' control <- mpCtrl(list(
#'   est = mseCtrl(method=perfect.sa),
#'   hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.15))))
#' # ... and run it
#' mse <- mp(om, ctrl=control, args=list(iy=2025, fy=2030))
#' # Compute performance using all default statistics, data(statistics)
#' performance(mse, run="r00", type="test")
#' # or select a few of them
#' performance(mse, statistics=statistics[c("SBMSY", "FMSY")], run="r00", type="test")

setMethod("performance", signature(x="FLmse"),
  function(x, statistics=.validStatistics(om(x)), om=name(x@om), control=FALSE,
    type="MP", run="1", ...) {

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
      res <- do.call(performance, c(list(x=x@om, statistics=statistics), args,
        control_args, tracks, om=name(x@om), type=type, run=run))

      # NAME mp if  possible
      # TODO: IF type, run missing
      if(!"mp" %in% colnames(res) & all(c("type", "run") %in% colnames(res)))
        res[, mp:=paste(om, type, run, sep="_")]

      # ADD control$hcr@args
      if(control) {
          
        hcrargs <- args(control(x, 'hcr'))

        # USE only numeric or FLQuant/FLPar with single dims but 'iter',
        # and that 1 | iter(om)
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

#' @rdname performance

setMethod("performance", signature(x="FLmses"),
  function(x, ...) {

    args <- list(...)

    # RETURN slot if no other args
    if(length(args) == 0)
      return(slot(x, 'performance'))
    else {
      
      if(!"statistics" %in% names(args)) {
        args$statistics <- .validStatistics(om(x[[1]]))
      }

      if(!"years" %in% names(args)) {
        args$years <- dimnames(om(x[[1]]))$year[-1]
      }

      if(all(unlist(lapply(args, is, "FLmses")))) {
        return(rbindlist(c(list(performance(x)), lapply(args, performance))))

      } else {

        res <- rbindlist(Map(function(i, j) do.call(performance, c(list(x=i,
          refpts=refpts(i), run=j, om=name(om(i))), args)), i=x, j=names(x)))

        # SET mp if possible
        if(!"mp" %in% colnames(res) & all(c("type", "run") %in% colnames(res)))
          res[, mp:=paste(om, type, run, sep="_")]

        return(res)
      }
    }
  }
)

# performance<-(FLmse, data.table)

setMethod('performance<-', signature(x='FLmses', value="data.frame"),
  function(x, value){
    slot(x, 'performance') <- data.table(value)
    return(x)
})

# }}}

# performance(list) FLmse / FLQuants {{{

#' @rdname performance

setMethod("performance", signature(x="list"),
  function(x, statistics, refpts=FLPar(),
    years=seq(dims(x[[1]])$minyear + 1, dims(x[[1]])$maxyear), ...) {

    # - list(FLom | FLombf)
    if(all(unlist(lapply(x, is, 'FLo')))) {
      if(missing(statistics))
        statistics <- mse::statistics[c('C', 'F', 'SB')]

      # TODO: ADD om if missing, via idcol or :=, DROP Map
      res <- rbindlist(Map(function(i, j) do.call(performance, c(list(x=i,
        refpts=refpts(i), statistics=statistics, years=years, run=j),
        list(...))), i=x, j=names(x)))

      return(res[])
    }
 
    # - list (FLmse/FLom), coerce to FLmse and pass
    if(any(unlist(lapply(x, is, 'FLo')))) {
      x <- lapply(x, function(i) {
        if(is(i, 'FLo'))
          return(FLmse(om=i, tracking=data.table()))
        else
          return(i)
      })
    }

    # - list(mse) | FLmses
    if(all(unlist(lapply(x, is, 'FLmse')))) {
      return(performance(FLmses(x), statistics=statistics, years=years, ...)[])
    }

    # - list(FLmses), assumes performance is stored
    if(all(unlist(lapply(x, is, 'FLmses')))) {
      return(rbindlist(lapply(x, function(i) performance(i))))
    }
         
    # ELSE assume list of FLQuants
    if(!all(unlist(lapply(x, is, 'FLQuants'))))
      stop("input list must contain objects of class FLQuants")
      
      res <- data.table::rbindlist(lapply(x, performance,
        statistics=statistics, refpts=refpts, ...), idcol='run')
    
    return(res[])
  }
) 
# }}}

# performance(FLStock) {{{

#' @rdname performance

setMethod("performance", signature(x="FLStock"),
  function(x, statistics, metrics=list(R=rec, SB=ssb, B=tsb, C=catch, L=landings,
      D=discards, F=fbar, HR=hr), ...) {

      flqs <- metrics(x, metrics=metrics)

    return(performance(flqs, statistics=statistics, ...)[])
  }
)
# }}}

# performance(FLStocks) {{{

#' @rdname performance

setMethod("performance", signature(x="FLStocks"),
  function(x, statistics, ...) {

    res <- rbindlist(lapply(x, performance,
      statistics=statistics, ...), idcol='biol')
    
    return(res[]) 
  })
# }}}
