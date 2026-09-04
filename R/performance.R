# performance.R - DESC
# mse/R/performance.R

# Copyright European Union, 2016-17
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

globalVariables(c(".", "data", "mp", "om", "run", "statistic", "age", "unit",
  "season", "area", "i.type", "..type"))

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

# Standardize performance data.table column order and types.
# Missing standard columns are added while non-standard columns are preserved.
# @param x A data.table to standardize
# @param add_missing Logical, if TRUE adds missing standard columns with NA
# @return The standardized data.table (modified by reference)

.standardizeDT <- function(x, add_missing = TRUE) {

  # RETURN early if no rows
  if(nrow(x) == 0)
    return(invisible(x))

  # DEFINE standard columns and their types
  standard_cols <- c('om', 'biol', 'mp', 'year', 'statistic', 'name', 'iter',
    'data', 'type', 'run', 'desc')
  standard_types <- list(
    om = "character",
    biol = "character",
    mp = "character",
    year = "integer",
    statistic = "character",
    name = "character",
    iter = "integer",
    data = "numeric",
    type = "character",
    run = "character",
    desc = "character"
  )

  # ADD missing standard columns if requested
  if(add_missing) {
    missing_cols <- setdiff(standard_cols, colnames(x))
    if(length(missing_cols) > 0) {
      for(col in missing_cols) {
        type <- standard_types[[col]]
        if(type == "integer")
          set(x, j = col, value = NA_integer_)
        else if(type == "numeric")
          set(x, j = col, value = NA_real_)
        else
          set(x, j = col, value = NA_character_)
      }
    }
  }

  # CONVERT types for present standard columns
  for(col in intersect(names(standard_types), colnames(x))) {
    target_type <- standard_types[[col]]

    if(target_type == "integer" && !is.integer(x[[col]])) {
      set(x, j = col, value = as.integer(x[[col]]))
    } else if(target_type == "numeric" && !is.numeric(x[[col]])) {
      set(x, j = col, value = as.numeric(x[[col]]))
    } else if(target_type == "character" && !is.character(x[[col]])) {
      # CONVERT factors to character
      if(is.factor(x[[col]]))
        set(x, j = col, value = as.character(x[[col]]))
    }
  }

  # FACTOR character columns (except standard ones that should stay character)
  factor_exclude <- c('om', 'biol', 'mp', 'type', 'run', 'statistic', 'name',
    'desc')
  chr_cols <- names(x)[sapply(x, is.character) & !names(x) %in% factor_exclude]
  if(length(chr_cols) > 0)
    x[, (chr_cols) := lapply(.SD, as.factor), .SDcols=chr_cols]

  # SET column order: standard columns first, then extra columns
  present_standard <- intersect(standard_cols, colnames(x))
  extra_cols <- setdiff(colnames(x), standard_cols)
  setcolorder(x, c(present_standard, extra_cols))

  return(invisible(x))
} 

# .merge, twist y and merge to x

.merge <- function(x, y) {
  if(length(y) == 0)
    return(x)
  if(!is.list(x))
    x <- c(x, y)
  else {
    y <- setNames(lapply(names(x), function(i)
      lapply(y, `[[`, i)), names(x))
    x <- Map(function(x, y) FLQuants(c(x,y)), x, y)
  }
  return(x)
}

# Extract a shared FLPar or the element matching a container member name.
.refptsFor <- function(refpts, name) {
  if(is(refpts, "FLPar"))
    return(refpts)

  if(!is(refpts, "list") || is.null(names(refpts)) || !name %in% names(refpts))
    stop("'refpts' must be an FLPar or a named list with an entry for each input.")

  return(refpts[[name]])
}

# .compactDT

.compactDT <- function(x) {

  # RETURN early if no rows
  if(nrow(x) == 0)
    return(invisible(x))

  # FACTOR character columns
  chr_cols <- names(x)[sapply(x, is.character)]
  if(length(chr_cols) > 0)
    x[, (chr_cols) := lapply(.SD, as.factor), .SDcols=chr_cols]
  
  # INTEGER year and iter
  if("year" %in% names(x))
    x[, year := as.integer(year)]
  if("iter" %in% names(x))
    x[, iter := as.integer(iter)]

  return(invisible(x))
}

# .validDT
.validDT <- function(x) {

  if(!is.data.table(x))
    stop("input must be a data.table")
  
  if(!all(c("statistic", "year", "data") %in% colnames(x)))
    stop("data.table must contain columns: statistic, year, data")
  
  return(TRUE)
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
#'   Defaults to the reference points held by inputs that contain them. For
#'   `FLmses` and lists, a supplied `FLPar` applies to every member, while a
#'   named list supplies one object per member. `FLQuants` defaults to an empty
#'   `FLPar()`.
#' @param metrics Optional metrics object for FLStock/FLStocks input. Can be:
#'   - An `FLQuants` object with pre-computed metrics
#'   - A list of metric functions
#'   - A single function to compute metrics
#' @param years Years on which statistics should be computed. Can be:
#'   - A vector of years to use
#'   - A named list of year vectors (names become year labels in output)
#'   Defaults to last year of input if omitted.
#' @param om Optional name for the operating model.
#' @param type Optional name for the MP type.
#' @param run Optional name for the model run.
#' @param mp Optional combined MP name. Auto-generated if not provided.
#' @param control Logical. For FLmse input, include HCR control arguments in output?
#'   Defaults to FALSE.
#' @param ... Additional arguments passed through (e.g., custom metrics, tracking data).
#'
#' @return A `data.table` containing computed performance statistics with columns:
#'   - statistic: Name of the computed statistic
#'   - year: Year or period for which statistic was computed
#'   - name: Display name of statistic
#'   - desc: Description of statistic
#'   - iter: Iteration number (or 1 if statistic computes a probability)
#'   - data: The computed value
#'   - om, type, run, mp: Identifiers for the analysis
#'
#' @seealso
#' [statistics], [refpts()], [metrics()]
#'
#' @author Iago Mosqueira (WMR)
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
    om=character(0), type=character(0), run=character(0),
    mp=paste(c(om, type, run), collapse="_"), ...) {

    # GET extra args
    dots <- list(...)

    # GET names in refpts and metrics, plus FLQuant dimnames
    valid.names <- c(dimnames(refpts)$params, names(x),
      c("age", "year", "unit", "season", "area"), names(dots))

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

    # ADD name as desc if missing, separate expr and flag change and computability
    statistics <- lapply(statistics, function(x) {
      if(is.null(x$desc)) x$desc <- x$name

      x$expr <- x[names(x) == ""][[1]][[2]]
      x$vars <- all.vars(x$expr)
      x$is_change <- grepl("change|variability|difference", x$desc)
      x$computable <- all(sapply(x$vars, function(v) exists(v) | v %in% valid.names))
      
      return(x)
    })

    # COERCE refpts to list
    refpts_lst <- lapply(as(refpts, 'list'), identity)

    # MAP over years
    res <- data.table::rbindlist(Map(function(i, ni) {

      if(any(unlist(lapply(statistics, '[[', 'is_change'))) & length(i) == 1) {
        i_use <- seq(an(i) - 1, an(i))
        x_i <- lapply(x, '[', j=ac(i_use))
        inp_change <- c(x_i, lapply(refpts_lst, rep, each=length(i_use)), dots)
        x_i <- lapply(x, '[', j=ac(i))
        inp_base <- c(x_i, lapply(refpts_lst, rep, each=length(i)), dots)
      } else {
        x_i <- lapply(x, '[', j=ac(i))
        inp_base <- c(x_i, lapply(refpts_lst, rep, each=length(i)), dots)
        inp_change <- inp_base  # never used but avoids unbound variable
      }

      data.table::rbindlist(lapply(statistics, function(j) {
        
        inp <- if(j$is_change & length(i) == 1) inp_change else inp_base

        if(j$computable) {
          
          res <- as.data.table(as.data.frame(eval(j$expr, inp), drop=FALSE))
            setkeyv(res, c("age", "unit", "season", "area", "iter"))

          res <- res[, .(data=mean(data), year=ni), by=.(age, unit, season, area, iter)]

          return(res)

        } else {
          warning(paste0("statistic '", j$name, "' could not be computed, check metrics or tracking."))
          return(NULL)
        }
      }), idcol="statistic", fill=TRUE)[, c("statistic", "data", "iter")]
    }, i=years, ni=names(years)), idcol="year")

    # SET year as numeric, TODO:combine with periods
    if(!any(is.na(suppressWarnings(as.numeric(names(years))))))
      res[, year := as.numeric(year)]

    # ADD statistic name(s) & description(s)
    names <- sapply(statistics, '[[', 'name')
    descs <- sapply(statistics, '[[', 'desc')
    res[, name := names[statistic]]
    res[, desc := descs[statistic]]

    # CREATE and ASSIGN cols
    set(res, j=c('om', 'type', 'run', 'mp'), value=list(om, type, run, mp))

    # STANDARDIZE columns
    .standardizeDT(res)

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

    # COMPUTE on metrics
    res <- performance(metrics, refpts=refpts, statistics=statistics, om=om, ...)
    res[, biol := name(stock(x))]

    # STANDARDIZE columns
    .standardizeDT(res)

    return(res[])
  }
)
# }}}

# performance(FLombf) {{{

#' @rdname performance

setMethod("performance", signature(x="FLombf"),
  function(x, refpts=FLCore::refpts(x),
    statistics=mse::statistics[c('C', 'F', 'HR', 'SB')],
    metrics=NULL, om=name(x), ...) {

    # COMPUTE metrics, argument hides metrics method
    if(is.null(metrics))
      metrics <- metrics(x)
    else
      metrics <- do.call("metrics", c(list(object=x), metrics))

    # SET NULL name if missing
    if(length(om) == 0L)
      om <- NULL

    # COMPUTE
    res <- rbindlist(lapply(setNames(nm=names(metrics)), function(i) {
      res <- performance(metrics[[i]], refpts=.refptsFor(refpts, i),
        statistics=statistics, om=om, ...)
      res[, biol := NULL]
      res
    }), idcol="biol")

    # STANDARDIZE columns
    .standardizeDT(res)
 
    return(res)
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
  function(x, refpts=FLCore::refpts(x),
    statistics=.validStatistics(om(x)), metrics=NULL,
    years=dimnames(om(x))$year[-1], om=name(x@om), type="MP", run="1",
    control=FALSE, ...) {

    # GET arguments (extra metrics)
    dots <- list(...)

    # SEPARATE functions and values
    fid <- sapply(dots, is, 'function')

    # IF no extra args, return 'performance' attribute ...
    res <- attr(x, 'performance')
    
    if(missing(statistics) & missing(refpts) & !is.null(res)) {
      if(is(om(x), "FLom"))
        res[, biol := name(stock(om(x)))]
      return(res[])

    } else {

      # GET hcr args
      hcrargs <- Filter(is.numeric, args(control(x)$hcr))

      # GET tracking elements as FLQuants
      if(length(tracking(x)) > 0) {
        tracks <- copy(tracking(x))
        setnames(tracks, 1:2, c('quant', 'qname'))
        tracks <- as(tracks, 'FLQuants')
        tracks <- lapply(tracks, setquant, "age")
      } else {
        tracks <- NULL
      }

      # GET refpts
      rps <- refpts
      if(is(rps, "FLPar")) {
         vars_rps <- dimnames(rps)$params
      } else {
         vars_rps <- unique(unlist(lapply(rps, function(i) dimnames(i)$params)))
      }

      # GET standard metrics
      mets <- metrics(x, metrics=metrics)

      # ADD non-function args
      mets <- .merge(mets, dots[!fid])

      # GET current metrics names
      nms_mets <- if(is(mets, "FLQuants")) names(mets) else names(mets[[1]])

      # IDENTIFY required formula elements
      needed <- unique(unlist(lapply(statistics, function(s)
        all.vars(s[[1]][[2]]))))

      # LIST those in tracks, hcrargs, refpts and metrics
      known <- c(names(tracks), names(hcrargs), vars_rps, nms_mets)

      # IDENTIFY extra needed calls, ignores builtins()
      extra <- sapply(needed[!needed %in% c(known, builtins())], exists)

      # COMPUTE extra and add to mets
      if(any(extra)) {
        extras <- lapply(setNames(nm=names(extra)[extra]), function(i)
          do.call(i, list(om(x))))

        # ADD extras
        mets <- .merge(mets, extras)
      }

      # CALL performance(metrics)
      res <- do.call(performance, c(list(x=mets, statistics=statistics,
        refpts=rps), hcrargs, tracks, list(years=years,
        om=unname(name(x@om)), type=type, run=run)))

      # NAME mp if  possible
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

      if(is(om(x), "FLom"))
        res[, biol := name(stock(om(x)))]

      # STANDARDIZE columns
      .standardizeDT(res)

      return(res[])
    }
  }
)

setReplaceMethod('performance', signature(x='FLmse', value="data.frame"),
  function(x, value){
    attr(x, "performance") <- .standardizeDT(data.table(value))
    return(x)
})

# }}}

# performance(FLmses) {{{

#' @rdname performance

setMethod("performance", signature(x="FLmses"),
  function(x, type=NULL, refpts=NULL, ...) {

    args <- list(...)
    
    # RETURN empty data.table if x is empty
    if(length(x) == 0) {
      return(data.table())
    }

    # RETURN performance slot if exists AND statistics not provided
    if(!"statistics" %in% names(args) & is.null(refpts) &
      nrow(slot(x, 'performance')) > 0) {
      return(slot(x, 'performance')[])
    # COMPUTE
    } else {
      # SET statistics if missing
      if(!"statistics" %in% names(args)) {
        args$statistics <- .validStatistics(om(x[[1]]))
      }

      # SET years if missing
      if(!"years" %in% names(args)) {
        args$years <- dimnames(om(x[[1]]))$year[-1]
      }

      # LAPPLY over args if FLmses
      if(all(unlist(lapply(args, is, "FLmses")))) {
        return(rbindlist(c(list(performance(x)), lapply(args, performance))))

      # CALL on each FLmse in FLmses
      } else {
        res <- rbindlist(Map(function(i, j){
          call <- c(list(x=i, run=j, om=name(om(i))), args)
          if(!is.null(refpts))
            call$refpts <- .refptsFor(refpts, j)
          do.call(performance, call)
        }, i=x, j=names(x)), fill=TRUE)

        # ADD type
        if(!is.null(type)) {
          if(is(type, "data.table")) {
            col <- colnames(type)[colnames(type) != "type"][1]
            res[type, type := i.type, on = col]
          } else if(is(type, "character")) {
            res[, type := ..type]
          }
          res[, mp:=paste(om, type, run, sep="_")]
        }

        # SET mp if possible
        if(!"mp" %in% colnames(res) & all(c("type", "run") %in% colnames(res)))
          res[, mp:=paste(om, type, run, sep="_")]

        return(res[])
      }
    }
  }
)

# performance<-(FLmse, data.table)

setReplaceMethod('performance', signature(x='FLmses', value="data.frame"),
  function(x, value){
    slot(x, 'performance') <- .standardizeDT(data.table(value))
    return(x)
})

# }}}

# performance(list) FLmse / FLmses / FLQuants {{{

#' @rdname performance

setMethod("performance", signature(x="list"),
  function(x, statistics, refpts=NULL,
    years="missing", ...) {

    # - list(FLmses)
    if(all(unlist(lapply(x, is, 'FLmses')))) {

      if(missing(statistics)) {
        return(rbindlist(Map(function(i, j) {
          if(is.null(refpts))
            performance(i)
          else
            performance(i, refpts=.refptsFor(refpts, j))
        }, i=x, j=names(x))))
      
      } else {
        # COMPUTE
        if(missing(years))
          return(rbindlist(Map(function(i, j) {
            if(is.null(refpts))
              performance(i, statistics=statistics, ...)
            else
              performance(i, statistics=statistics,
                refpts=.refptsFor(refpts, j), ...)
          }, i=x, j=names(x))))
        else
          return(rbindlist(Map(function(i, j) {
            if(is.null(refpts))
              performance(i, statistics=statistics, years=years, ...)
            else
              performance(i, statistics=statistics, years=years,
                refpts=.refptsFor(refpts, j), ...)
          }, i=x, j=names(x))))
      }
    }

    # - list(FLQuants), handle early before checking years
    if(all(unlist(lapply(x, is, 'FLQuants')))) {
      
      # SET years default from first element if missing
      if(missing(years))
        years <- seq(dims(x[[1]][[1]])$minyear + 1, dims(x[[1]][[1]])$maxyear)
      
      # SET list of refpts
      if(is.null(refpts))
        refpts <- FLPar()
      if(!is(refpts, "list")) {
        refpts <- lapply(setNames(nm=names(x)), function(x) refpts)
      }

      # CALL performance(FLQuants)
      res <- rbindlist(Map(function(x, y) {
        res <- performance(x, statistics=statistics, refpts=y, ...)
        res[, biol := NULL]
        res
      }, x=x, y=refpts), idcol='biol')

      .standardizeDT(res)
      return(res[])
    }
 
    # SET years default from first element if missing (for non-FLQuants)
    if(missing(years))
      years <- seq(dims(x[[1]])$minyear + 1, dims(x[[1]])$maxyear)

    # - list(mse), return existing table if no statistics, otherwise compute
    if(all(unlist(lapply(x, is, 'FLmse')))) {
      flmses <- FLmses(x)
      if(is.null(refpts))
        return(performance(flmses, statistics=statistics, years=years, ...)[])
      return(performance(flmses, statistics=statistics, years=years,
        refpts=refpts, ...)[])
    }

    # - list(FLom | FLombf), compute performance
    if(all(unlist(lapply(x, is, 'FLo')))) {
      if(missing(statistics))
        statistics <- mse::statistics[c('C', 'F', 'SB')]

      res <- rbindlist(Map(function(i, j) {
        call <- c(list(x=i, statistics=statistics, years=years, run=j),
          list(...))
        if(!is.null(refpts))
          call$refpts <- .refptsFor(refpts, j)
        do.call(performance, call)
      }, i=x, j=names(x)), fill=TRUE)

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
      # Recurse with coerced list
      if(missing(statistics))
        return(performance(x, refpts=refpts, years=years, ...))
      else
        return(performance(x, statistics=statistics, refpts=refpts,
          years=years, ...))
    }

    # Should not reach here
    stop("input list must contain objects of supported classes (FLmses, FLmse, FLo, or FLQuants)")
  }
) 
# }}}

# performance(FLStock) {{{

#' @rdname performance

setMethod("performance", signature(x="FLStock"),
  function(x, statistics, metrics=list(R=rec, SB=ssb, B=tsb, C=catch, L=landings,
    D=discards, F=fbar, HR=hr), ...) {

    flqs <- metrics(x, metrics=metrics)

    # COMPUTE
    res <- performance(flqs, statistics=statistics, ...)

    # STANDARDIZE columns
    .standardizeDT(res)

    return(res[])
  }
)
# }}}

# performance(FLStocks) {{{

#' @rdname performance

setMethod("performance", signature(x="FLStocks"),
  function(x, statistics, ...) {

    res <- rbindlist(lapply(x, function(i) {
      res <- performance(i, statistics=statistics, ...)
      res[, biol := NULL]
      res
    }), idcol='biol')

    .standardizeDT(res)
    return(res[]) 
  })
# }}}
