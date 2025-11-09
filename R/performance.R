# performance.R - DESC
# mse/R/performance.R

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
  function(x, statistics=mse::statistics[c("C", "F", "SB", "AAVC")],
    refpts=FLPar(), years=setNames(nm=dimnames(x[[1]])$year[-1]),
    probs=c(0.1, 0.25, 0.50, 0.75, 0.90),
    om=NULL, type=NULL, run=NULL, mp=paste(c(om, type, run), collapse="_"), ...) {

    # CHECK statistics are all valid

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
            lapply(lapply(as(refpts, 'list'), function(x) x[!is.na(x)]),
              rep, each=length(i)), list(...))), drop=FALSE)

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
  function(x, metrics=FLCore::metrics(x), ...) {

      # CREATE or PASS FLQuants
      if(is(metrics, "FLQuants"))
        flqs <- metrics
      else if(is.list(metrics) & is.function(metrics[[1]]))
        flqs <- do.call(FLCore::metrics, list(object=x, metrics))
      else if(is.function(metrics))
        flqs <- do.call(metrics, list(x))
      else
        stop("metrics could not be computed")

    return(performance(flqs, ...)[])
  }
)
# }}}

# performance(FLStocks) {{{

#' @rdname performance
#' @examples
#' perf <- performance(FLStocks(B=run, A=run), statistics, 
#'   refpts=FLPar(MSY=110000), metrics=list(C=catch), years=list(2012:2015))

setMethod("performance", signature(x="FLStocks"),
  function(x, statistics, refpts=FLPar(),
    years=seq(dims(x[[1]])$minyear + 1, dims(x[[1]])$maxyear),
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
  })
# }}}

# performance(list) FLmse / FLQuants {{{

#' @rdname performance

setMethod("performance", signature(x="list"),
  function(x, statistics, refpts=FLPar(),
    years=seq(dims(x[[1]])$minyear + 1, dims(x[[1]])$maxyear),
    probs=NULL, grid="missing", mc.cores=1, ...) {

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
        refpts=refpts(i), statistics=statistics, years=years, probs=probs), 
        run=j, list(...))), i=x, j=names(x)))

      # SET mp if possible
      if(!"mp" %in% colnames(res) & all(c("type", "run") %in% colnames(res)))
        res[, mp:=paste(om, type, run, sep="_")]

      return(res[])
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
  function(x, refpts=x@refpts, statistics=mse::statistics[c('C', 'F', 'SB')],
    metrics=NULL, om=name(x), ...) {

    # BUG: metrics argument hides metrics method
    if(is.null(metrics))
      metrics <- metrics(x)

    return(performance(stock(x), refpts=refpts, metrics=metrics, 
      statistics=statistics, om=om, ...))
  }
)
# }}}

# performance(FLombf) {{{
# TODO: DEFAULT statistics, mse::statistics[c('C', 'F', 'SB')]
setMethod("performance", signature(x="FLombf"),
  function(x, statistics, refpts=x@refpts,
    metrics=NULL, years=as.character(seq(dims(x)$minyear + 1, dims(x)$maxyear)),
    probs=NULL, om=name(x), ...) {

    # COMPUTE metrics
    if(is.null(metrics))
      mets <- do.call('metrics', list(object=x))
    else
      mets <- do.call('metrics', list(object=x, metrics=metrics))

    # CALL performance by biol
    res <- mapply(function(me, rp) {
      performance(x=me, statistics=statistics, refpts=rp, years=years,
        probs=probs, ...)
      }, me=mets, rp=x@refpts, SIMPLIFY=FALSE)

    res <- rbindlist(res, idcol="biol")

    # ADD om and drop mp
    res[, om:=om]
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
  function(x, om=name(x@om), ...) {

    args <- list(...)

    res <- attr(x, 'performance')

    # IF no extra args, return 'performance' attribute ...
    if(length(args) == 0 & !is.null(res)) {

      return(res)

    } else {

      # GET hcr args
      control_args <- Filter(is.numeric, args(control(x)$hcr))

      # GET tracking elements as FLQuants
      tracks <- divide(tracking(x), dim=1)

      # COMPUTE on x@om
      res <- do.call(performance, c(list(x=x@om), args, control_args,
        tracks, om=name(x@om)))

      # NAME mp if possible
      if(!"mp" %in% colnames(res) & all(c("type", "run") %in% colnames(res)))
        res[, mp:=paste(om, type, run, sep="_")]

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
    slot(x, 'performance') <- value
    return(x)
})

# }}}

# --- performance DB

# writePerformance {{{

#' Write performance table to file
#'
#' @param dat data.table with performance statistics
#' @param file file name to write to, defaults to 'model/performance.dat.gz'
#' @return file name, invisibly
#' @author Iago Mosqueira, WMR
#' @keywords utilities

writePerformance <- function(dat, file="model/performance.dat.gz", overwrite=FALSE) {

  # SET correct column types
  dat[, (colnames(dat)) := lapply(.SD, as.character), .SDcols = colnames(dat)]
  dat[, (c("year", "data")) := lapply(.SD, as.numeric), .SDcols = c("year", "data")]

  # ADD empty type and run if missing
  if(all(!c("type", "run") %in% names(dat)))
    dat[, `:=`(type=character(1), run=character(1), mp=character(1)), ] 
  # SET mp from type and run
  else if (is.null(dat[["mp"]]) & all(c("type", "run") %in% names(dat))) 
    dat[, mp := paste(om, type, run, sep="_")]

  # SET label
  dat[, label := ifelse(mp == character(1), om, mp)]

  # SET column order
  setcolorder(dat, neworder=c('om', 'type', 'run', 'mp', 'biol', 'statistic',
    'name', 'desc', 'year', 'iter', 'data'))

  # CREATE
  if(!file.exists(file) | overwrite) {

    fwrite(dat, file=file)

    invisible(file)

  # ADD by substituting
  } else {

    # CHECK dat exists in file
    db <- readPerformance(file)

    # RUN anti-join on biol, statistic, year, iter, om, type & run
    db <- db[!dat, on=.(biol, statistic, year, iter, om, type, run)]

    # ADD new rows
    db <- rbind(db, dat)

    # WRITE to file
    fwrite(db, file=file)

    invisible(file)
  }
}
# }}}

# readPerformance {{{

readPerformance <- function(file="model/performance.dat.gz") {

  # READ file
  dat <- fread(file, colClasses=c(type='character', run='character',
    mp='character', biol='character', year='numeric', iter='character',
    data='numeric'))

  # SET key
  setkey(dat, om, type, run, biol, mp, statistic, year)

  # SET column order
  setcolorder(dat, neworder=c('om', 'type', 'run', 'mp', 'biol', 'statistic',
    'name', 'desc', 'year', 'iter', 'data'))

  # SET as factor
  #asfactor <- c("om", "type", "run", "biol", "statistic", "mp")
  #dat[, c(asfactor) := lapply(.SD, as.factor), .SDcols=asfactor]

  # RETURN
  return(dat)
}

# }}}

# summaryPerformance {{{

summaryPerformance <- function(file="model/performance.dat.gz") {

  #
  if(!is(file, "data.table"))
    file <- readPerformance(file)

  # TABLE by mp: years, (frq), iter, BY om, type
  res <- file[, .(
    # year range
    years=paste(min(year), max(year), sep="-"),
    # frequency
    frq=c(dist(sort(unique(as.numeric(year)))[1:2])),
    # no. iters
    iter=length(unique(iter))
    # DO by om, type & run
    ), by=.(om, type, run)]

  setorder(res, om, type, run)

  # GET summary row values
  summ <- file[, lapply(.SD, function(x) length(unique(x))),
    .SDcols = c("om", "type", "mp")] 

  # PRINT it
  cat(do.call(sprintf, c(list(fmt="- oms: %i, types: %i, mps: %i\n"), unlist(summ))))

  # PRINT tree or summary table
  # print(as.data.frame(res))

  invisible(TRUE)
}

# }}}

# labelPerformance {{{

#' @title labelPerformance
#' @description Creates a label column in a performance table
#' @param dat A performance statistics table, as returned by performance()
#' @param labels Labels to be inserted, as vector or data.frame/data.table
#' @return A labelled performance statistics data.table
#' @details
#' - 'numeric'
#' - vector
#' - data.frame or data.table
#' - NULL
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname labelPerformance
#' @alias setLabelPerformance

labelPerformance <- function(dat, labels=NULL) {

  # TODO: EXCLUDE mp=character(1)

  # NO label, use mp
  if(is.null(labels)) {
    labels <- data.table(mp=sort(unique(dat[mp != character(1), mp])),
      label=sort(unique(dat[mp != character(1), mp])))

  # 'numeric', set as sequence in unique order
  } else if(identical(labels, "numeric")) {
    labels <- data.table(mp=unique(dat[mp != character(1), mp]), 
      label=paste0("MP", seq(unique(dat[mp != character(1), mp]))))
  
  # VECTOR, assign names by unique()
  } else if(is.vector(labels)) {
    labels <- data.table(mp=dat[mp != character(1), unique(mp)], label=labels)
  
  # SET as data.table JIC
  } else {
    labels <- data.table(labels)
  }

  # CHECK mp matches for non-empty
  if(!all(unique(dat[mp != character(1), mp]) %in% unique(labels[, mp]))) {
    stop("'mp' names in both tables do not match")
  }

  # ADD om if missing mp
  dat[mp == character(1), label:=om]

  # MERGE by mp on rows with mp
  # dat <- merge(dat[, !"label"], labels[, .(mp, label)], by="mp")
  omdat <- dat[mp == character(1), ]
  mpdat <- merge(dat[mp != character(1), !"label"], labels[, .(mp, label)], by="mp")
  dat <- rbind(omdat, mpdat)

  # SET as factor, OM labels (no mp) first
  levs <- c(dat[mp == character(1),
    unique(label)], sort(dat[mp != character(1), unique(label)]))
 
  dat[, label := factor(label, levels=levs)]

  # END
  return(dat[])
}
# }}}

# setLabelPerformance {{{

#' @title setLabelPerofrmance
#' @description FUNCTION_DESCRIPTION
#' @param file File where performance table is stored, default: 'model/performance.dat.gz'
#' @return Invisible updates the table in file
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname labelPerformance

setLabelPerformance <- function(file="model/performance.dat.gz", labels) {

  dat <- readPerformance(file)

  dat <- labelPerformance(dat, labels)

  writePerformance(dat, file=file, overwrite=TRUE)
}
# }}}

# periodsPerformance {{{

periodsPerformance <- function(x, periods) {

  # COERCE to list
  periods <- as.list(periods)
 
  years <- unlist(lapply(periods, function(x) {
    if(length(x) > 1)
      paste(x[1], substr(rev(x)[1], 3, 4), sep="-")
    else
      x
  }))

  # ASSIGN names if missing
  if(is.null(names(periods))) {
    names(periods) <- years
  }

  res <- rbindlist(Map(function(pe, na, ye) {
    x[year %in% pe, .(data=mean(data, na.rm=TRUE), period=na, year=ye),
    by=.(type, mp, statistic, name, desc, iter)]},
    pe=periods, na=names(periods), ye=years))

  return(res)
}
# }}}
