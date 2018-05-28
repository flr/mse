# run.R - DESC
# mse/R/run.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

utils::globalVariables(c("i", "run"))

# tune {{{

#' @title tune
#' @rdname tune
#' @md
#' @description
#' Carry out multiple runs of an MP for a given dataset over a grid of values
#' for the MP/HCR paramaters, in order to find the parameter combination(s) that
#' give the best performance over the chosen indicators, a.k.a. *tuning*
#' @details DETAILS
#' @param mp A function executing a projection applying a given MP, see \code{\link{mseBasic}} for an example
#' @param grid A name list of *mp* argument values to loop along
#' @param indicators A list of performance indicators
#' @param refpts The reference points needed to compute the indicators, *FLPar*
#' @param ... Any other arguments to be passed on to *mp*
#' @return A list or aggregatecd FLR object, depending on the output of *mp*
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[mse]{mseBasic}},\code{\link[mse]{performance}}

tune <- function(mp, grid, indicators, refpts, ...) {

  # PARSE args
  args <- list(...)

  # CREATE grid
  df <- do.call(data.table::CJ, grid)

  # CREATE run index
  df <- df[ , run := .GRP, by = key(df)]

  # Progress Bar
  pb <- utils::txtProgressBar(min = 0, max = nrow(df),
      initial = 1, style=3)
  
  # LOOP over grid rows
  out <- foreach(i = seq(nrow(df))) %dopar% {
    
    # CALL mp
    run <- do.call(mp, c(args, list(hcrparams=as(df[i,][, !"run", with=FALSE],
      'FLPar'), tune=TRUE)))
    
    # UPDATE pb
    setTxtProgressBar(pb, i)

    cbind(performance(run, indicators=indicators, refpts=refpts), df[i,])
  }

  close(pb) 

  # JOIN out
  out <- data.table::rbindlist(out)

  # ADD tolower(mp) name
  nmp <- tolower(deparse(substitute(mp)))
  out[, mp:=nmp]
  setcolorder(out, c(length(out), 1:(length(out)-1)))

  setkey(out, mp, run, indicator, name, iter)

  return(out)
} # }}}

# doRuns {{{
doRuns <- function(mp, grid, metrics=missing, ...) {
  
  # PARSE args
  args <- list(...)

  # CREATE grid
  df <- do.call(data.table::CJ, grid)

  # CREATE run index
  df <- df[ , run := .GRP, by = key(df)]
  
  # PRINT message
  message(paste("Running grid with", nrow(df), "combinations."))

  # LOOP over grid rows
  out <- foreach(i = seq(nrow(df)), .errorhandling="remove") %dopar% {

    message(paste0("[", i, "]"))
    
    # CALL mp
    run <- do.call(mp, c(args, list(hcrparams=as(df[i,][, !"run", with=FALSE],
      'FLPar'), tune=TRUE)))

    if(missing(metrics))
      return(run)
    else
      if(metrics == TRUE)
        return(metrics(run))
      else
        return(metrics(run, metrics))
  }

  # TODO HANDLE errors
  if(length(out) != dim(df)[1])
    warning("Some runs errored!")
  
  # NAMES out
  names(out) <- paste0("run", df$run)

  # getPlural
  if(exists(getPlural(out[[1]]), mode="function"))
    out <- do.call(getPlural(out[[1]]), out)

  # ADD grid as attribute
  attr(out, "grid") <- df

  return(out)

} # }}}
