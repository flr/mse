# run.R - DESC
# mse/R/run.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

globalVariables("i")

# tune {{{
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
    
    # UPDATE pb
    setTxtProgressBar(pb, i)

    # CALL mp
    run <- do.call(mp, c(args, as.list(df[i,][, !"run", with=FALSE])))

    cbind(performance(run, indicators=indicators, refpts=refpts), df[i,])
  }

  close(pb) 

  # JOIN out
  out <- data.table::rbindlist(out)

  # ADD tolower(mp) name
  nmp <- tolower(deparse(substitute(mp)))
  out[,mp:=nmp]
  setcolorder(out, c(length(out), 1:(length(out)-1)))

  setkey(out, mp, run, indicator, name, iter)

  return(out)
} # }}}

# doRuns {{{
doRuns <- function(mp, grid, metrics=list(SB=ssb, B=stock, C=catch, F=fbar, R=rec), ...) {
  
  # PARSE args
  args <- list(...)

  # CREATE grid
  df <- do.call(data.table::CJ, grid)

  # CREATE run index
  df <- df[ , run := .GRP, by = key(df)]

  # LOOP over grid rows
  out <- foreach(i = seq(nrow(df))) %dopar% {

    cat(paste0("[", i, "]"), "\n")

    # CALL mp
    do.call(mp, c(args, as.list(df[i,][, !"run", with=FALSE])))

    # do.call('metrics', c(list(x=run), metrics))
  }

  # NAMES out
  names(out) <- paste0("R", df$run)

  # getPlural
  if(exists(getPlural(out[[1]]), mode="function"))
    out <- do.call(getPlural(out[[1]]), out)

  return(out)

} # }}}
