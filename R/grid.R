# grid.R - Functions to create grid of model and simulation runs
# mse/R/grid.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' Functions to create grids and lists of model and simulation runs
#'
#' Grids of combinations of variables (or parameters) can be created from a list
#' of scenarios contaning all possible values for each variables. Such a grid can
#' be used when conditioning an operating model around the structural uncertainty
#' of fixed parameters and submodel choices. And it can also be used to create
#' a range of combinations of management procedure parameters for *tuning* and
#' selection of MPs.
#'
#' The list of *scenarios* used as input should contain one element per variable,
#' be it either a vector or a list of alternative values. The list must be named,
#' and elements of class *list* should themselves be named. Names are added to
#' vector elements by calling *as.character()* on their content. See examples
#' below for guidance.
#'
#' @param scenarios Values and levels for each run variable, *list*.
#' @param names Should the table output contain names (`FALSE`) or indices (`TRUE`, default).
#' @param grid Index table with grid to be converted into input list.
#' @param ... Extra arguments, see *Details*.
#'
#' @return A `data.frame` or `list`.
#'
#' @name grid
#' @rdname grid
#'
#' @author Iago Mosqueira, EC JRC
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#' # Lists of scenarios can contains vectors ...
#' list(steepness=c(0.6, 0.7, 0.8), M=c(0.2, 0.3, 0.4))
#' # lists ...
#' list(M=list(0.2, 0.4, lo=seq(0.2, 0.4, length=10)))
#' # or both
#' scenarios  <- list(steepness=c(0.7, 0.8), M=list(0.2, 0.4, lo=seq(0.2, 0.4, length=10)))

# expandGrid {{{

#' @rdname grid
#' @aliases expandGrid
#' @examples
#' # Create full grid as index table
#' expandGrid(scenarios)
#' # Drop certain combinations
#' expandGrid(scenarios, M == 0.2 & steepness == 0.7, M == "lo" & steepness == 0.8)
#' # Output as names
#' expandGrid(scenarios, M == 0.2 & steepness == 0.7, names=TRUE)

expandGrid <- function(scenarios, ..., names=FALSE) {

  # CREATE index
  idx <- expand.grid(lapply(scenarios, seq), stringsAsFactors=FALSE)

  # CONVERT to list of lists
  scen <- lapply(scenarios[colnames(idx)], as, "list")

  # CREATE names table
  nms <- lapply(scen, function(x) {
    if(is.null(names(x)))
      names(x) <- as(x, "character")
    names(x) <- ifelse(names(x) == character(1), as(x, "character"), names(x))
    return(names(x))
    })
  nmsd <- expand.grid(nms, stringsAsFactors=FALSE)

  if(names)
    res <- nmsd
  else
    res <- idx
  
  # PARSE subset
  drps <- eval(substitute(list(...)), envir=nmsd)

  if(length(drps) > 0) {
    drp <- Reduce('|', drps)
    res <- res[!drp & !is.na(drp),]
    rownames(res) <- seq(1, nrow(res))
  } 

  return(res)
} # }}}

# gridList {{{

#' @rdname grid
#' @aliases gridList
#' @examples
#' # Create list of variable combinations
#' runs <- gridList(scenarios)
#' runs[[1]]
#' length(runs)
#' # Create list but dropping certain combinations
#' runs <- gridList(scenarios, M == 0.2 & steepness == 0.7)
#' runs[[1]]
#' length(runs)

gridList <- function(scenarios, ..., grid=missing) {
  
  if(missing(grid))
    grid <- expandGrid(scenarios, ...)
  
  res <- vector(mode = "list", length = nrow(grid))
  names(res) <- rownames(grid)
  nms <- colnames(grid)

  for(i in seq(nrow(grid))) {
    ele <- lapply(nms, function(x) scenarios[[x]][1])
    names(ele) <- nms
    res[[i]] <- ele
  }

  return(res)
} # }}}


 nameGrid <- function (df, dir, from = 1) {
    df$number <- seq(from = from, length = nrow(df))
    df$id <- paste(df$number, apply(df, 1, function(x) paste0(gsub(" ", 
        "", paste0(names(x), as.character(x))), collapse = "_")), 
        sep = "-")
    return(df)
}

