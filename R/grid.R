# grid.R - DESC
# /grid.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' Functions to create grids and list for model runs
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
#' @param scenarios Values and levels for each run variable, *list*.
#' @param names Should the table output contain names (`FALSE`) or indices (`TRUE`, default).
#' @param grid Index table with grid to be converted into input list.
#' @param ... Extra arguments, see *Details*.
#'
#' @return A `data.frame` or `list`.
#'
#' @name grid
#' @rdname grid
#' @aliases expandGrid
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#'
#' scenarios  <- list(steepness=c(0.7, 0.8), M=list(0.2, 0.4, lo=seq(0.2, 0.4, length=10)))
#' # Create full grid as index table
#' expandGrid(scenarios)
#' # Drop certain combinations
#' expandGrid(scenarios, M == 0.2 & steepness == 0.7, M == "lo" & steepness == 0.8)
#' # Output as names
#' expandGrid(scenarios, M == 0.2 & steepness == 0.7, names=TRUE)

# expandGrid {{{
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
