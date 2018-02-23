# mc.R - DESC
# mse/R//mc.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' Compute number of necessary Monte Carlo runs
#'
#' The `mcN` function implements the simple method of Bukaci et al. (2016) to
#' calculate the number of Monte Carlo (MC) simulations required to obtain
#' results with a given precision level.
#'
#' Bukaci, E. Korini, Th., Periku, E., Allkja, S., Sheperi, P. 2016. Number of iterations needed in Monte Carlo Simulation using reliability analysis for tunnel supports.  Int. J. of Eng. Res. and Apps., 6 (6-3): 60-64. <www.ijera.com/papers/Vol6_issue6/Part%20-%203/J0606036064.pdf>
#'
#' @name mcN
#' @rdname mcN
#'
#' @param z Value of Z for a given confidence level for a normally distributed random variable, default is 1.96 for a 95% CI, *numeric*.
#' @param E The required percentage error of the mean, *numeric*.
#' @param s
#' @param z
#' @param start
#' @param by
#' @examples
#' \dontrun{
#'  data(ple4) 
#'  ssb <- rlnorm(2000, log(ssb(ple4)), 0.5)
#'  itse <- mcN(ssb) 
#'  plot(se~iters, itse, type='l')
#'  }

setMethod("mcN", signature(x="FLQuant"),
  function(x, s, E=5, z=1.96, start=100, by=1) {

    i <- dim(x)[6]
    if(i < start)
      stop(paste0("Object does not have enough 'iter', dim(x)[6] = ", i))
    
    its <- seq(start, i, by=by)

    mciters <- function(x, s, E=5, z=1.96) {  
      ((100 * z * s) / (E * x)) ^ 2
    }

    res <- unlist(lapply(its,
      function(x) mciters(mean(iter(ssb, 1:x)),
        s=var(iter(ssb, 1:x)) ^ 0.5)))

  return(data.frame(iters=its, se=res))
})

