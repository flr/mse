# mc.R - DESC
# mse/R//mc.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


#' mcN
#'
#' @param z \code{numeric}  Value of Z for a given confidence level for a normally distributed random variable, default is 1.96 for a 95% CI
#' @param E \code{numeric}  The required percentage error of the mean
#'
#' E. Bukaci et al.Int. Journal of Engineering Research and Applications www.ijera.com ISSN: 2248-9622, Vol. 6, Issue 6, (Part - 3) June 2016, pp.60-64
#'
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

