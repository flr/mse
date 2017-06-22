# noise.R - DESC
# mse/R/noise.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' @title Random noise with different frequencies
#' 
#' @description A noise generator
#' 
#' @param n number of iterations
#' @param len an \code{FLQuant}
#' @param sd standard error for simulated series
#' @param b autocorrelation parameter a real number in [0,1] 
#' @param burn gets rid of 1st values i series
#' @param trunc get rid of values > abs(trunc)
#' @param what returns time series for year, cohort or age"
#' @param ... anyl
#' @aliases rnoise rnoise-method rnoise,numeric,FLQuant-method rnoise,numeric,missing-method
#' @aliases rlnoise rlnoise-method rlnoise,numeric,FLQuant-method rlnoise,numeric,missing-method
#' 
#' @export
#' @docType methods
#' @rdname rnoise
#'
#' @importFrom methods is
#'
#' @return A \code{FLQuant} with autocorrelation equal to B.
#' 
#' @references Ranta and Kaitala 2001 Proc. R. Soc.
#' vt = b * vt-1 + s * sqrt(1 - b^2)
#' s is a normally distributed random variable with mean = 0
#' b is the autocorrelation parameter
#' @export
#' 
#' @examples
#' \dontrun{
#' flq=FLQuant(1:100)
#' white <- rnoise(1000,flq,sd=.3,b=0)
#' plot(white)
#' acf(white)
#' 
#' red <- rnoise(1000,flq,sd=.3,b=0.7)
#' plot(red)
#' acf(red)
#' 
#' data(ple4)
#' res=rnoise(1000,flq,sd=.3,b=0)
#' 
#' ggplot()+
#' geom_point(aes(year,age,size= data),
#'             data=subset(as.data.frame(res),data>0))+
#' geom_point(aes(year,age,size=-data),
#'             data=subset(as.data.frame(res),data<=0),colour="red")+
#' scale_size_area(max_size=4, guide="none")+
#' facet_wrap(~iter)
#' 
#' res=rnoise(4,m(ple4),burn=10,b=0.9,cohort=TRUE)
#' ggplot()+
#' geom_point(aes(year,age,size= data),
#'           data=subset(as.data.frame(res),data>0))+
#' geom_point(aes(year,age,size=-data),
#'           data=subset(as.data.frame(res),data<=0),colour="red")+
#' scale_size_area(max_size=4, guide="none")+
#' facet_wrap(~iter)
#' 
#' }

setMethod("rnoise", signature(n='numeric', len="FLQuant"),
  function(n=n, len=len, sd=1, b=0, burn=0, trunc=0, what=c("year","cohort","age"), seed=NA) {
 
    # CHECK and ADJUST len dims
    if(!dim(len)[6] %in% c(1, n))
      stop("len must have 1 or n iters")

    if(dim(len)[6] == 1)
      res <- propagate(len, n)
    else
      res <- len

    # APPLY by dim
    switch(what[1],
      "cohort"={
        object <- as(len, "FLCohort")
        res <- apply(object, c(2:6), function(x)
          t(noiseFn(len=length(x), sd=sd, b=b, burn=burn, trunc=trunc, seed=seed)))
        res <- array(res, unlist(laply(dimnames(object), length)),
          dimnames=dimnames(object))
        res <- as(FLCohort(res), "FLQuant")
      },
      "year" = {
        leng <- prod(dim(len)[-6])
        # MATRIX with n rows and recycled sd and d
        res[] <- apply(matrix(c(sd, b), ncol=2, nrow=n), 1,
          function(x) noiseFn(len=leng, sd=x[1], b=x[2], burn=burn, trunc=trunc, seed=seed))
      },
      "age" = {
        res <- apply(len, c(2:6),
          function(x) noiseFn(len=length(x), sd=sd, b=b, burn=burn, trunc=trunc, seed=seed))
        res <- as.FLQuant(res, dimnames=dimnames(len))
      }
    )
    return(len + res)
  }
)

setMethod("rnoise", signature(n='numeric', len="missing"),
  function(n=n, sd=1, b=0, burn=0, trunc=0, seed=NA) {
    return(noiseFn(len=n, sd=sd, b=b, burn=burn, trunc=trunc, seed=seed))
  }
)

setMethod("rlnoise", signature(n='numeric', len="FLQuant"),
  function(n=n, len=len, sd=1, b=0, burn=0, trunc=0, what=c("year", "cohort", "age"), seed=NA) {
    return(exp(rnoise(n=n, len=len, sd=sd, b=b, burn=burn, trunc=trunc, what=what[1], seed=seed)))
  }
)

# noiseFn {{{
noiseFn <- function(len, sd=1, b=0, burn=0, trunc=0, seed=NA) {

  # set.seed by call if given
  if(!is.na(seed))
    set.seed(seed)

  # CHECK burn >= 0
  if (burn < 0)
    stop("burn must be >=0")
  
  # SET burn + 1
  burn <- burn + 1

  # OUTPUT vector, will hack off first values at the end
  x <- rep(0, len + burn)

  # Ranta and Kaitala 2001 Proc. R. Soc.
  s <- rnorm(len + burn, mean=0, sd=sd)

  for(i in (1:(len+burn-1))){
    x[i+1] <- b * x[i] + s[i] * sqrt(1 - b^2)
    if(trunc > 0){
      if (x[i+1] > (1 - trunc))  x[i+1] <- ( 1 - trunc)
      if (x[i+1] < (-1 + trunc)) x[i+1] <- (-1 + trunc)}
  }
  
  if (burn <= 0)
    return(x)
  
  x <- x[-(seq(burn))]
  
  return(x)
}# }}}
