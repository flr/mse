# oem.R - DESC
# /oem.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' @title oem
#'
#' @description Creates an \code{FLQuant} to represent an index of relative abundance
#' 
#' @param object \code{FLStock} for which an index is to be generated
#' @param sel \code{FLQuant} vector at age that shapes the catch or biomass.
#' additive, by default set to \code{TRUE},  
#' @param fish.depenpent \code{logical} that determines whether the index is proportional to the 
#' stock or catch/effort, by default set to \code{TRUE}
#' @param effort \code{character} c("h","f") that determines what proxy to use for effort,
#' i.e. harvest rate or instanteous fishing mortality 
#' @param mass \code{logical} default is TRUE, whether index is in mass or numbers
#' @param ... other arguments
#' 
#' @aliases 
#' oem,FLStock-method 
#' 
#' @export
#' @rdname oem
#' 
#' @return \code{FLQuant} with time series
#' 
#' @aliases oem-method oem,FLStock,ANY-method oem 
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#'  data(ple4) 
#'  cpue <- oem(ple4) 
#'  }
setMethod('oem',   signature(object='FLStock'),
  function(object, sel=FLQuant(FLQuant(1, dimnames=dimnames(harvest(object)))),
    timing = 0.5, fish.dependent = TRUE, effort = c("f","h"), mass = TRUE) {

    timing <- pmax(pmin(timing, 1.0), 0.0)
    stock <- (stock(object)[, -dim(stock(object))[2]] * timing +
      stock(object)[, -1]*(1.0 - timing))
  
  if (fish.dependent) {
    if (effort[1] == "h")
      E <- catch(object) %/% stock
    else  
      E <- fbar(object)
    
    cpue <- (catch.n(object) %*% sel) %/% E
  } else { 
    cpue <- apply((stock%*%sel), 2:6, sum)
  }
  if (mass)
    cpue <- cpue %*% catch.wt(object)

  return(cpue)
  }
)

hyperstability<-function(object,omega=1,ref=apply(object,c(1,3:6),mean)) 
  ref%*%((object%/%ref)^omega)

bias<-function(object,bias=0.02) 
  FLQuant(cumprod(1+rep(bias,dim(object)[2])),dimnames=dimnames(object))



