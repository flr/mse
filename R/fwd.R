# fwd.R - DESC
# /fwd.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' fwd
#' @examples
#' data(p4om)
#' res <- fwd(om, control=fwdControl(year=2018:2030, quant="f",
#'   value=rep(c(refpts(om)$FMSY), 13)))

setMethod("fwd", signature(object="FLom", fishery="missing",
  control="fwdControl"),

  function(object, control, maxF=4, deviances=residuals(sr(object)), ...) {
    
    stock <- fwd(stock(object), control=control, sr=sr(object), deviances=deviances,
      maxF=maxF, ...)

    stock(object) <- stock

    return(object)
  }
)

#' res <- fwd(om, fbar=FLQuant(c(refpts(om)$FMSY), dimnames=list(year=2018:2039, iter=1:250)))

setMethod("fwd", signature(object="FLom", fishery="missing", control="missing"),
  function(object, maxF=4, deviances=residuals(sr(object)), ...) {

    stock <- fwd(stock(object), sr=sr(object), deviances=deviances,
      maxF=maxF, ...)

    stock(object) <- stock

    return(object)
  }
)
