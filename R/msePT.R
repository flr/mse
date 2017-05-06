# msePT.R - DESC
# /msePT.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# msePT {{{

#' An example function to carry out an MSE run for a given MP
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
#' @param PARAM Lorem ipsum dolor sit amet
#'
#' @return RETURN Lorem ipsum dolor sit amet
#'
#' @name FUNCTION
#' @rdname FUNCTION
#' @aliases FUNCTION
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords design
#' @examples

msePT <- function(
  # OM: FLStock + SR + cpue
  omp, sr, cpue=stock(stk),
  # years
  years, verbose=FALSE,
  # hcr
  hcr=~ifelse(dep <= Dlimit, 0,
    ifelse(dep < Dtarget, (lambda * MSY) / (Dtarget - Dlimit) * (dep - Dlimit),
    lambda * MSY)),
  # hcrparams
  hcrparams=FLPar(Dlimit=0.10, Dtarget=0.40, lambda=1.0, dltac=0.15, dhtac=0.15),
  # lags
  dlag=1, mlag=1, 
  # oem, imp
  oemparams=FLPar(sd=0, b=0), imparams, tune=FALSE) {

  # VARIABLES
  freq <- years[2] - years[1]

  # MESSAGES
  if(verbose)
    pb <- utils::txtProgressBar(min = years[1], max = years[length(years)],
      initial = 1, style=3)
  
  # TAC
  tac <- catch(omp)[, ac(seq(years[1] - dlag, years[length(years)] + freq))]

  # LOOP
  for (y in years) {

    # CATCH data to y-dlag
    stk <- window(omp, end=c(y - dlag))
    
    # oem w/ selectivity[, y - dlag] in weight
    obs <- quantSums(oem(stk[,ac(seq(y - dlag - freq, y - dlag))],
      sel=sel(stk)[,ac(y - dlag)], mass=TRUE))

    # DEBUG
    obs <- stock(stk)[,ac(seq(y - dlag - freq, y - dlag))]

    # EXTEND cpue from delta(obs)
    cpue[, ac(seq(y - dlag - freq + 1, y - dlag))] <- 
      cpue[, ac(y - dlag - freq)] %*% obs[,-1] / obs[, -dim(obs)[2]] %*%
      # E: LN(0, 0.3) + b
      # TODO b from history, sd from OM or actual value
      rlnoise(1, FLQuant(0, dimnames=dimnames(obs[,-1])[-6]), sd=c(oemparams$sd), b=c(oemparams$b))

    # DEBUG
    cpue[, ac(seq(y - dlag - freq + 1, y - dlag))] <- 
      cpue[, ac(y - dlag - freq)] %*% obs[,-1] / obs[, -dim(obs)[2]]
    
    # SA: (sb, MSY) <- bd(catch, cpue)
    # DEBUG
    sb <- window(stock(omp), end=y - dlag)
    MSY <- rpts$MSY

    dep <- sb[, ac(y - dlag)] / sb[,1]

    # DECISION
    ytac <- eval(hcr[[2]], c(as(hcrparams, 'list'),
      list(dep=dep, MSY=MSY)))
    
    # CONSTRAINT in TAC change
    ptac <- c(tac[, ac(y-dlag)])
    ytac <- pmax(ptac * (1 - hcrparams$dltac), pmin(ptac * (1 + hcrparams$dhtac), c(ytac)))

    # LOG tac
    tac[, ac(seq(y + mlag, length=freq))] <- rep(ytac, each=freq)

    # FWD w/IMP. ERROR + SR residuals
    # TODO ADD rec residuals
    # TODO ADD imp error
    omp <- fwd(omp, sr=sr,
      control=fwdControl(quant="catch", year=seq(y + mlag, length=freq), value=rep(ytac)))

    # DONE
    if(verbose)
      utils::setTxtProgressBar(pb, y)
  }

  if(verbose)
    cat("\n")

  # END
  if(tune)
    return(window(omp, start=years[1] - dlag - 1))
  else
    return(list(om=window(omp, start=years[1] - dlag - 1, end=years[length(years)] + mlag),
      tac=tac, cpue=cpue))

} # }}}
