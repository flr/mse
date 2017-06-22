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

# msePT {{{
msePT <- function(

  # OM: FLStock + SR +
  omp, sr,
  # CPUE
  cpue, cpuesel,
  # years
  years,
  # hcr
  hcr=~ifelse(dep <= Dlimit, 0,
    ifelse(dep < Dtarget, (lambda * MSY) / (Dtarget - Dlimit) * (dep - Dlimit),
    lambda * MSY)),
  # hcrparams
  hcrparams=FLPar(Dlimit=0.10, Dtarget=0.40, lambda=1.0, dltac=0.15, dhtac=0.15),
  # lags
  dlag=1, mlag=1, 
  # oem, imp
  oemparams=FLPar(sd=0, b=0), imparams,
  # options
  tune=FALSE, verbose=FALSE, sa=TRUE) {

  # VARIABLES
  freq <- years[2] - years[1]

  # MESSAGES
  if(verbose)
    pb <- utils::txtProgressBar(min = years[1], max = years[length(years)],
      initial = 1, style=3)
  
  # TAC
  tac <- catch(omp)[, ac(seq(years[1] - dlag, years[length(years)] + freq))]

  # LOOP
  for (y in years[-length(years)]) {
    
    # CATCH data to y-dlag, NO ERROR
    stk <- window(omp, end=c(y - dlag))
    
    # oem w/ selectivity[, y - dlag] in weight
    obs <- quantSums(oem(stk[,ac(seq(y - dlag - freq, y - dlag))],
      sel=cpuesel, mass=TRUE))
    
    # EXTEND cpue from delta(obs)
    cpue[, ac(seq(y - dlag - freq + 1, y - dlag))] <- 
      cpue[, ac(y - dlag - freq)] %*% obs[,-1] / obs[, -dim(obs)[2]] %*%
      # E: LN(0, sd) + b
      rlnoise(dim(obs)[6], FLQuant(0, dimnames=dimnames(obs[,-1])[-6]),
        sd=c(oemparams$sd), b=c(oemparams$b))

    # --- SA: (sb, MSY) <- bd(catch, cpue)
    if(sa) {
    
    # CREATE bd object
      # TODO ADD E (sampling noise) E ~ N(hr*B, sqrt(B*hr*(1-hr)))
    bd <- mpb::biodyn(catch=catch(stk))

    # Initial GUESS for k
    params(bd)['k',] <- 20 * mean(catch(bd))

    # SET initial values
    bd <- mpb::fwd(bd, catch=catch(bd))
    mpb::setParams(bd) <- window(cpue, end=y-dlag)
    mpb::setControl(bd) <- params(bd)
    bd@control["p",] <- c(-1, 0.0001, 0.0001, 0.0001)

    # FIT bd (17 sec, 200 iter)
    res <- mpb::fit(bd, index=window(cpue, end=y-dlag))

    # RESULTS
    sb <- res@stock
    MSY <- mpb::refpts(res)['msy']
    # ---

    } else {
    
    # HACK, direct obs of OM
    sb <- window(vb(stk), end=y - dlag)
      sb[, ac(y - dlag)] <- sb[, ac(y - dlag)] * rlnoise(dim(sb[, ac(y - dlag)])[6],
      FLQuant(0, dimnames=dimnames(sb[, ac(y - dlag)])[-6]),
      sd=c(oemparams$sd), b=c(oemparams$b))
    MSY <- refpts$MSY
    }
    
    # CALCULATE depletion
    dep <- sb[, ac(y - dlag)] / sb[, 1]
    
    # DECISION at y + mlag
    ytac <- suppressMessages(eval(hcr[[2]], c(as(hcrparams, 'list'),
      list(dep=dep, MSY=MSY))))
    
    # CONSTRAINT in TAC change
    ptac <- c(tac[, ac(y-dlag)])
    ytac <- pmax(ptac * (1 - hcrparams$dltac), pmin(ptac * (1 + hcrparams$dhtac), c(ytac)))

    # LOG tac
    tac[, ac(seq(y + mlag, length=freq))] <- rep(ytac, each=freq)

    # FWD w/IMP. ERROR + SR residuals
    # TODO ADD imp error
    omp <- fwd(omp, sr=sr,
      control=fwdControl(quant="catch", year=seq(y + mlag, length=freq), value=rep(ytac)),
      residuals=sr$residuals)

    # DONE
    if(verbose)
      utils::setTxtProgressBar(pb, y)
  }

  if(verbose)
    cat("\n")

  # END
  if(tune)
    return(window(omp, start=years[1] - dlag - 1, end=years[length(years)]))
  else
    return(list(om=window(omp, start=years[1] - dlag - 1, end=years[length(years)]),
      tac=window(tac, end=years[length(years)]), cpue=cpue))

} # }}}
