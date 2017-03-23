# mseIndex.R - DESC
# /mseIndex.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# mseIndex {{{

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
#' 
#' # om (cod + codsr)
#' data(cod)
#' # Trim down for speed
#' cod <- iter(cod, 1:20)
#' codsr <- iter(codsr, 1:20)
#' # FLBRP
#' codrp <- brp(FLBRP(cod, sr=codsr))
#' # Expand om
#' com <- fwdWindow(cod, codrp, end=53)
#' 
#' # INTERMEDIATE year(s): constant w
#' com <- fwd(com, sr=codsr, f=expand(fbar(com)[,'30'], year=31:33))
#' 
#' refpts <- FLPar(
#'   Fmsy=refpts(codrp)['msy', 'harvest'],
#'   Bmsy=refpts(codrp)['msy', 'ssb'],
#'   B0=refpts(codrp)['virgin', 'ssb'],
#'   Blim=refpts(codrp)['f0.1', 'ssb'])
#'  
#' # MP params
#' years <- seq(32, 50, by=2)
#' 
#' # example
#' r0 <- mseIndex(omp=com, sr=codsr, refpts=refpts, index=NA,
#'   years=seq(30, 50, by=2), oemparams=NA, imparams=NA, verbose=TRUE)
#' 
#' plot(r0)
#' 

mseIndex <- function(
  # OM: FLStock + SR + RPs + index
  omp, sr, refpts, index,
  # years
  years, verbose=FALSE,
  # hcr
  hcr=~tac * (1 + lambda * slope),
  # hcrparams
  hcrparams=FLPar(lambda=1),
  # lags
  dlag=1, mlag=1, 
  # oem, imp
  oemparams, imparams) {

  # SETUP

  # MESSAGES
  if(verbose)
    pb <- utils::txtProgressBar(min = years[1], max = years[length(years)],
      initial = 1, style=3, title="Years:")

  # LOOP
  for (y in years) {

    # OBSERVATION
    # - catch + E
    stk <- window(omp, end=c(y-dlag))
    # - cpue + E
    cpue <- mpb::oem(stk, sel=harvest(stk)[,1]) # %*% FLife::rlnoise(1, stk)

    # INDICATOR (SA)
    dat <- data.table(as.data.frame(cpue[, ac(seq(y-dlag-4, y-dlag))], drop=TRUE))
    slope <- dat[, {coef(lm(log(data)~year))[2]}, by = iter]$V1

    # DECISION + error
    dec <- evalPredictModel(window(stk, start=y-1),
      predictModel(model=hcr, params=rbind(refpts, hcrparams)))

    ftarget <- FLQuant(dec, 
      dimnames=list(iter=dim(omp)[6]))

    # FWD
    omp <- fwd(omp, sr=codsr, f=expand(ftarget, year=seq(y + mlag, length=3)))

    # DONE
    if(verbose)
      setTxtProgressBar(pb, y)
  }

  # END
  return(omp)

} # }}}
