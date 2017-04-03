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
#'
#' # Trim down for speed
#' cod <- iter(cod, 1:20)
#' codsr <- iter(codsr, 1:20)
#'
#' # FLBRP
#' codrp <- brp(FLBRP(cod, sr=codsr))
#'
#' years <- seq(30, 50, by=2)
#'
#' # Expand om
#' com <- fwdWindow(cod, codrp, end=53)
#' 
#' # example
#' r0 <- mseIndex(omp=com, sr=codsr, index=NA,
#'   hcrparams=FLPar(lambda=5, ny=5, dtac=0.15),
#'   years=years, oemparams=NA, imparams=NA, verbose=TRUE)
#' 
#' plot(r0$omp) + geom_vline(aes(xintercept=as.numeric(ISOdate(30,1,1))))
#'
#' tune(mseIndex, grid=list(lambda=seq(-2, 10)), indicators, refpts, ...)

mseIndex <- function(
  # OM: FLStock + SR + RPs + index
  omp, sr, index,
  # years
  years, verbose=FALSE,
  # hcr
  hcr=~tac * (1 + lambda * slope),
  # hcrparams
  hcrparams=FLPar(lambda=1.25, ny=5, dtac=0.15),
  # lags
  dlag=1, mlag=1, 
  # oem, imp
  oemparams, imparams) {

  # VARIABLES
  freq <- years[2] - years[1]

  # MESSAGES
  if(verbose)
    pb <- utils::txtProgressBar(min = years[1], max = years[length(years)],
      initial = 1, style=3, title="Years:")
  
  # TAC
  tac <- catch(omp)[, ac(seq(years[1] - dlag, years[length(years)] + freq))]

  # LOOP
  for (y in years) {

    # OBSERVATION
    # - catch + E
    stk <- window(omp, end=c(y - dlag))
    # - cpue +
    # TODO sel by iter
    cpue <- quantSums(oem(stk, sel=harvest(stk)[,1], mass=TRUE)) %*%
      # E: LN(0, 0.3) + b
      # TODO b from history
      # TODO sd from OM or actual value
      rlnoise(1, FLQuant(0, dimnames=dimnames(stock(stk))[-6]), sd=0.3, b=0)

    # INDICATOR (SA)
    dat <- data.table(as.data.frame(cpue[, ac(seq(y-dlag-4, y-dlag))], drop=FALSE))
    slope <- dat[year %in% seq(y - dlag - hcrparams$ny, y - dlag),
      {coef(lm(log(data)~year))[2]}, by = iter]$V1

    # DECISION
    ytac <- eval(hcr[[2]], c(as(hcrparams, 'list'),
      list(tac=c(tac[,ac(y - dlag)]), slope=slope)))
    
    # CONSTRAINT in TAC change
    ptac <- c(tac[, ac(y-dlag)])
    ytac <- pmax(ptac * (1 - hcrparams$dtac), pmin(ptac * (1 + hcrparams$dtac), ytac))

    # LOG tac
    tac[, ac(seq(y + mlag, length=freq))] <- rep(ytac, each=freq)

    # DEBUG
    print(paste(y, ytac/ptac, sep=" - "))
    
    # FWD w/ERROR + SR residuals
    # TODO get rec var in history
    omp <- fwd(omp, sr=sr,
      catch=FLQuant(c(ytac), dimnames=list(year=seq(y + mlag, length=freq))))

    # DONE
    if(verbose)
      setTxtProgressBar(pb, y)
  }

  # END
  return(list(omp=window(omp, start=years[1] - dlag - 1), tac=tac))

} # }}}
