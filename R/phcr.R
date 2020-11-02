# phcr.R - DESC
# mse/R/phcr.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# movingF.hcr {{{
movingF.phcr <- function(stk, frp="f0.1", model="missing", interval, args, hcrpars, tracking) {
	ay <- args$ay
	iy <- args$iy
	if(ay==iy | (ay-iy)%%interval==0){
		if(!missing(model)){
			sr0 <- fmle(as.FLSR(stk, model=model))
			hcrpars <- refpts(brp(FLBRP(stk, sr0)))[frp,"harvest"]
		} else {
			hcrpars <- refpts(brp(FLBRP(stk)))[frp,"harvest"]
		}
	}
	list(hcrpars=hcrpars, tracking=tracking)	
} # }}}

# indicator.phcr
indicator.phcr <- function(stk, itrg, args, tracking, ...){
	if(is(itrg, 'FLPar')) hcrpars <- itrg else hcrpars <- FLPar(itrg=itrg) 
	list(hcrpars=hcrpars, tracking=tracking)	
}


