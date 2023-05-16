# phcr.R - DESC
# mse/R/phcr.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# movingF.phcr {{{
movingF.phcr <- function(stk, frp="f0.1", model="missing", interval, args, hcrpars, tracking) {

  # args
	ay <- args$ay
	iy <- args$iy

  # RUN brp() with or without SR fit
	if(ay == iy | (ay - iy) %% interval == 0){
		if(!missing(model)){
			sr0 <- fmle(as.FLSR(stk, model=model))
			hcrpars <- refpts(brp(FLBRP(stk, sr0)))[tolower(frp),"harvest"]
		} else {
			hcrpars <- refpts(brp(FLBRP(stk)))[tolower(frp),"harvest"]
		}
	}
	list(hcrpars=hcrpars, tracking=tracking)	
} # }}}

# brp.phcr {{{

brp.phcr <- function(stk, model, params, args, tracking) {

  brp <- brp(FLBRP(stk, sr=list(model=model, params=params)))

  rps <- refpts(brp)

  hcrpars <- FLPar(sbsafe=c(rps["msy", "ssb", ]))

  return(list(hcrpars=hcrpars, tracking=tracking))
}
# }}}

# run(2022), Fsqo(2023), F0(2024)

# intermediate.phcr {{{

intermediate.phcr <- function(stk, sr) {

  # FWD seq(iy, ay) w/ fbar=Fsquo

  #

}
# }}}
