# dispatch.R - DESC
# mse/R/dispatch.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# dispatching

flsval <- list(object="stk", test="!is(object, \"FLS\")", msg="\"stk must be of class FLStock\"")

flival <- list(object="idx", test= "!is(object, \"FLIndices\")", msg="\"idx must be of class FLIndices\"")

flpval <- list(object="hcrpars", test= "!is(object, \"FLPar\")", msg="\"hcrpars must be of class FLPar\"")

flfval <- list(object="ctrl", test= "!is(object, \"fwdControl\")", msg="\"ctrl must be of class fwdControl\"")

flqval <- list(object="flq", test= "!is(object, \"FLQuant\")", msg="\"flq must be of class FLQuant\"")

# mpDispatch {{{
mpDispatch <- function(ioval, ...){
	args <- list(...)
	method <- args$method
	args$method <- NULL
	# checks in
	for(i in ioval$iv){
		object <- args[i$object]
		str <- paste("if(", i$test, ")", i$msg, sep=" ")
		eval(parse(text=str))
	}
	# dispatch
	out <- do.call(method, args)
	# checks out
	for(i in ioval$ov){
		object <- out[i$object]
		str <- paste("if(", i$test, ")", i$msg, sep=" ")
		eval(parse(text=str))
	}
	out
} # }}}

