# dispatch.R - DESC
# mse/R/dispatch.R

# Copyright European Union, 2018
# Author: Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#         Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# dispatching rules {{{

flsval <- list(object="stk", test="!is(object, \"FLS\")",
  msg="\"stk must be of class FLStock\"")

flssval <- list(object="stk", test="!is(object, \"FLStocks\")",
  msg="\"stk must be of class FLStocks\"")

flival <- list(object="idx", test= "!is(object, \"FLIndices\")",
  msg="\"idx must be of class FLIndices\"")

flpval <- list(object="hcrpars", test= "!is(object, \"FLPar\")",
  msg="\"hcrpars must be of class FLPar\"")

flfval <- list(object="ctrl", test= "!is(object, \"fwdControl\")",
  msg="\"ctrl must be of class fwdControl\"")

flqval <- list(object="flq", test= "!is(object, \"FLQuant\")",
  msg="\"flq must be of class FLQuant\"")

flqsval <- list(object="ind", test= "!is(object, \"FLQuants\")",
  msg="\"ind must be of class FLQuants\"")

floval <- list(object="om", test="!is(object, \"FLo\")",
  msg="\"om must be of class FLo\"")

# }}}

# mpDispatch {{{

mpDispatch <- function(ioval, ..., step){

  # GET arguments
	args <- list(...)

  # EXTRACT method and REMOVE from args

	method <- args$method
	args$method <- NULL
	
  # CHECK input objects
	for(i in ioval$iv){
		object <- args[[i$object]]
		str <- paste("if(", i$test, ") stop(", i$msg, ")", sep=" ")
		eval(parse(text=str))
	}
	
  # DISPATCH, only args in formals if no '...'.
	if("..." %in% names(formals(method)))
	  out <- do.call(method, args)
  else
    out <- do.call(method, args[names(args) %in% names(formals(method))])
	
  # CHECK output objects
	for(i in ioval$ov){
		object <- out[[i$object]]
		str <- paste("if(", i$test, ") stop(", i$msg, ")", sep=" ")
		eval(parse(text=str))
	}
	
  return(out)
} 
# }}}
