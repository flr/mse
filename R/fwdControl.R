# fwdControl.R - DESC
# ioalbmse/R/fwdControl.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# asfwdControl(missing) {{{
asfwdControl <- function(...) {
		
		args <- list(...)

		# CHECK length == 1
		if (length(args) != 1)
			stop("Method needs a single FLQuant")

		# NAME
		quantity <- names(args)

		# CONVERT
		flq <- args[[1]]
		df <- as.data.frame(flq)[,c('year', 'iter', 'data')]

		# ITERS
		if(dim(flq)[6] == 1) {
			target <- cbind(df[,c('year', 'data')], quantity=quantity)
			names(target)[grep('data', names(target))] <- 'val'

			return(fwdControl(target))
		} else {

			target <- cbind(df[df$iter == df$iter[1],][,c('year', 'data')], quantity=quantity)
			names(target)[grep('data', names(target))] <- 'val'

			arrt <- array(NA, dim=c(dim(target)[1], 3, dim(flq)[6]),
				dimnames=list(seq(dim(target)[1]), c('min', 'val', 'max'), iter=dimnames(flq)$iter))
			arrt[,'val',] <- c(flq)
			
			return(fwdControl(target, trgtArray=arrt))
		}
	stop('Conversion unsucessful')
}

# }}}
