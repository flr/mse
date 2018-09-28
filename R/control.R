# fwd.R - DESC
# /fwd.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# getCtrl
# 

getCtrl <- function(values, quantity, years, it, rel.year="missing"){
	dnms <- list(iter=1:it, year=years, c("min", "val", "max"))
	arr0 <- array(NA, dimnames=dnms, dim=unlist(lapply(dnms, length)))
	arr0[,,"val"] <- unlist(values)
	arr0 <- aperm(arr0, c(2,3,1))
	if(!missing(rel.year)){
		ctrl <- fwdControl(data.frame(year=years, quantity=quantity, val=NA, rel.year=rel.year))
	} else {
		ctrl <- fwdControl(data.frame(year=years, quantity=quantity, val=NA))
	}
	ctrl@trgtArray <- arr0
	ctrl
}

