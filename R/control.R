# fwd.R - DESC
# /fwd.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# getCtrl
# 

getCtrl <- function(values, quantity, years, it, rel.year="missing"){
	
  if(missing(rel.year))
    ctrl <- fwdControl(list(year=years, value=values, quant=quantity))
  else
    ctrl <- fwdControl(list(year=years, value=values, quant=quantity,
      relYear=rel.year))
	
  return(ctrl)
}
