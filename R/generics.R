# generics.R - DESC
# /generics.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

setGeneric("performance", function(x, ...) standardGeneric("performance"))
setGeneric("rnoise", function(n, len, ...) standardGeneric("rnoise"))
setGeneric("rlnoise", function(n, len, ...) standardGeneric("rlnoise"))
setGeneric('oem', function(object,...) standardGeneric('oem'))
