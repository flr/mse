# sa.R - DESC
# /sa.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# perfect.sa {{{
perfect.sa <- function(stk, idx, genArgs, tracking, ...) {

  tracking["conv.est",ac(genArgs$ay)] <- 1

  list(stk=stk, tracking=tracking)
}
# }}}

