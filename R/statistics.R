# statistics.R - DESC
# /home/mosqu003/Projects/FLR/code/mse/mse/R/statistics.R

# Copyright (c) WMR, 2025.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


#' A complete set of performance statistics
#'
#' A list containing a large number of performance statistics:
#' @format A named list with elements of class list, each containing three elements: a formula, a long name ('name'), and a description ('desc'), both of type 'character':
#' \describe{
#'   \item{SB}{SB: Mean spawner biomass}
#'   \item{SB0}{SB/SB\[0]: Mean spawner biomass relative to unfished}
#'   \item{minSB0}{min(SB/SB\[0]): Minimum spawner biomass relative to unfished}
#'   \item{SBMSY}{SB/SB\[MSY]: Mean spawnwer biomass relative to SBMSY}
#'   \item{F}{F: Mean fishing mortality}
#'   \item{Ftarget}{F/F\[target]: Mean fishing mortality relative to target}
#'   \item{FMSY}{F/F\[MSY]: Mean fishing mortality relative to FMSY}
#'   \item{green}{P(Green): Probability of being in Kobe green quadrant}
#'   \item{orange}{P(Orange): Probability of being in Kobe orange quadrant}
#'   \item{yellow}{P(Yellow): Probability of being in Kobe yellow quadrant}
#'   \item{red}{P(Red): Probability of being in Kobe red quadrant}
#'   \item{PSBMSY}{P(SB>=SB\[MSY]): Probability of SB greater or equal to SBMSY}
#'   \item{PSBlim}{P(SB>SB\[limit]): Probability that spawner biomass is above SBlim}
#'   \item{PSB20B0}{P(SB > 0.20 %*% SB\[0]): Probability that spawner biomass is above 20% SB[0]}
#'   \item{risk1}{mean(P(SB<B\[limit])): ICES Risk 1, mean probability that spawner biomass is below Blim}
#'   \item{risk2}{once(P(SB<B\[limit])): ICES Risk 2, probability that spawner biomass is above Blim once}
#'   \item{risk3}{max(P(SB>B\[limit])): ICES Risk 3, max probability that spawner biomass is above Blim}
#'   \item{C}{mean(C): Mean catch over years}
#'   \item{CMSY}{C/MSY: Mean proportion of MSY}
#'   \item{AAVC}{AAV(C): Average annual variability in catch}
#'   \item{IACC}{IAC(C): Percentage inter-annual change in catch}
#'   \item{PC0}{P(shutdown): Probability of fishery shutdown, defined as catch less than 10% of MSY}
#' }
#' @docType data
#' @keywords datasets
#' @name statistics
#' @usage data(statistics)
NULL

# items <- Map(function(name, stat)
#   paste0("#'   \\item{", name, "}{", stat$name, ":", stat$des, ".}"),
#   name=names(statistics), stat=statistics)
# EDIT:
# - CHANGE [ to \[
