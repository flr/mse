# statistics.R - DESC
# mse/R/statistics.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


#' Performance Statistics
#'
#' A list of key statistics used in fisheries management to evaluate the performance
#' of management procedures. This is a long list that contains statistics that measure
#' very similar outcomes, so a subset should be made of those most relevant to the
#' management objectives of interest.
#'
#' @format A list containing named elements, each of which represents a specific statistic. Each of them contains:
#' - \code{formula}: A formula defining how the metric is calculated.
#' - \code{name}: A short descriptive name. This can contain [plotmath()] expressions to be parsed by plot functions.
#' - \code{desc}: A more detailed description of the metric.
#'
#' @details
#' Performance statistics are used by the [performance()] method to compute time series,
#' or aggregates along time, of quantities of interest related to the result of applying
#' a particular management procedure to an operating model. They combione [metrics()]
#' computed from the projected stocks, populations and fisheries, with biological, 
#' economic or other reference points, but can also use the results of calculations and 
#' decisions carried out by the MP.
#'
#' Of the three elements in the list used to define each statistic, the first unnamed 
#' element, of class 'formula' is the one evaluated by [performance()]. The formula is 
#' evaluated with access to the reference points of the OM, contained in the `refpts`
#' slot, a set of [metrics] obtained from the projected OM, the contents of the 
#' [tracking] table with decisions and outputs from the MP internal calcultions, as well
#' as any function available in the workspace.
#'
#' The statistics currently included are:
#' \describe{
#'   \item{SB}{Spawner biomass in tonnes (\eqn{SB}), from the `SB` metric.}
#'   \item{SB0}{Spawner biomass relative to unfished (\eqn{SB/SB0}), requires the `SB0` refpt.}
#'   \item{minSB0}{Minimum spawner biomass relative to unfished (\eqn{min(SB/SB0)}) requires the `SB0` refpt..}
#'   \item{SBMSY}{Spawner biomass relative to \eqn{SB[MSY]} (\eqn{SB/SB[MSY]}) requires the `SBMSY` refpt..}
#'   \item{R}{Recruitment (\eqn{R}), from the `R` metric.}
#'   \item{F}{Fishing mortality (\eqn{F}), from the `F` metric.}
#'   \item{Ftarget}{Fishing mortality relative to target (\eqn{F/F[target]}) requires the `Ftarget` refpt.}
#'   \item{FMSY}{Fishing mortality relative to \eqn{F[MSY]} (\eqn{F/F[MSY]}) requires the `FMSY` refpt.}
#'   \item{green}{Probability of being in the Kobe green quadrant (\eqn{P(Green)}),  requires the `SBMSY` and `FMSY` refpts.}
#'   \item{orange}{Probability of being in the Kobe orange quadrant (\eqn{P(Orange)}),  requires the `SBMSY` and `FMSY` refpts.}
#'   \item{yellow}{Probability of bein,  requires the `SBMSY` and `FMSY` refptsg in the Kobe yellow quadrant (\eqn{P(Yellow)}),  requires the `SBMSY` and `FMSY` refpts.}
#' #'   \item{red}{Probability of being in the Kobe red quadrant (\eqn{P(Red)}),  requires the `SBMSY` and `FMSY` refpts.}
#'   \item{PSBMSY}{Probability that spawner biomass is greater than or equal to \eqn{SB[MSY]} (\eqn{P(SB>=SB[MSY])}),  requires the `SBMSY` refpt.}
#'   \item{PSBlim}{Probability that spawner biomass is above \eqn{SB[lim]} (\eqn{P(SB>SB[lim])}),  requires the `SBlim` refpt.}
#'   \item{PSB20B0}{Probability that spawner biomass is above 20% of \eqn{SB0} (\eqn{P(SB > 0.20 %*% SB0)}),  requires the `SB0`refpt.}
#'   \item{risk1}{ICES Risk 1: Probability that spawner biomass is below \eqn{B[lim]} (\eqn{P(SB<B[lim])}), requires the `SBlim` refpt.}
#'   \item{risk2}{ICES Risk 2: Probability that spawner biomass falls below \eqn{B[lim]} at least once (\eqn{once(P(SB<B[lim]))}), requires the `SBlim` refpt.}
#'   \item{risk3}{ICES Risk 3: Maximum probability that spawner biomass is below \eqn{B[lim]} (\eqn{max(P(SB<B[lim]))}), requires the `SBlim` refpt.}
#'   \item{C}{Catch in tonnes (\eqn{C[t]}), from the `C` metric.}
#'   \item{CMSY}{Proportion of maximum sustainable yield (\eqn{C/MSY}), requires the `MSY` refpt.}
#'   \item{IACC}{Percentage inter-annual change in catch (\eqn{IAC(C)}), from the `C` metric.}
#'   \item{PIACC20}{Probability that the inter-annual change in catch being less than 20% (\eqn{P(IAC(C)<0.20)}), from the `C` metric.}
#'   \item{PC0}{Probability of fishery shutdown (\eqn{P(shutdown)}), defined as catch falling below 10% of MSY, so requires the `MSY` refpt.}
#' }
#'
#' @examples
#' data(statistics)
#' # Access a specific statistic
#' statistics$SBMSY
"statistics"
