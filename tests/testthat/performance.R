# performance.R - DESC
# /performance.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


indicators <- list(
  A1 = list(~yearMeans(C), name= "mean(C)"),
  F1 = list(~yearMeans(F), name="var(F)"))

indicators <- list(
  # S1
  S1 = list(~yearMeans(SB/SB0), name = "mean(SB/SB_0)",
    desc = "Mean spawner biomass relative to unfished"),
  # S2
  S2 = list(~apply(SB/SB0, c(1, 3:6), min), name = "min(SB/SB_0)",
    desc = "Minimum spawner biomass relative to unfished"))

refpts=FLPar(SBMSY=15000, FMSY=0.3)

data(ple4)



indicators <- list(S6=list(~yearSums((SB > SBMSY) + (F < FMSY))/length(SB), name = "P(Green)",
  desc = "Probability of being in Kobe green quadrant"))

indicators <- list(S6=list(~iterSums((SB > SBMSY) + (F < FMSY))/length(SB), name = "P(Green)",
  desc = "Probability of being in Kobe green quadrant"))

performance(propagate(ple4, 10), indicators=indicators, refpts=refpts, years=2008)
