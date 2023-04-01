# statistics.R - performance statistics
# mse/data-raw/statistics.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

statistics <- list(
  # SB0
  SB0 = list(~yearMeans(SB/SB0), name = "SB/SB[0]",
    desc = "Mean spawner biomass relative to unfished"),
  # minSB0
  minSB0 = list(~apply(SB/SB0, c(1, 3:6), min), name = "min(SB/SB[0])",
    desc = "Minimum spawner biomass relative to unfished"),
  # SBMSY
  SBMSY = list(~yearMeans(SB/SBMSY), name = "SB/SB[MSY]",
    desc = "Mean spawnwer biomass relative to SBMSY"),
  # Ftarget
  Ftarget = list(~yearMeans(F/Ftarget), name = "F/F[target]",
    desc = "Mean fishing mortality relative to target"),
  # FMSY
  FMSY = list(~yearMeans(F/FMSY), name = "F/F[MSY]",
    desc = "Mean fishing mortality relative to FMSY"),
  # green
  green = list(~yearSums(FLQuant((SB / SBMSY) > 1 & (F / FMSY) < 1)) / dim(SB)[2],
    name = "P(Green)", desc = "Probability of being in Kobe green quadrant"),
  # red
  red = list(~yearSums(FLQuant((SB / SBMSY) < 1 & (F / FMSY) > 1)) / dim(SB)[2],
    name = "P(Red)", desc = "Probability of being in Kobe red quadrant"),
  # PSBMSY
  PSBMSY = list(~yearMeans((SB / SBMSY) >= 1), name = "P(SB>=SB[MSY])",
    desc = "Probability of SB greater or equal to SBMSY"),
  # PSBlim
  PSBlim = list(~yearMeans((SB / SBlim) > 1), name = "P(SB>SB[limit])", 
    desc = "Probability that spawner biomass is above SBlim"),
  # PSB20B0
  PSB20B0 = list(~yearSums((SB / (0.2 * SB0)) > 1) / dim(SB)[2],
    name = "P(SB > 0.20 %*% SB[0])", 
    desc = "Probability that spawner biomass is above 20% SB[0]"),
  # risk1
  risk1 = list(~yearMeans(iterMeans((SB / SBlim) < 1)),
    name = "mean(P(SB<B[limit]))", 
    desc = "ICES Risk 1, mean probability that spawner biomass is below Blim"),
  # risk2
  risk2 = list(~yearMeans(iterMeans(((SB / SBlim) < 1) > 0)),
    name = "once(P(SB<B[limit]))", 
    desc = "ICES Risk 2, probability that spawner biomass is above Blim once"),
  # risk3
  risk3 = list(~apply(iterMeans((SB / SBlim) < 1), c(1,3:6), max),
    name = "max(P(SB>B[limit]))", 
    desc = "ICES Risk 3, max probability that spawner biomass is above Blim"),
  # C
  C = list(~yearMeans(C), name = "mean(C)", desc = "Mean catch over years"),
  # C/MSY
  CMSY = list(~yearMeans(C/MSY), name = "C/MSY", desc = "Mean proportion of MSY"),
  # AAV
  AAVC = list(~yearMeans(abs(C[, -1] - C[, -dim(C)[2]]) / C[, -1]), name = "AAV(C)",
    desc = "Average annual variability in catch"),
  # IACC
  IACC = list(~100 * yearSums(abs(C[, -1] - C[, -dim(C)[2]])) / yearSums(C),
  name="IAC(C)", desc="Percentage inter-annual change in catch"),
  # PC0
  PC0 = list(~yearSums(C < 0.01 * MSY) / dim(C)[2], name = "P(shutdown)", 
    desc = "Probability of fishery shutdown")
  )

kobestatistics <- list(
  # green
  green = list(~iterSums(FLQuant((SB / SBMSY) >= 1 & (F / FMSY) < 1)) / dim(SB)[6],
    name = "P(Green)", desc = "Probability of being in Kobe green quadrant"),
  # orange
  orange = list(~iterSums(FLQuant((SB / SBMSY) >= 1 & (F / FMSY) >= 1)) / dim(SB)[6],
    name = "P(Orange)", desc = "Probability of being in Kobe orange quadrant"),
  # yellow
  yellow = list(~iterSums(FLQuant((SB / SBMSY) < 1 & (F / FMSY) < 1)) / dim(SB)[6],
    name = "P(Yellow)", desc = "Probability of being in Kobe yellow quadrant"),
  # red
  red = list(~iterSums(FLQuant((SB / SBMSY) < 1 & (F / FMSY) >= 1)) / dim(SB)[6],
    name = "P(Red)", desc = "Probability of being in Kobe red quadrant")
)

save(statistics, kobestatistics, file="../data/statistics.RData", compress="xz")
