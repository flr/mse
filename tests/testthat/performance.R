# performance.R - DESC
# /performance.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

data(cod)

refpts <- FLPar(SSBMSY=400, SSBlim=300, FMSY=0.18)

indicators <- list(
  # S3
  S3 = list(~yearMeans(SSB/SSBMSY), name = "mean(SSB/SSB_MSY)",
    desc = "Mean spawnwer biomass relative to BMSY"),
  # S5
  S5 = list(~yearMeans(F/FMSY), name = "mean(F/F_MSY)",
    desc = "Mean fishing mortality relative to FMSY"),
  # S6
  S6 = list(~FLQuant(sum((SSB / SSBMSY) > 1 & (F / FMSY) < 1) / length(SSB)),
    name = "P(Green)", desc = "Probability of being in Kobe green quadrant"),
  # F2
  F2 = list(~iterProb((SSB / SSBlim) > 1), name = "P(B > Blim)", 
    desc = "Probability that spawner biomass is above Blim"),
  # Y1
  Y1 = list(~yearMeans(C), name = "mean(C)", desc = "Mean catch over years"),
  # T1
  T1 = list(~yearMeans(C[, -1]/C[, -dims(C)$year]), name = "mean(C_t / C_t-1)",
    desc = "Mean absolute proportional change in catch"))




indicators <- list(
  # S3
  S3 = list(~yearMeans(SSB/SSBMSY), name = "mean(SSB/SSB_MSY)",
    desc = "Mean spawnwer biomass relative to BMSY"),
  # S5
  S5 = list(~yearMeans(F/FMSY), name = "mean(F/F_MSY)",
    desc = "Mean fishing mortality relative to FMSY"),
  # S6
  S6 = list(~FLQuant(sum((SSB / SSBMSY) > 1 & (F / FMSY) < 1) / length(SSB)),
    name = "P(Green)", desc = "Probability of being in Kobe green quadrant"),
  # F2
  F2 = list(~FLQuant(sum((SSB / SSBlim) > 1) / length(SSB)), name = "P(B > Blim)", 
    desc = "Probability that spawner biomass is above Blim"),
  # Y1
  Y1 = list(~yearMeans(C), name = "mean(C)", desc = "Mean catch over years"),
  # T1
  T1 = list(~yearMeans(C[, -1]/C[, -dims(C)$year]), name = "mean(C_t / C_t-1)",
    desc = "Mean absolute proportional change in catch"))

run <- window(cod, start=20)

perf <- performance(run, indicators, refpts, years=30, mp='OM')

perf <- performance(run, indicators, refpts, years=list(20:25, 20:30), mp='OM')

perf <- performance(run, indicators, refpts, years=list(short=20:25, medium=20:30), mp='OM')
