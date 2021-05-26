# simple.R - DESC
# /simple.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(FLasher)
library(FLBRP)
library(mse)

# LOAD OM (FLStock + FLSR)
data(cod)

cob <- brp(FLBRP(cod, sr=cos))
refpts <- FLPar(SBMSY=c(refpts(cob)["msy","ssb"]), FMSY=c(refpts(cob)["msy", "harvest"]))

# PREPARE OMP

nyears <- 30
cop <- fwdWindow(cod, cob, end=dims(cod)$maxyear + nyears)

save(cob, cop, file="com.RData")

# VARIABLES

years <- seq(dims(cod)$maxyear + 1, length=nyears)

# DEFINE MP (basic) {{{

basic <- function(omp, sr, years, fmult, ftar) {

  # LOOP over years
  for (y in years) {

    # SET fwdControl
    fta <- ifelse(fbar(omp)[,ac(y-1)] > ftar,  fbar(omp)[,ac(y-1)] * fmult, fbar(omp)[,ac(y-1)] / fmult)

    # fwd
    omp <- fwd(omp, asfwdControl(f=fta), sr=sr)
    omp <- FLash::fwd(omp, asfwdControl(f=fta), sr=list(params=sr@params, model='ricker'))
  }

  return(window(omp, end=y-1))
} # }}}

# RUN 0

R0 <- basic(cop, cos, years, fmult=0.01, ftar=0.05)
plot(R0)

# SET GRID

grid <- list(
  fmult=seq(0.1, 2, length=3),
  ftar=seq(0.01, 0.2, length=3))

# RUN

library(doMC)
registerDoMC(6)

runs <- doRuns(basic, grid=grid, omp=cop, sr=cos, years=years)

save(R0, runs, refpts, file="runs.RData")

# PERFORMANCE

inds <- list(
  I1=list(~yearMeans(SB/SBMSY), name="mean(ssb/SBMSY)", desc="Mean SB / SBMSY"),
  I2=list(~yearMeans(F/FMSY), name="mean(fbar/SBMSY)", desc="Mean F / FMSY"))

perf <- performance(runs, indicators=inds, refpts=refpts, years=c(10, 20))

perq <- performance(runs, indicators=inds, refpts=refpts, years=c(10, 20),
  probs=c(0.1, 0.25, 0.50, 0.75, 0.90))

# PLOTS

# Trade-off plot for multiple runs
plotTOs(perq, "I1", "I2", year="20", colkey="run")

# Plot of OM + RUNS
plotOMR(cod, window(runs, start=25))
plotOMR(cod, window(runs, start=25), refpts["SBMSY",])
