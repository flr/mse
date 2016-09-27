# simple.R - DESC
# /simple.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(FLash)
library(FLBRP)

# LOAD OM (FLStock + FLSR)
data(cod)

cob <- FLBRP::brp(FLBRP::FLBRP(cod, sr=cos))

# PREPARE OMP
nyears <- 30

cop <- fwdWindow(cod, cob, end=dims(cod)$maxyear + nyears)

# VARIABLES

years <- seq(dims(cod)$maxyear + 1, length=nyears)

# DEFINE MP

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
}


#
R0 <- basic(cop, cos, years, fmult=0.01, ftar=0.05)
plot(R0)

# SET GRID

grid <- list(
  fmult=seq(0.1, 2, length=3),
  ftar=seq(0.01, 0.2, length=3))

# SUBSAMPLE

# RUN
library(doMC)
registerDoMC(6)

runs <- doRuns(basic, grid=grid, omp=cop, sr=cos, years=years)

# CALCULATE PERFORMANCE

refpts <- FLPar(SBMSY=c(refpts(cob)["msy","ssb"]))
inds <- list(I1=list(~yearMeans(SB/SBMSY), name="mean(SB/SBMSY)", desc="Mean SB / SBMSY"))

perf <- rbindlist(lapply(runs$runs, performance, refpts=refpts, indicators=inds),
  idcol="run")

perf[,dist:=data - 1]

perf[,.SD[dist==min(dist)],]

perf[,.SD[dist > 0.90 * min(dist)],]

