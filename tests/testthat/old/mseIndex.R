# mseIndex.R - DESC
# /mseIndex.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(mse)
library(FLBRP)

data(ple4)
data(ple4.index)

psr <- fmle(as.FLSR(ple4, model='bevholt'))
prp <- brp(FLBRP(ple4, psr))
pom <- fwdWindow(ple4, prp, end=2040)

# -- F = 0
pte <- fwd(pom, sr=psr, f=FLQuant(0, dimnames=list(year=2006:2036)))

# -- DEBUG
omp <- pom
sr <- psr
cpue <- quantSums(index(ple4.index) * catch.wt(ple4)[1:8, ac(1985:2008)])

hcr <- ~tac * (1 + lambda * slope)
hcrparams <- FLPar(lambda=1.25, ny=5, dtac=0.15)

# No intermediate year
years <- seq(2008, 2038, by=2)
dlag <- 1
mlag <- 1 

pr0 <- mseIndex(omp=pom, sr=psr, cpue=cpue,
  hcrparams=FLPar(lambda=0.50, ny=5, dtac=0.15),
  years=years, oemparams=NA, imparams=NA, verbose=FALSE)

plot(pr0$om)


# -- GRID lambda = seq(-2, 2, length=20)
library(doParallel)
registerDoParallel(5)

lambda <- seq(-2, 2, length=20)

out <- foreach(i = lambda) %dopar% {

  mseIndex(omp=pom, sr=psr, index=NA,
    hcrparams=FLPar(lambda=i, ny=5, dtac=0.15),
    years=seq(2006, 2036, by=2), oemparams=NA, imparams=NA, verbose=FALSE)
  }

# NAMES out
names(out) <- lambda
res <- FLStocks(lapply(out, "[[", "omp"))

plot(res)


# --- ALB

library(ioalbmse)

data(oms)

aom <- fwdWindow(om, rp, end=2038)

# -- F = 0
ate <- fwd(aom, sr=sr, f=FLQuant(0, dimnames=list(year=2015:2038)))

# -- R0, lambda = 0.
pr0 <- mseIndex(omp=aom, sr=sr, cpue=propagate(ind, 100),
  hcrparams=FLPar(lambda=-1, ny=5, dtac=0.10),
  years=seq(2014, 2036, by=2), oemparams=NA, imparams=NA, verbose=FALSE)

plot(FLStocks(OM=om, R0=window(pr0$omp, end=2036), F0=ate))

ggplot(metrics(window(pr0$omp, end=2036)), aes(x=date, y=data, colour=iter)) + geom_line() +
  facet_grid(qname~., scales='free') + theme(legend.pos="none")

# -- GRID lambda = seq(-2, 2, length=20)
library(doParallel)
registerDoParallel(5)

lambda <- seq(-2, 2, length=20)

out <- foreach(i = lambda, .final = function(x) setNames(x, lambda)) %dopar% {

  mseIndex(omp=aom, sr=sr, index=NA,
    hcrparams=FLPar(lambda=i, ny=5, dtac=0.15),
    years=seq(2014, 2036, by=2), oemparams=NA, imparams=NA, verbose=FALSE)
  }

# NAMES out
aos <- FLStocks(sapply(out, "[", "omp"))
plot(aos)

perf <- performance(aos, indicators=indicators)
