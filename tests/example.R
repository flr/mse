# example.R - DESC
# mse/tests/example.R

# Copyright European Union, 2018
# Authors: Finlay Scott (EC JRC)
#          Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# ==============================================================================
# SETUP
# ==============================================================================

# LOAD packages

library(FLa4a)
library(ggplotFL)
library(mse)

# LOAD data

data(ple4)
data(ple4.indices)

stk <- ple4
idx <- ple4.indices["BTS-Combined (all)"]

# VARIABLES

it <- 2 # iterations
fy <- 2030 # final year
y0 <- range(stk)["minyear"] # initial OM year
dy <- range(stk)["maxyear"] # final OM year
iy <- dy # initial year of projection (also intermediate)
#ny <- fy - iy + 1 # number of years to project from initial year
nsqy <- 3 # number of years to compute status quo metrics
vy <- ac(iy:fy) # vector of years to be projected

mpargs <- list(fy=fy, y0=y0, iy=iy, nsqy=nsqy)

# ==============================================================================
# OM conditioning
# ==============================================================================

# - Two SRRs: geomean and Bevholt

mcsave <- 500
mcmc <- mcsave*it

fit <- sca(stk, idx, fit="MCMC", mcmc = SCAMCMC(mcmc = mcmc, mcsave = mcsave, mcprobe = 0.4))

stk <- stk + fit

# skin to keep one iteration
stk0 <- qapply(stk, iterMedians)

# Fit a4a model to replicate official assessment w/MCMC

# average recruitment estimation sd
rv1 <- sqrt(mean(c(iterVars(log(rec(stk)))), na.rm=TRUE))

# average autocor lag1
# TODO acf(residuals)
ac1 <- mean(apply(window(rec(stk), end=2008)@.Data, 6, function(x)
  c(acf(c(x), plot=FALSE, lag.max=1)$acf[2])))

# BevHolt
srbh <- fmle(as.FLSR(stk0, model="bevholt"), method="L-BFGS-B", lower=c(1e-6, 1e-6), upper=c(max(rec(stk)) * 3, Inf))

# Residuals
resbh <- ar1rlnorm(rho=ac1, years=dy:fy, iters=it, margSD=rv1*2)
residuals(srbh) <- resbh

# ==============================================================================
# Refpts
# ==============================================================================

brp <- brp(FLBRP(stk0, srbh))

# ==============================================================================
# Set up operating model
# ==============================================================================

# Set up future assumptions - means of 3 years
stk <- fwdWindow(stk, brp, end=2030)

#==============================================================================
# Fleet behaviour
#==============================================================================

fb <- mseCtrl(method=hyperstability.fb, args=list(beta=0.8))

#==============================================================================
# OM object
#==============================================================================
om <- FLom(stock=stk, sr=srbh, refpts=refpts(brp))#, fleetBehaviour=fb)

###############################################################################
# OEM settings
###############################################################################

#==============================================================================
# prepare objects
#==============================================================================

stk <- stock(om)

#==============================================================================
# Estimate the indices catchability from the a4a fit (without simulation)
#==============================================================================

set.seed(0)

# Use all indices
idcs <- FLIndices()
for (i in 1:length(idx)){
	# this is a simplification as if index reflects 01 January abundances
	lst <- mcf(list(idx[[i]]@index, stock.n(stk0)))
	# log catchability of index 
	idx.lq <- log(lst[[1]]/lst[[2]]) 
	# empty quant
	idx.qmu <- idx.qsig <- stock.n(iter(stk,1)) 
	# Every year has the same mean catchability
	idx.qmu[] <- yearMeans(idx.lq) 
	idx.qsig[] <- sqrt(yearVars(idx.lq))
	idx.q <- FLQuant(NA, dimnames=dimnames(stock.n(stk)))
	# Build FLQ of index catchability based on lognormal distribution with mean and sd calculated above
	idx.q <- rlnorm(it, idx.qmu, idx.qsig) 
	#idx.q[,ac(y0:iy)] <- idx.q[,ac(y0:iy)]
	idx_temp <- idx.q * stock.n(stk)
	# generate initial index
	idx_temp <- FLIndex(index=idx_temp, index.q=idx.q) 
	range(idx_temp)[c("startf", "endf")] <- c(0, 0)
	idcs[[i]] <- idx_temp
}
names(idcs) <- names(idx)

#==============================================================================
# Deviances for catch.n
#==============================================================================

set.seed(0)

catch.dev <- log(catch.n(stk))
catch.dev <- catch.dev-iterMeans(catch.dev)
Sig <- apply(catch.dev[,ac(y0:dy),1,1,,drop=TRUE], 3, function(x) cov(t(x)))
Sig <- apply(Sig, 1, mean)
Sig <- matrix(Sig, ncol=dim(catch.dev)[1])
catch.dev[,ac(vy)][] <- t(mvrnorm(it * length(vy), rep(0, nrow(Sig)), Sig))
catch.dev <- exp(catch.dev)

#==============================================================================
# OEM object
#==============================================================================

idxDev <- lapply(idcs, index.q)
names(idxDev) <- "index.q"
stkDev <- FLQuants(catch.n=catch.dev)
dev <- list(idx=idxDev, stk=stkDev)
obs <- list(idx=idcs[1], stk=stk)

oem <- FLoem(method=sampling.oem, observations=obs, deviances=dev)
#save(oem, file="oem.RData")

###############################################################################
# Implementation error
###############################################################################

iem <- FLiem(method=noise.iem, args=list(fun="rlnorm", mean=0, sd=0.1, multiplicative=TRUE))

###############################################################################
# Management procedure
###############################################################################

# general pars, add seed
mpargs$seed <- 1234

#==============================================================================
# Scenarios
#==============================================================================

# base
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3))))

res1 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
res2 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

registerDoParallel(2)
mpargs$nblocks <- 2
resp2 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# base with TAC
ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

res2 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# base with TAC and SA
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))

res3 <- mp(om, FLoem(), ctrl.mp=ctrl, genArgs=mpargs)

# base with TAC and IEM
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

res4 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# base with TAC and SA and OEM and IEM
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))

res5 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# testing biased assessment
biased.sa <- function(stk, idx, bbias=1, fbias=1, ...){
	args <- list(...)
	dy <- dimnames(catch(stk))[[2]]
	dy <- dy[length(dy)]
	tracking <- args$tracking
	stock.n(stk)[, dy] <- stock.n(stk)[, dy]*bbias
	harvest(stk)[, dy] <- harvest(stk)[, dy]*fbias
	list(stk = stk, tracking = tracking)
}

ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.2)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=biased.sa, args=list(fbias=.5))))

res6 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# base with TAC and separable SA
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa)))

res7 <- mp(om, FLoem(), ctrl.mp=ctrl, genArgs=mpargs)

plot(FLStocks(om=window(stock(om), end=2017), sce1=stock(res1), sce2=stock(res2), sce3=stock(res3), sce4=stock(res4), sce5=stock(res5), sce6=stock(res6), sce7=stock(res7)))


