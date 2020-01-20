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

library(mse)
library(FLa4a)
library(ggplotFL)
library(doParallel)

# LOAD data

data(ple4)
data(ple4.indices)

stk <- ple4
idx <- ple4.indices["BTS-Combined (all)"]

# VARIABLES

it <- 25 # iterations
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
# OM projection method
#==============================================================================

proj <- mseCtrl(method=fwd.om, args=list(maxF=2))

#==============================================================================
# OM object
#==============================================================================
om <- FLom(stock=stk, sr=srbh, refpts=refpts(brp), projection=proj)#, fleetBehaviour=fb)

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

iem <- FLiem(method=noise.iem, args=list(fun="rlnorm", mean=0, sd=0, multiplicative=TRUE))

###############################################################################
# Management procedure
###############################################################################

# general pars, add seed
mpargs$seed <- 1234

#==============================================================================
# Scenarios
#==============================================================================

#------------------------------------------------------------------------------
# base
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=FLQuant(0.3)))))

# run new method in single core without foreach
resp1 <- mp(om, oem, ctrl=ctrl, args=mpargs)

# run new method in 1 core with foreach
registerDoParallel(1)
resp1a <- mp(om, oem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp1), stock(resp1a))

# run new method in 3 cores with foreach
registerDoParallel(3)
resp1b <- mp(om, oem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp1), stock(resp1b))

#------------------------------------------------------------------------------
# base with TAC
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

# test parallel
# run new method in single core without foreach
resp2 <- mp(om, oem, ctrl=ctrl, args=mpargs)

# run new method in 1 core with sequential foreach
registerDoParallel(1)
resp2a <- mp(om, oem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp2), stock(resp2a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp2b <- mp(om, oem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp2), stock(resp2b))

#------------------------------------------------------------------------------
# base with TAC and SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))

# test parallel
# run new method in single core without foreach
resp3 <- mp(om, oem, ctrl=ctrl, args=mpargs)

# run new method in 1 core with sequential foreach
registerDoParallel(1)
resp3a <- mp(om, oem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp3), stock(resp3a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp3b <- mp(om, oem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp3), stock(resp3b))

#------------------------------------------------------------------------------
# base with TAC and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

# test parallel
# run new method in single core without foreach
resp4 <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)

# run new method in 1 core with sequential foreach
registerDoParallel(1)
resp4a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp4), stock(resp4a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp4b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp4), stock(resp4b))

#------------------------------------------------------------------------------
# base with TAC and SA and OEM and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))

# test parallel
# run new method in single core without foreach
resp5 <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)

# run new method in 1 core with sequential foreach
registerDoParallel(1)
resp5a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp5), stock(resp5a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp5b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp5), stock(resp5b))

#------------------------------------------------------------------------------
# testing biased assessment
#------------------------------------------------------------------------------
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

# test parallel
# run new method in single core without foreach
resp6 <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)

# run new method in 1 core with sequential foreach
registerDoParallel(1)
resp6a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp6), stock(resp6a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp6b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp6), stock(resp6b))

#------------------------------------------------------------------------------
# base with TAC and separable SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa, args=list(fit="assessment", qmodel=list(~s(age, k=3), fmodel=~s(age, k=4) + s(year, k=20), update=FALSE)))))

# test parallel
# run new method in single core without foreach
resp7 <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)

# run new method in 1 core with sequential foreach
registerDoParallel(1)
resp7a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp7), stock(resp7a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp7b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp7), stock(resp7b))

#==============================================================================
# Test again with cluster
#==============================================================================

#------------------------------------------------------------------------------
# base
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3))))

# run new method in single core without foreach
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {i <- 1})
registerDoParallel(cl)
resp1a <- mp(om, oem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp1), stock(resp1a))

# run new method in 2 cores with foreach
cl <- makeCluster(3)
clusterEvalQ(cl = cl, expr = {i <- 1})
registerDoParallel(cl)
resp1b <- mp(om, oem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp1), stock(resp1b))

#------------------------------------------------------------------------------
# base with TAC
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

# test parallel
# run new method in single core without foreach
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {i <- 1})
registerDoParallel(cl)
resp2a <- mp(om, oem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp2), stock(resp2a))

# run new method in 3 cores with foreach
cl <- makeCluster(3)
clusterEvalQ(cl = cl, expr = {i <- 1})
registerDoParallel(cl)
resp2b <- mp(om, oem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp2), stock(resp2b))

#------------------------------------------------------------------------------
# base with TAC and SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))

# test parallel
# run new method in single core without foreach
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {i <- 1;library(FLa4a)})
registerDoParallel(cl)
resp3a <- mp(om, oem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp3), stock(resp3a))

# run new method in 3 cores with foreach
cl <- makeCluster(3)
clusterEvalQ(cl = cl, expr = {i <- 1;library(FLa4a)})
registerDoParallel(cl)
resp3b <- mp(om, oem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp3), stock(resp3b))

#------------------------------------------------------------------------------
# base with TAC and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

# test parallel
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {i <- 1})
registerDoParallel(cl)
resp4a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp4), stock(resp4a))

# run new method in 2 cores with foreach
cl <- makeCluster(3)
clusterEvalQ(cl = cl, expr = {i <- 1;library(FLa4a)})
registerDoParallel(cl)
resp4b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp4), stock(resp4b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC and SA and OEM and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))

# test parallel
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {i <- 1;library(FLa4a)})
registerDoParallel(cl)
resp5a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp5), stock(resp5a))

# run new method in 2 cores with foreach
cl <- makeCluster(3)
clusterEvalQ(cl = cl, expr = {i <- 1;library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 3
resp5b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp5), stock(resp5b))

#------------------------------------------------------------------------------
# testing biased assessment
#------------------------------------------------------------------------------
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

# test parallel
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {i <- 1;library(FLa4a)})
registerDoParallel(cl)
resp6a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp6), stock(resp6a))

# run new method in 2 cores with foreach
cl <- makeCluster(3)
clusterEvalQ(cl = cl, expr = {i <- 1;library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 3
resp6b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp6), stock(resp6b))

#------------------------------------------------------------------------------
# base with TAC and separable SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa, args=list(fit="assessment", qmodel=list(~s(age, k=3), fmodel=~s(age, k=4) + s(year, k=20), update=FALSE)))))

# test parallel
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {i <- 1;library(FLa4a)})
registerDoParallel(cl)
resp7a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp7), stock(resp7a))

# run new method in 2 cores with foreach
cl <- makeCluster(3)
clusterEvalQ(cl = cl, expr = {i <- 1;library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 3
resp7b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
stopCluster(cl)
all.equal(stock(resp7), stock(resp7b))

#==============================================================================
# Tests iters in args
#==============================================================================
flq <- FLQuant(c(0.3,0.2,0.4), dim=c(1,1,1,1,1,3))

#------------------------------------------------------------------------------
# base
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq))))

#resp1 <- mp(om, oem, ctrl=ctrl, args=mpargs)

# run new method in single core without foreach
registerDoParallel(1)
resp1a <- mp(om, oem, ctrl=ctrl, args=mpargs)
#all.equal(stock(resp1), stock(resp1a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp1b <- mp(om, oem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp1a), stock(resp1b))

#------------------------------------------------------------------------------
# base with TAC
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is)))

#resp2 <- mp(om, oem, ctrl=ctrl, args=mpargs)

# run new method in single core without foreach
registerDoParallel(1)
resp2a <- mp(om, oem, ctrl=ctrl, args=mpargs)
#all.equal(stock(resp2), stock(resp2a))

# run new method in 3 cores with foreach
registerDoParallel(3)
resp2b <- mp(om, oem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp2a), stock(resp2b))

#------------------------------------------------------------------------------
# base with TAC and SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))

#resp3 <- mp(om, oem, ctrl=ctrl, args=mpargs)

# run new method in single core without foreach
registerDoParallel(1)
resp3a <- mp(om, oem, ctrl=ctrl, args=mpargs)
#all.equal(stock(resp3), stock(resp3a))

# run new method in 3 cores with foreach
registerDoParallel(3)
resp3b <- mp(om, oem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp3a), stock(resp3b))

#------------------------------------------------------------------------------
# base with TAC and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is)))

#resp4 <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)

# test parallel
registerDoParallel(1)
resp4a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
#all.equal(stock(resp4), stock(resp4a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp4b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp4a), stock(resp4b))

#------------------------------------------------------------------------------
# base with TAC and SA and OEM and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))

#resp5 <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)

# test parallel
registerDoParallel(1)
resp5a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
#all.equal(stock(resp5), stock(resp5a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp5b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp5a), stock(resp5b))

#------------------------------------------------------------------------------
# testing biased assessment
#------------------------------------------------------------------------------
biased.sa <- function(stk, idx, bbias=1, fbias=1, ...){
	args <- list(...)
	dy <- dimnames(catch(stk))[[2]]
	dy <- dy[length(dy)]
	tracking <- args$tracking
	stock.n(stk)[, dy] <- stock.n(stk)[, dy]*bbias
	harvest(stk)[, dy] <- harvest(stk)[, dy]*fbias
	list(stk = stk, tracking = tracking)
}

ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=biased.sa, args=list(fbias=.5))))

#resp6 <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)

# test parallel
registerDoParallel(1)
resp6a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
#all.equal(stock(resp6), stock(resp6a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp6b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp6a), stock(resp6b))

#------------------------------------------------------------------------------
# base with TAC and separable SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa, args=list(fit="assessment", qmodel=list(~s(age, k=3), fmodel=~s(age, k=4) + s(year, k=20), update=FALSE)))))

#resp7 <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)

# test parallel
registerDoParallel(1)
resp7a <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
#all.equal(stock(resp7), stock(resp7a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp7b <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)
all.equal(stock(resp7a), stock(resp7b))

