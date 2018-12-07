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

it <- 3 # iterations
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
res1 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
library(doParallel)

# run new method in single core without foreach
mpargs$nblocks <- 1
resp1 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 3
resp1a <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 3
resp1b <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1b))

#------------------------------------------------------------------------------
# base with TAC
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

res2 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp2 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp2a <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp2b <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2b))

#------------------------------------------------------------------------------
# base with TAC and SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))

res3 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp3 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp3a <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp3b <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3b))

#------------------------------------------------------------------------------
# base with TAC and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

res4 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp4 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp4a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp4b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4b))

#------------------------------------------------------------------------------
# base with TAC and SA and OEM and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))

res5 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp5 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp5a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp5b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5b))

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

res6 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp6 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp6a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp6b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6b))

#------------------------------------------------------------------------------
# base with TAC and separable SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa, args=list(fit="assessment", qmodel=list(~s(age, k=3), fmodel=~s(age, k=4) + s(year, k=20), update=FALSE)))))

res7 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp7 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 3
resp7a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp7b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7b))

#plot(FLStocks(om=window(stock(om), end=2017), sce1=stock(res1), sce2=stock(res2), sce3=stock(res3), sce4=stock(res4), sce5=stock(res5), sce6=stock(res6), sce7=stock(res7)))

#==============================================================================
# Test again with cluster
#==============================================================================

cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)

#------------------------------------------------------------------------------
# base
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3))))

# run new method in single core without foreach
mpargs$nblocks <- 1
resp1 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp1a <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
resp1b <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)

ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp2 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp2a <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp2b <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC and SA
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp3 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp3a <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp3b <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC and IEM
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp4 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp4a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp4b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC and SA and OEM and IEM
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp5 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp5a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp5b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5b))
stopCluster(cl)

#------------------------------------------------------------------------------
# testing biased assessment
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
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
mpargs$nblocks <- 1
resp6 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp6a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp6b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC and separable SA
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa, args=list(fit="assessment"))))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp7 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp7a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp7b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7b))
stopCluster(cl)

#==============================================================================
# More tests
#==============================================================================

#------------------------------------------------------------------------------
# Iters in args
#------------------------------------------------------------------------------
flq <- FLQuant(c(0.3,0.2,0.4), dim=c(1,1,1,1,1,3))
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(
	method=fixedF.hcr, 
	args=list(ftrg=flq)
	)))
res1 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel

# run new method in single core without foreach
mpargs$nblocks <- 1
resp1 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1))
all.equal(res1, resp1) # genArgs will be different 

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 3
resp1a <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res1, resp1a)
all.equal(stock(res1), stock(resp1a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 3
resp1b <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res1, resp1b)
all.equal(stock(res1), stock(resp1b))

#------------------------------------------------------------------------------
# base with TAC
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is)))
res2 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp2 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res2, resp2)
all.equal(stock(res2), stock(resp2))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp2a <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res2, resp2a)
all.equal(stock(res2), stock(resp2a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp2b <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res2, resp2b)
all.equal(stock(res2), stock(resp2b))

#------------------------------------------------------------------------------
# base with TAC and SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))
res3 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp3 <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res3, resp3)
all.equal(stock(res3), stock(resp3))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp3a <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res3, resp3a)
all.equal(stock(res3), stock(resp3a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp3b <- mpParallel(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res3, resp3b)
all.equal(stock(res3), stock(resp3b))

#------------------------------------------------------------------------------
# base with TAC and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is)))
res4 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp4 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res4, resp4)
all.equal(stock(res4), stock(resp4))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp4a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res4, resp4a)
all.equal(stock(res4), stock(resp4a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp4b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res4, resp4b)
all.equal(stock(res4), stock(resp4b))

#------------------------------------------------------------------------------
# base with TAC and SA and OEM and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))
res5 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp5 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res5, resp5)
all.equal(stock(res5), stock(resp5))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp5a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res5, resp5a)
all.equal(stock(res5), stock(resp5a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp5b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res5, resp5b)
all.equal(stock(res5), stock(resp5b))

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
	ctrl.est = mseCtrl(method=biased.sa, args=list(fbias=flq))))
res6 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp6 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res6, resp6)
all.equal(stock(res6), stock(resp6))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp6a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res6, resp6a)
all.equal(stock(res6), stock(resp6a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp6b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res6, resp6b)
all.equal(stock(res6), stock(resp6b))

#------------------------------------------------------------------------------
# base with TAC and separable SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa)))
res7 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp7 <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res7, resp7)
all.equal(stock(res7), stock(resp7))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp7a <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res7, resp7a)
all.equal(stock(res7), stock(resp7a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp7b <- mpParallel(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res7, resp7b)
all.equal(stock(res7), stock(resp7b))


