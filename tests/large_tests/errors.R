library(mse)
library(FLa4a)
library(ggplotFL)
load("test.rdata")

#------------------------------------------------------------------------------
# OK
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

# test parallel
# run new method in single core without foreach
resp2 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# run new method in 1 core with sequential foreach
registerDoParallel(1)
resp2a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(resp2), stock(resp2a))

# run new method in 2 cores with foreach
registerDoParallel(3)
resp2b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(resp2), stock(resp2b))

#------------------------------------------------------------------------------
# ERROR: quando número de iters por bloco não é igual
#------------------------------------------------------------------------------

# run new method in 1 core with sequential foreach
registerDoParallel(1)
resp2a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp2b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

#------------------------------------------------------------------------------
# ERROR: cluster também falha
#------------------------------------------------------------------------------

cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {i <- 1})
registerDoParallel(cl)
resp1a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)


