library(FLBRP)
library(FLash)

load("/home/laurie/Desktop/tmp/t.RData")

rtn   = fwd(object, f=hvt,sr=rf)
fbar(rtn)[,dimnames(hvt)$year]
hvt

iter(FLBRP:::refpts(rf)["msy",1],18)
iter(params(rf),18)

fbar(fwd(iter(object,1:17),f=iter(hvt,1:17), sr=iter(rf,1:17)))[,dimnames(hvt)$year]
fbar(fwd(iter(object,1:18),f=iter(hvt,1:18), sr=iter(rf,1:18)))[,dimnames(hvt)$year]

hvt[,,,,,18]=hvt[,,,,,19]
iter(FLBRP:::refpts(rf),18)=iter(FLBRP:::refpts(rf),19)
fbar(fwd(iter(object,1:18),f=iter(hvt,1:18), sr=iter(rf,1:18)))[,dimnames(hvt)$year]

iter(params(rf),18)=iter(params(rf),19)
fbar(fwd(iter(object,1:18),f=iter(hvt,1:18), sr=iter(rf,1:18)))[,dimnames(hvt)$year]

source('~/Desktop/flr/FLash/R/fwdControl.R')
source('~/Desktop/flr/FLash/R/setSRs.R')
source('~/Desktop/flr/FLash/R/validityFLSR.R')
source('~/Desktop/flr/FLash/R/fwd.R')


