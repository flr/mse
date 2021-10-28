# test-FLom.R - DESC
# /test-FLom.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(FLjjm)


# --- CLASS FLomBF

data(cjm_2stk)

# goFish

projection(om) <- mseCtrl(method=fwdbf.om,
  args=list(FCB=FCB(F=1:4, C=1, B=c(1,1,2,1))))

method(oem) <- cjm2.oem

ctrl <- mpCtrl(list(
  hcr = mseCtrl(method=catchHockey.hcr,
    args=list(Btrigger=c(6000, 2500), Ctarget=c(300, 50))),
  est = mseCtrl(method=jjms.sa, args=list(dat=observations(oem)$dat,
    ctl=observations(oem)$ctl, mp=TRUE))
))

om <- fwd(om, control=fwdControl(list(year=2020, value=c(50,50,50,50), quant="catch",
    fishery=rep(1:4, each=1), catch=1), FCB=FCB(F=1:4, C=1, B=c(1,1,2,1))))

mpargs <- list(iy=2020, data_lag=1)

timet0 <- system.time(
t0 <- mp(omf, oem=oem, iem=NULL, ctrl=ctrl, args=mpargs, scenario="test",
   verbose=TRUE)
)
