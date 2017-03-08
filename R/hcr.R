# hcr.R - DESC
# /hcr.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

hcr <- function(stock, params, rule=f~ifelse(ssb > btrigger, fmsy, ssb %*% (fmsy / btrigger))) {

  # CHECKS

  # CREATE predictModel
  pm <- predictModel(
    model=rule,
    params=params)

  # eval predictModel
  res <- predict(pm, stock)

  return(res)
}

# cod
# hcr(cod[,'30'], params=FLPar(btrigger=415, fmsy=0.15))


# ple4
# data(ple4)
# p4sr <- fmle(as.FLSR(ple4, model='bevholt'))

# ftarget <- hcr(ple4[,'2000'], params=FLPar(btrigger=215000, fmsy=0.35))[[1]]

# res <- fwd(ple4, sr=p4sr, f=expand(ftarget, year=2001:2008))

# plot(FLStocks(HCR=res, PLE=ple4))
