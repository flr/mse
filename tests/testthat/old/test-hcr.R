# test-hcr.R - DESC
# /test-hcr.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

data(ple4om)

# --- catchSSB.hcr {{{

# DATA & arguments

data(ple4om)

dy <- '2017'

stk <- iter(window(stock(om), start=1962, end=dy), 1)

# PLOT 'depletion' time series

plot(ssb(stk) %/% ssb(stk)[,1]) + ylim(c(0,NA)) +
  xlab("") + ylab("Depletion") + geom_hline(yintercept=0.40, linetype=2)

# GET years where depletion > 0.40

dep <- ssb(stk) %/% ssb(stk)[, 1]

# GET catches set by hcr for ayrs: dtarget=0.4, dlimit=0.10

ayrs <- 1980:2017

tacs <- unlist(lapply(ayrs, function(x) {
  catchSSB.hcr(stk, MSY=140000, dtarget=0.40, dlimit=0.10, args=list(ay=x, data_lag=1),
    tracking=FLQuant())$ctrl[1,]$value 
  } 
))

# PLOT HCR vs. decisions for given data_lag

res <- data.frame(dep=c(dep[, ac(ayrs)]), tac=tacs, ay=ayrs, dy=ayrs-1, my=ayrs+1)

ggplot(res, aes(x=dep, y=tac)) +
  # geom_text(aes(label=ay))
  geom_point() +
  xlim(c(0, NA)) + ylim(c(0, NA)) +
  geom_segment(x=0, xend=0.10, y=0, yend=0) +
  geom_segment(x=0.10, xend=0.40, y=0, yend=140000) +
  geom_segment(x=0.40, xend=0.8, y=140000, yend=140000) +
  ylab("TAC (t)") + xlab("Depletion")

# }}}

# --- ices {{{

# DATA & arguments

data(ple4om)

stk <- iter(window(stock(om), start=1962, end=dy), 1)

dy <- '2017'

fmin <- 0.05
ftrg <- 0.15
blim <- 240000
bsafe <- 400000

#' ices.hcr(stock(om), fmin=0.05, ftrg=0.15, blim=20000, bsafe=300000,
#'   args=list(ay=1995, data_lag=1, management_lag=1), tracking=FLQuant())

# PLOT ssb time series

plot(ssb(stk)) + ylim(c(0,NA)) + xlab("") + ylab("SSB (t)") +
  geom_hline(yintercept=c(blim, bsafe), linetype=c(2,3)) +
  annotate("text", x=1960, y=blim, label=expression(B[lim])) +
  annotate("text", x=1960, y=bsafe, label=expression(B[safe]))

# GET catches set by hcr for ayrs

ayrs <- 1980:2017

fs <- unlist(lapply(ayrs, function(x) {
  ices.hcr(stk, fmin=0.05, ftrg=0.15, blim=blim, bsafe=bsafe,
    args=list(ay=x, data_lag=1, management_lag=1),
    tracking=FLQuant())$ctrl[1,]$value 
  } 
))

# PLOT HCR vs. decisions for given data_lag

res <- data.frame(ssb=c(ssb(stk)[, ac(ayrs)]), f=fs, ay=ayrs, dy=ayrs-1, my=ayrs+1)

ggplot(res, aes(x=ssb, y=f)) +
  # geom_text(aes(label=ay))
  geom_point() +
  xlim(c(0, NA)) + ylim(c(0, NA)) +
  geom_segment(x=0, xend=blim, y=fmin, yend=fmin) +
  geom_segment(x=blim, xend=bsafe, y=fmin, yend=ftrg) +
  geom_segment(x=bsafe, xend=max(res$ssb), y=ftrg, yend=ftrg) +
  ylab(expression(bar(F))) + xlab("SSB (t)")

# }}}

# pid.hcr {{{

# TODO: FWD om for F trajectory

track <- FLQuants(FLQuant(dimnames=list(metric='hcr', year=2000:2005,
  iter=seq(dims(om)$iter))))

args <- list(ay=2003, data_lag=1, management_lag=1, frq=1, it=1)

pid.hcr(stock(om), ind=FLQuant(), tracking=track, args=args,
  nyears=5, metric=ssb, ref=yearMeans(ssb(om)), kp=0.5, ki=0.01, kd=0.7)

pid.hcr(stock(om), ind=FLQuant(), tracking=track, args=args,
  nyears=5, metric=ssb, ref=yearMeans(ssb(om)), kp=0.5, ki=0.02, kd=0.3)

# }}}
