# plot.R - DESC
# /home/mosquia/Active/MSE_PKG@flr/mse/R/plot.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# FLmse, missing {{{
setMethod("plot", signature(x="FLmse", y="missing"),
  function(x, ...) {
    # PLOT om
    plot(om(x), ...)
  }
)

# }}}

# .plotom {{{
.plotom <- function(stocks, window=TRUE) {
 
  # WINDOW om
  if(isTRUE(window))
    maxyear <- min(unlist(lapply(stocks[-1], function(i) dims(i)$minyear)))
  else
    maxyear <- min(unlist(lapply(stocks[-1], function(x) dims(i)$maxyear)))
    
  stocks[[1]] <- window(stocks[[1]], end=maxyear)

  # SORT OUT names
  if(is.null(names(stocks)))
    names(stocks) <- c("OM", paste0("MP", seq(length(stocks) - 1)))
 
  # PLOT FLStocks
  plot(FLStocks(stocks))

}
# }}}

# FLo, FLmse, ... {{{
setMethod("plot", signature(x="FLo", y="FLmse"),
  function(x, y, ..., window=TRUE) {

    # MERGE all FLmse args
    y <- c(list(y), list(...))

    plot(x, y, window=window)
  })

setMethod("plot", signature(x="FLom", y="list"),
  function(x, y, window=TRUE) {

    # MERGE stocks in list
    stks <- FLStocks(c(stock(x), lapply(y, stock)))

    # PLOT
    .plotom(stks, window=window)

  }
)
# }}}

# FLombf, list, ... {{{
setMethod("plot", signature(x="FLombf", y="list"),
  function(x, y, window=TRUE) {

    # EXTRACT stock(om) as FLStocks
    oms <- stock(x)

    # EXTRACT FLStocks from each FLmse
    mses <- lapply(y, stock)

    # COMBINE by biol
    stks <- lapply(seq(oms), function(i)
      FLStocks(c(list(oms[[i]]), lapply(mses, "[[", i)))
    )

    if(length(stks) == 1)
      .plotom(stks[[1]], window=window)
    else
      Reduce("|", lapply(stks, .plotom, window=window))
  })
# }}}

# FLo, fwdControl {{{

setMethod("plot", signature(x="FLo", y="fwdControl"),
  function(x, y, fill="#E69F00", ...) {

    yrs <- range(y$year)

    # CREATE standard plot
    p <- plot(x, ...)

    # GET x variable
    if(rlang::as_name(p$mapping$x) == "date") {
      yrs <- range(p$data[with(p$data, year %in% yrs), "date"])
    }

    p + geom_vline(xintercept=yrs[1], alpha=0.4) +
      geom_vline(xintercept=yrs[2], alpha=0.2) +
      annotate("rect", xmin = yrs[1], xmax = yrs[2], ymin = -Inf, ymax = Inf,
        fill = fill, alpha=0.1)
  }
) # }}}
