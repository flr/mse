# plot.R - DESC
# /home/mosquia/Active/MSE_PKG@flr/mse/R/plot.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# FLo, ... {{{
setMethod("plot", signature(x="FLo", y="missing"),
  function(x, ...) {

    args <- list(...)
    metrics <- args$metrics

    # COMPUTE metrics
    if(is.null(metrics))
      mets <- metrics(x)
    else
      mets <- do.call("metrics", list(object=x, metrics=metrics))

    plot(mets) + ylim(c(0, NA))
  }
)
setMethod("plot", signature(x="FLo", y="FLmse"),
  function(x, y, ..., window=TRUE) {

    # MERGE all FLmse args
    y <- c(list(y), list(...))

    plot(x, y, window=window)
  }
)

setMethod("plot", signature(x="FLom", y="list"),
  function(x, y, window=TRUE, ...) {

    # WINDOW om
    if(isTRUE(window))
      maxyear <- min(unlist(lapply(y, function(i) dims(i)$minyear)))
    else
      maxyear <- min(unlist(lapply(y, function(i) dims(i)$maxyear)))
  
    x <- window(x, end=maxyear)

    # PARSE args
    args <- list(...)
    metrics <- args$metrics

    # COMPUTE metrics
    if(is.null(metrics))
      mets <- lapply(c(list(OM=x), y), metrics)
    else
      mets <- lapply(c(list(OM=x), y), metrics, metrics=metrics)

    # PLOT
    args$metrics <- NULL
    do.call(plotListFLQuants, c(list(x=mets), args))
  }
)
# }}}

# FLmse, missing {{{
setMethod("plot", signature(x="FLmse", y="missing"),
  function(x, ...) {
    # PLOT om
    plot(om(x), ...)
  }
)

# }}}

# .plotom {{{
.plotom <- function(stocks, window=TRUE, ...) {
 
  # WINDOW om
  if(isTRUE(window))
    maxyear <- min(unlist(lapply(stocks[-1], function(i) dims(i)$minyear)))
  else
    maxyear <- min(unlist(lapply(stocks[-1], function(i) dims(i)$maxyear)))
    
  stocks[[1]] <- window(stocks[[1]], end=maxyear)

  # SORT OUT names
  if(is.null(names(stocks)))
    names(stocks) <- c("OM", paste0("MP", seq(length(stocks) - 1)))
 
  # PLOT FLStocks
  plot(FLStocks(stocks), ...)

}
# }}}

# FLombf {{{
setMethod("plot", signature(x="FLombf", y="missing"),
  function(x, ...) {
    
    # GET extra args
    args <- list(...)

    # DISPATCH if args are FLmse
    if(length(args) == 0) {

      # 1. SSB
      bs <- lapply(ssb(x), unitSums)
      ubs <- units(bs[[1]])
      bs <- lapply(bs, function(u) {units(u) <- ""; return(u)})
      p1 <- plot(bs) + ylim(c(0,NA)) +
        ylab(paste0("SSB (", ubs , ")"))

      # 2. F
      fs <- lapply(fbar(x), unitMeans)
      fs <- lapply(fs, function(u) {units(u) <- ""; return(u)})
      p2 <- plot(fs) + ylim(c(0,NA)) +
        ylab(paste0("F"))

      # 3. F
      rs <- lapply(rec(x), unitSums)
      urs <- units(rs[[1]])
      rs <- lapply(rs, function(u) {units(u) <- ""; return(u)})
      p3 <- plot(rs) + ylim(c(0,NA)) +
        ylab(paste0("Rec (", urs , ")"))

      # 4. catch by fleet
      cas <- lapply(catch(fisheries(x)), unitSums)

      p4 <- ggplot(cas, aes(x=date, y=data, group=qname, fill=qname,
          colour=qname)) +
        geom_flquantiles(probs=c(0.25, 0.50, 0.75), alpha=0.3) +
        geom_flquantiles(probs=c(0.05, 0.50, 0.95), alpha=0.3) +
        ylim(c(0, NA)) +
        ylab(paste0("Catch (", units(cas[[1]]), ")")) +
        xlab("") +
        # TODO: WRITE geom_label_end
          geom_label_repel(data=as.data.frame(
          lapply(cas, function(x) window(iterMedians(x), start=-1)),
          date=TRUE), aes(label = qname), nudge_x = 2, na.rm = TRUE,
          fill='white') +
        theme(legend.position="none")

      # COMBINE p1 + p2
      return(wrap_plots(p1/p2/p3, p4))
    
    } else {

      plot(x, args)

    }
 }) # }}}

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
