# plots.R - DESC
# ioalbmse/R/plots.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# plotTOs {{{

#' plotTOs(perq, "S1", "T1", "2028") + xlim(c(0, 0.30)) + ylim(c(0.5, 1.5))
plotTOs <- function(data, x, y, year=max(data$year), alpha=0.5, colkey="mp") {
  
  # HACK: scoping issue in data.table due to match arg & col names
  ye <- year
  
  dat <- data[indicator %in% c(x, y),][year %in% ye,]
  names <- unique(dat[indicator %in% c(x, y), name])
  
  # TURN run and mp (if it exists) into character
  dat[, 'run' := lapply(.SD, as.character), .SDcols='run']
  if("mp" %in% names(dat)) {
    dat[, mp := lapply(.SD, as.character), .SDcols='mp']

    dat <- dcast(dat, year + run + mp ~ indicator, sep="",
      value.var=c('10%','25%','50%','75%','90%'))
  } else {
    dat <- dcast(dat, year + run ~ indicator, sep="",
      value.var=c('10%','25%','50%','75%','90%'))
  }
  
  p <- ggplot(dat, aes_q(x=as.name(paste0("50%", x)),
      y=as.name(paste0("50%", y)), group=as.name(colkey), colour=as.name(colkey))) +
    # vertical lines
    geom_linerange(aes_q(ymin=as.name(paste0("10%", y)),
      ymax=as.name(paste0("90%", y))), alpha=alpha) +
    geom_linerange(aes_q(ymin=as.name(paste0("25%", y)),
      ymax=as.name(paste0("75%", y))), size=1, alpha=alpha) +
    # horizontal lines
    ggstance::geom_linerangeh(aes_q(xmin=as.name(paste0("10%", x)),
      xmax=as.name(paste0("90%", x))), alpha=alpha) +
    ggstance::geom_linerangeh(aes_q(xmin=as.name(paste0("25%", x)),
      xmax=as.name(paste0("75%", x))), size=1, alpha=alpha) +
    # 50% point
    geom_point(aes_q(), alpha=alpha) + scale_shape(solid=FALSE) +
    xlab(paste(x, names[1], sep=": ")) +
    ylab(paste(y, names[2], sep=": ")) 
  
  return(p)
}
# }}}

# plotOMR {{{
plotOMR <- function(om, runs, refpts, qname="ssb",
  ylab=paste0(toupper(qname), " (", units(qua), ")")) {

  # GET element, slot or method
  foo <- get(qname)

  if(!missing(refpts)) {
    qua <- foo(om) %/% refpts
    quas <- lapply(runs, function(x) foo(x) %/% refpts)
  } else {
    qua <- foo(om)
    quas <- lapply(runs, foo)
  }

  p1 <- plot(qua) + ylab(ylab) +
    geom_vline(xintercept=as.numeric(ISOdate(dims(qua)$maxyear,1,1)), linetype=2, colour='darkgrey')

  p2 <- plot(quas) + facet_wrap(~qname) + ylab(ylab) +
    geom_vline(xintercept=as.numeric(ISOdate(dims(qua)$maxyear,1,1)), linetype=2, colour='darkgrey')

  if(!missing(refpts)) {
    p1 <- p1 + geom_hline(aes(yintercept=1), linetype=2) 
    p2 <- p2 + geom_hline(aes(yintercept=1), linetype=2) 
  }

  # TODO DO with grid.arrange
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(4, 2)))
  vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
  print(p1, vp = vplayout(1, 1:2))
  print(p2, vp = vplayout(2:4, 1:2))

  invisible()
} # }}}
