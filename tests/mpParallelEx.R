library(FLCore)

data(ple4)

#' PROPAGATE object
object <- propagate(ple4, 300)

# RESHUFFLE iters, to check names are kept in the same order
object <- FLCore::iter(object, sample(1:300, 300))

library(doParallel)

# MULTIPLE outputs, .multicombine

foo <- function(object, nblocks=getDoParWorkers()) {

  # CHOOSE %do% or %dopar%
  if(nblocks > 1)
    `%fun%` <- `%dopar%`
  else
    `%fun%` <- `%do%`

  # SPLIT iters in nblocks
  iters <- split(seq(dims(object)$iter), sort(seq(dims(object)$iter)%%nblocks) + 1)

  # LOOP and combine
  out <- foreach(i=iters, .combine=function(...)
    list(stock=do.call('combine', lapply(list(...), '[[', 'stock')),
         tracking=do.call('combine', lapply(list(...), '[[', 'tracking'))),
    .multicombine=TRUE, .errorhandling = "stop", .inorder=TRUE) %fun% {

    # SUBSET object(s)
    if(nblocks > 1)
      object <- FLCore::iter(object, i)

    # PRINT pid
    print(Sys.getpid())

    # RETURN
    list(stock=object, tracking=ssb(object))

  }

  return(out)
}


registerDoParallel(1)
res <- foo(object)

registerDoParallel(2)
res <- foo(object)
res <- foo(object, 1)

registerDoParallel(3)
res <- foo(object)
res1 <- foo(object, 1)


