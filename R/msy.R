setGeneric('fmsy',     function(object,...) standardGeneric('fmsy'))
setGeneric('bmsy',     function(object,...) standardGeneric('bmsy'))

setMethod('fmsy', signature(object='FLBRP'), function(object,...)                             
  computeRefpts(object)["msy","harvest"])
setMethod('bmsy', signature(object='FLBRP'), function(object,...)                             
  computeRefpts(object)["msy","ssb"])


hcrParam=function(ftar,btrig,fmin,blim){
  
  setNms=function(x,nm,nits){
    
    names(dimnames(x))[1]='params'
    dimnames(x)[[1]]     =nm
    if (nits!=dims(x)$iter)
      x=propagate(x,nits)
    
    return(x)}
  
  nits=max(laply(list(ftar,btrig,fmin,blim), function(x) dims(x)$iter))
  
  ftar =setNms(ftar, nm='ftar', nits)
  btrig=setNms(btrig,nm='btrig',nits)
  fmin =setNms(fmin, nm='fmin', nits)
  blim =setNms(blim, nm='blim', nits)
  
  if (nits==1) res=FLPar(  array(c(ftar,btrig,fmin,blim),c(4,nits),dimnames=list(params=c('ftar','btrig','fmin','blim'),iter=seq(nits)))) else
    res=FLPar(t(array(c(ftar,btrig,fmin,blim),c(nits,4),dimnames=list(iter=seq(nits),params=c('ftar','btrig','fmin','blim')))))
  
  #units(res)='harvest'
  return(res)}

