  hcrSBT1=function(cpue,tac,k1=2.0,k2=3.0,gamma=1,nyrs=5,lag=1,interval=3){
  
  dat=as.data.frame(cpue[,ac(-((nyrs-1):0)+dims(cpue)$maxyear)])
  lambda=as.FLQuant(ddply(dat, .(iter), with,  data.frame(data=coefficients(lm(data~year))[2])))
  
  flag  =lambda<0
  lambda=abs(lambda)
  
  res=1+ifelse(flag,-k1,k2)*lambda^ifelse(flag,gamma,1)
  res=res*tac
  
  dmns=dimnames(tac)
  dmns$year=as.integer(dmns$year)+lag+seq(interval)-1
  dmns$iter=dimnames(cpue)$iter
  
  res=FLQuant(rep(c(res),each=interval),dimnames=dmns)
  
  return(res)}

mseEMP<-function(
  #OM as FLStock and FLBRP
  om,eql,
  
  #MP,
  control=c(k1=3.0,k2=1.5,gamma=1,nyrs=5,lag=1,interval=3),
  
  #years over which to run MSE, doesnt work if interval==1, this is a bug
  start=range(om)["maxyear"]-30,interval=3,end=range(om)["maxyear"]-interval,
  
  #Stochasticity, either by default or suppliedas args
  srDev=FLife:::rlnoise(dim(om)[6],FLQuant(0,dimnames=list(year=start:end)),0.3), 
  uDev =FLife:::rlnoise(dim(mp)[6],FLQuant(0,dimnames=dimnames(iter(stock.n(om),1))),0.2),
  
  #Capacity, i.e. F in OM can not be greater than this
  maxF=1.5){ 
  
  ##So you dont run to the end then crash
  end=min(end,range(om)["maxyear"]-interval)
  
  ## Make sure number of iterations are consistent
  nits=c(om=dims(om)$iter, eql=dims(params(eql))$iter, rsdl=dims(srDev)$iter)
  if (length(unique(nits))>=2 & !(1 %in% nits)) ("Stop, iters not '1 or n' in om")
  if (nits['om']==1) stock(om)=propagate(stock(om),max(nits))
  
  ## Limit on capacity, add to fwd(om) if you want
  maxF=FLQuant(1,dimnames=dimnames(srDev))%*%apply(fbar(window(om,end=start)),6,max)*maxF
  
  ## Observation Error (OEM) setup 
  pGrp=range(om)["plusgroup"]
  
  cpue=window(stock.n(om),end=start)[dimnames(uDev)$age]
  cpue=cpue%*%uDev[,dimnames(cpue)$year]
  
  ## Loop round years
  cat('\n==')
  for (iYr in seq(start,end,interval)){
    cat(iYr,", ",sep="")
    
    ## Observation Error, using data from last year back to the last assessment
    ## CPUE
    cpue=window(cpue,end=iYr-1)
    cpue[,ac(iYr-(interval:1))]=stock.n(om)[dimnames(cpue)$age,ac(iYr-(interval:1))]%*%uDev[,ac(iYr-(interval:1))]
    
    #### Management Procedure
    tac=hcrSBT1(apply(cpue,c(2,6),mean),catch(om)[,ac(iYr-1)])
    
    #print(tac)
    
    #### Operating Model update
    om =fwd(om,catch=tac,sr=eql,sr.residuals=srDev,maxF=mean(maxF)) 
  }
  cat('==\n')
  
  return(om)}