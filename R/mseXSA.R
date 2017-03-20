mseXSA<-function(
  #OM as FLStock and FLBRP
  om,eql,
  
  #MP, this could be an XSA, biodyn etc,
  mp,rf,control,
  
  #years over which to run MSE, doesnt work if interval==1, this is a bug
  start=range(om)["maxyear"]-30,interval=3,end=range(om)["maxyear"]-interval,
  
  #Stochasticity, either by default or suppliedas args
  srDev=rlnoise(dim(om)[6],FLQuant(0,dimnames=list(year=start:end)),0.3), 
  uDev =rlnoise(dim(mp)[6],FLQuant(0,dimnames=dimnames(iter(stock.n(om),1))),0.2),
  
  #Bounds on TAC changes
  bndTac=c(0.8,1.2),
  
  #Capacity, i.e. F in OM can not be greater than this
  maxF=2.0){ 
  
  ##So you dont run to the end then crash
  end=min(end,range(om)["maxyear"]-interval)
  
  ## Make sure number of iterations are consistent
  nits=c(om=dims(om)$iter, eql=dims(params(eql))$iter, rsdl=dims(srDev)$iter)
  if (length(unique(nits))>=2 & !(1 %in% nits)) ("Stop, iters not '1 or n' in om")
  if (nits['om']==1) stock(om)=propagate(stock(om),max(nits))
  
  ## Limit on capacity, add to fwd(om) if you want
  maxF=FLQuant(1,dimnames=dimnames(srDev))%*%apply(fbar(window(om,end=start)),6,max)*maxF
  
  ## Observation Error (OEM) setup 
  pGrp=range(mp)["plusgroup"]
  
  smp=setPlusGroup(om,pGrp)
  cpue=window(stock.n(smp),end=start)[seq(dim(mp)[1]-1)]
  cpue=cpue%*%uDev[dimnames(cpue)$age,dimnames(cpue)$year]
  
  ## MP, no need to add biological parameters, catch as these are already there, 
  ## rather get rid of stuff that has to be added by OEM and fit
  mp=window(mp,end=start-interval)
  
  ## Loop round years
  for (iYr in seq(start,end,interval)){
    #cat('\n===================', iYr, '===================\n')
    cat('\t', iYr)
    
    ## Observation Error, using data from last year back to the last assessment
    ## CPUE
    cpue=window(cpue,end=iYr-1)
    cpue[,ac(iYr-(interval:1))]=stock.n(om)[dimnames(cpue)$age,ac(iYr-(interval:1))]%*%
                                  uDev[dimnames(cpue)$age,ac(iYr-(interval:1))]
    
    ## Update and fill in biological parameters
    mp=fwdWindow(mp,end=iYr-1,rf)
    
    ## Add catches and create plus group 
    mp.=setPlusGroup(om[,ac(iYr-rev(seq(interval)))],pGrp)
    ## Should really do landings and discards
    catch(   mp[,ac(iYr-(interval:1))])=catch(   mp.)
    catch.n( mp[,ac(iYr-(interval:1))])=catch.n( mp.)
    catch.wt(mp[,ac(iYr-(interval:1))])=catch.wt(mp.)
    stock.wt(mp[,ac(iYr-(interval:1))])=stock.wt(mp.)
    
    #### Management Procedure
    ## fit
    idx=FLIndex(index=cpue)
    range(idx)[c("startf","endf")]=c(0.01,0.1)
    
    ## Bug with adding range
    xsa=FLXSA(mp,idx,control=control,diag.flag=FALSE)
    range(xsa)[c("min","max","plusgroup")]=range(mp)[c("min","max","plusgroup")]
    mp=mp+xsa
    
    ## Stock recruiment relationship
    sr=fmle(as.FLSR(mp,model="bevholt"),control=list(silent=TRUE))
    
    ## Reference points
    rf=brp(FLBRP(mp,sr=sr))
    
    ## in year update
    mp=fwdWindow(mp,end=iYr,rf)
    mp=fwd(mp,catch=catch(om)[,ac(iYr)],sr=rf)
    
    ## HCR
    hcrPar=hcrParam(ftar =0.70*fmsy(rf),
                    btrig=0.80*bmsy(rf),
                    fmin =0.10*fmsy(rf), 
                    blim =0.40*bmsy(rf))
    tac=hcr(mp,hcrPar,
              hcrYrs=iYr+seq(interval),
              bndTac=c(0.85,1.15),
              tac =TRUE)
      
    #### Operating Model update
    om=fwd(om,catch=tac,sr=eql,sr.residuals=srDev,maxF=maxF) 
    mp<<-mp
    rf<<-rf
    om<<-om
    }
  
  return(om)}