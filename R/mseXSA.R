mseXSA<-function(#OM as FLStock and FLBRP
                 om,eql,
                  
                 #MP, this could be an XSA, biodyn etc,
                 mp,rf,control,
          
                 #HCR
                 ftar =0.70,
                 btrig=0.80,
                 fmin =0.10, 
                 blim =0.40,
                 
                 #Bounds on TAC changes
                 bndTac=c(0.01,100),
                 
                 #years over which to run MSE, doesnt work if interval==1, this is a bug
                 start=range(om)["maxyear"]-30,interval=3,end=range(om)["maxyear"]-interval,
                  
                 #Stochasticity, either by default or suppliedas args
                 srDev=rlnoise(dim(om)[6],FLQuant(0,dimnames=list(year=start:end)),0.3), 
                 uDev =rlnoise(dim(mp)[6],FLQuant(0,dimnames=dimnames(iter(stock.n(om),1))),0.2),

                 #Capacity, i.e. F in OM can not be greater than this
                 maxF=1.5){ 
  
  
  ##Check last year so you dont run to the end then crash
  end=min(end,range(om)["maxyear"]-interval)
  
  ## Make sure number of iterations in OM are consistent
  nits=c(om=dims(om)$iter, eql=dims(params(eql))$iter, rsdl=dims(srDev)$iter)
  if (length(unique(nits))>=2 & !(1 %in% nits)) ("Stop, iters not '1 or n' in om")
  if (nits['om']==1) stock(om)=propagate(stock(om),max(nits))
  
  ## Limit on capacity, add to fwd(om,maxF=maxF) so catches dont go stuoid 
  maxF=FLQuant(1,dimnames=dimnames(srDev))%*%apply(fbar(window(om,end=start)),6,max)*maxF

  ## Observation Error (OEM) setup before looping through years 
  pGrp=range(mp)["plusgroup"]
  
  smp =setPlusGroup(om,pGrp)
  cpue=window(stock.n(smp),end=start-1)[seq(dim(mp)[1]-1)]
  cpue=cpue%*%uDev[dimnames(cpue)$age,dimnames(cpue)$year]
  
  ## MP, no need to add biological parameters and catch at this stage, as these are already there, 
  ## rather get rid of stuff that has to be added by OEM and stock assessment fit
  mp=window(mp,end=start-1)
  
  ## Loop round years
  cat('\n==')
  for (iYr in seq(start,end,interval)){
    cat(iYr,", ",sep="")
   
    ## Observation Error, using data from last year back to the last assessment
    smp =setPlusGroup(om[,ac(rev(iYr-seq(interval)))],pGrp)
      
    ## CPUE
    cpue=window(cpue,end=iYr-1)
    cpue[,ac(iYr-(interval:1))]=stock.n(smp)[dimnames(cpue)$age,ac(iYr-(interval:1))]%*%
      uDev[dimnames(cpue)$age,ac(iYr-(interval:1))]
       
    ## Update and fill in biological parameters
    if (iYr!=start)
      mp=fwdWindow(mp,end=iYr-1,rf)
    else  
      mp=window(mp,end=iYr-1)

    ## Add catches and create plus group 
    mp.=setPlusGroup(om[,ac(iYr-rev(seq(interval)))],pGrp)
    
    ## Should really do landings and discards
    landings(   mp[,ac(iYr-(interval:1))])=landings(   mp.)
    landings.n( mp[,ac(iYr-(interval:1))])=landings.n( mp.)
    landings.wt(mp[,ac(iYr-(interval:1))])=landings.wt(mp.)
    discards(   mp[,ac(iYr-(interval:1))])=discards(   mp.)
    discards.n( mp[,ac(iYr-(interval:1))])=discards.n( mp.)
    discards.wt(mp[,ac(iYr-(interval:1))])=discards.wt(mp.)
    catch(      mp[,ac(iYr-(interval:1))])=catch(      mp.)
    catch.n(    mp[,ac(iYr-(interval:1))])=catch.n(    mp.)
    catch.wt(   mp[,ac(iYr-(interval:1))])=catch.wt(   mp.)
    stock.wt(   mp[,ac(iYr-(interval:1))])=stock.wt(   mp.)
    
    #### Management Procedure
    ## fit
    idx=FLIndex(index=cpue)
    range(idx)[c("startf","endf")]=c(0.01,0.1)
    
    ## Bug with adding range
    xsa=FLXSA(mp,idx,control=control,diag.flag=FALSE)
    range(xsa)[c("min","max","plusgroup")]=range(mp)[c("min","max","plusgroup")]
    mp=mp+xsa
    
    #plot(FLStocks(mp=mp,om=om[,ac(1:(iYr-1))]))

    ## Stock recruiment relationship
    sr=fmle(as.FLSR(mp,model="bevholt"),control=list(silent=TRUE))
    
    ## Reference points
    rf=brp(FLBRP(mp,sr=sr))
    
    ## in year update
    mp=fwdWindow(mp,end=iYr,rf)
    mp=fwd(mp,catch=catch(om)[,ac(iYr)],sr=rf,maxF=mean(maxF))
    
    ## HCR
    hcrPar=hcrParam(ftar =ftar*fmsy(rf),
                    btrig=btrig*bmsy(rf),
                    fmin =fmin*fmsy(rf), 
                    blim =blim*bmsy(rf))
    tac=hcr(mp,rf,hcrPar,
              hcrYrs=iYr+seq(interval),
              bndTac=bndTac,
              tac =TRUE)
    
    #print(tac)
      
    #### Operating Model update
    om =fwd(om,catch=tac,sr=eql,sr.residuals=srDev,maxF=mean(maxF)) 
  }
  cat("==\n")
  
  return(om)}