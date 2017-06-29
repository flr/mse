
mseMPB<-function(
          #OM
          om,eql,
                  
          #MP
          mp,
          ftar =0.7,btrig=0.8,fmin=0.1,blim=0.4,        
          #years over which to run MSE
          start=range(om)["maxyear"]-30,interval=3,end=range(om)["maxyear"]-interval,
                  
          #Stochasticity
          srDev=rlnorm(dim(om)[6],FLQuant(0,dimnames=list(year=start:end)),0.3), 
          uDev =rlnorm(dim(om)[6],FLQuant(0,dimnames=dimnames(iter(stock(om),1))),0.3),
                  
          #Capacity, i.e. F in OM can not be greater than this
          maxF=1.5){ 

  ## Get number of iterations in OM
  nits=c(om=dims(om)$iter, eql=dims(params(eql))$iter, rsdl=dims(srDev)$iter)
  if (length(unique(nits))>=2 & !(1 %in% nits)) ("Stop, iters not '1 or n' in om")
  if (nits['om']==1) stock(om)=propagate(stock(om),max(nits))
  
  mp=window(mp,end=start)
  mp=fwd(mp,catch=catch(mp))
  mp@stock=window(stock(mp),end=70)

  ## Cut in capacity
  maxF=FLQuant(1,dimnames=dimnames(srDev))%*%apply(fbar(window(om,end=start)),6,max)*maxF
  maxF.<<-maxF
  #### Observation Error (OEM) setup 
  cpue=window(catch.n(om)%*%catch.wt(om)%/%fbar(om),end=start)[dimnames(uDev)$age,]
  cpue=cpue%*%uDev[,dimnames(cpue)$year]
  
  ## Loop round years
  for (iYr in seq(start,end-interval,interval)){
    cat('\n===================', iYr, '===================\n')
    
    ##OEM
    mp=window(mp,end=iYr-1)
    #bug in window
    catch(mp)[,ac(rev(iYr-seq(interval+1)))]=catch(om)[,ac(rev(iYr-seq(interval+1)))]
    
#    cpue=window(cpue,end=iYr-1)
#    cpue[,ac(iYr-(interval:1))]=stock(om)[dimnames(cpue)$age,ac(iYr-(interval:1))]%*%uDev[,ac(iYr-(interval:1))]
    cpue=window(cpue,end=iYr-1)[dimnames(uDev)$age]
    cpue[,ac(iYr-(interval:1))]=catch.n( om)[dimnames(uDev)$age,ac(iYr-(interval:1))]%*%
                                catch.wt(om)[dimnames(uDev)$age,ac(iYr-(interval:1))]%/%
                                fbar(    om)[,ac(iYr-(interval:1))]%/%
                                uDev[,ac(iYr-(interval:1))]
    
    #### Management Procedure
    mp@indices=FLQuants("1"=apply(cpue,c(2,6),sum))
    
    ##MP
    mp.<<-mp
    mp=fit(mp)
    mp=window(mp,end=iYr)
    #bug in window
    catch(mp)[,ac(rev(iYr-seq(interval+1)))]=catch(om)[,ac(rev(iYr-seq(interval+1)))]
    catch(mp)[,ac(iYr)]=catch(om)[,ac(iYr)]
    mp=fwd(mp,catch=catch(mp)[,ac(iYr)])
    mp.<<-mp
    
    ## HCR
    tac=hcr(mp,ftar=ftar,btrig=btrig,fmin=fmin,blim=blim,tac=TRUE)
  
    #### Operating Model Projectionfor TAC
    om =fwd(om,catch=tac,sr=eql,sr.residuals=srDev,maxF=mean(maxF))  
    
    om.<<-om
    }
  
  return(om)}

