utils::globalVariables(c('eql','srDev'))
utils::globalVariables('priors')
utils::globalVariables('mou')
utils::globalVariables('rcvPeriod')
utils::globalVariables('priors')
utils::globalVariables('cmdOps')
utils::globalVariables('ftar')
utils::globalVariables('btrig')
utils::globalVariables('fmin')
utils::globalVariables('blim')
utils::globalVariables('FLStock2biodyn')

#' @title mseBiodyn
#' 
#' @description Runs a full MSE using an \code{FLStock} object as the Operating Model and \code{biodyn} as the Mangement Procedure
#'           
#' @aliases mse
#' 
#' @param om an \code{FLStock} object 
#  @param eql an \code{FLBRP} object that holds the biological parameters for use in the projections
#' @param mp an \code{biodyn} object that holds the options for the biomass dynamic assessment model
#' @param range a \code{vector} the starting and end years for the projections, and the interval for running the MP
#' @param srDev  a \code{FLQuant} with recruitment deviates
#' @param uDev an \code{FLQuant} or \code{FLQuants} with CPUE residuals
#' @param ftar a \code{numeric} with target F in HCR
#' @param fmin a \code{numeric} with minimum F in HCR
#' @param blim a \code{numeric} with biomass limit for HCR
#' @param btrig a \code{numeric} with biomass trigger (i.e. when to reduce F) in HCR 
#' @param what a \code{character} that specifies what is to be used for the reference point in the HCR, recycled as required
#' @param mult a \code{logical} that specifies whether quantity in HCR options is a multiplier or probability, recycled as required
#'
#' @return  a list of \code{data.frame}s with performance measures from OM and summaries from MP, if \code{con!=NULL} will
#' also write to a MYSQL database
#'  
#' @export
#' @rdname runMSE
#' 
#' @seealso \code{\link{biodyn}}
#' 
#' @examples
#' \dontrun{
#' library(mpb)
#' library(FLash)
#' library(FLBRP)
#' 
#' load(om)
#' load(eql)
#' 
#' om=mpb::fwdWindow(om,eql,end=2030)
#' om=propagate(om,100)
#' 
#' srDev=FLQuant(0,dimnames=list(year=2000:2030))
#' srDev=rlnorm(100,srDev,0.3)
#' 
#' om=mpb::fwd(om,catch=catch(om)[,ac(2000:2011)],sr=eql,sr.residuals=srDev)
#' 
#' library(popbio)
#' 
#' mp=mpb::FLBRP2biodyn(  eql,"biodyn")
#' mp=mpb::FLStock2biodyn(om, "biodyn")
#' }
mseMPB<-function(
  #OM as FLStock and FLBRP
  om,eql,
  
  #MP, this could be an XSA, biodyn etc,
  mp,
  
  #years over which to run MSE, doesnt work if interval==1, this is a bug
  start=range(om)["maxyear"]-30,interval=3,end=range(om)["maxyear"]-interval,
  
  #Stochasticity, either by default or suppliedas args
  srDev=rlnoise(dim(om)[6],FLQuant(0,dimnames=list(year=start:end)),0.3), 
  uDev =rlnoise(dim(mp)[6],FLQuant(0,dimnames=dimnames(iter(stock.n(om),1))),0.2),
  
  #Bounds on TAC & F variability
  bndTac=c(0.8,1.2),
  bndF  =NULL,     
  
  #bias in CPUE
  omega=1,refB=1,qTrend=0,
  
  #Capacity, i.e. F in OM can not be greater than this
  maxF=2.0){ 

  ## Limit on capacity, add to fwd(om) if you want
  maxF=FLQuant(1,dimnames=dimnames(srDev))%*%apply(fbar(window(om,end=start)),6,max)*maxF
    
  ## Get number of iterations in OM
  nits=c(om=dims(om)$iter, eql=dims(params(eql))$iter, rsdl=dims(srDev)$iter)
  if (length(unique(nits))>=2 & !(1 %in% nits)) ("Stop, iters not '1 or n' in OM")
  if (nits['om']==1) stock(om)=propagate(stock(om),max(nits))
  nits=max(nits)
  
  control=control(mp)
  priors =mp@priors
  
  #### Observation Error (OEM) setup #######################
  cpue=window(stock(om),end=start)
  cpue=cpue%*%uDev[,dimnames(cpue)$year]
  
  ## Loop round years
  for (iYr in seq(start,range(om,'maxyear')-interval,interval)){
      #iYr = seq(start+rcvPeriod,range(om,'maxyear')-interval,interval)[1]
      cat('\n===================', iYr, '===================\n')
      cat(iYr, '\t')
    
      ## use data from last year
      cpue=window(cpue,end=iYr-1)
      cpue[,ac(iYr-(interval:1))]=stock.n(om)[dimnames(cpue)$age,ac(iYr-(interval:1))]%*%uDev[,ac(iYr-(interval:1))]
      
      u=apply(cpue,c(2,6),sum)
      
      #### Management Procedure
      ## Set up assessment parameter options
      bd=as(window(om,end=iYr-1),"biodyn")
      params(bd)=propagate(params(bd),nits)
      pnms=dimnames(control)$param[dimnames(control)$param%in%dimnames(params(bd))$params]
      params(bd)[pnms]=c(control[pnms,'val'])
      
      bd@priors=priors
      setParams( bd)=u 
      setControl(bd)=params(bd)
      bd@control[dimnames(control)$params,'phase'][]=control[dimnames(control)$params,'phase']
      #bd@control['q1','phase']=phaseQ
      bd@control['q1','val']  =1
      
      ## fit
      bd =fit(bd,u)
      bd =mpb::fwd(bd,catch=catch(om)[,ac(iYr)])
      
      ## HCR
      hcrPar=hcrParam(ftar =0.70*fmsy(bd),
                      btrig=0.80*bmsy(bd),
                      fmin =0.10*fmsy(bd), 
                      blim =0.40*bmsy(bd))
      hcrOutcome=hcr(bd,hcrPar,
                     hcrYrs=iYr+seq(interval),
                     bndF  =bndF,
                     bndTac=bndTac,
                     tac =TRUE)
      
      ## TACs for next year (iYtr+1) for n=interval years
      TAC  =hcrOutcome$tac
      TAC[]=rep(apply(TAC,6,mean)[drop=T],each=interval)
      
      #### Operating Model Projectionfor TAC
      om =mpb::fwd(om,catch=TAC,maxF=maxF,sr=eql,sr.residuals=srDev)  
      
      #### Summary Statistics
      ## HCR actions, i.e. is biomass<Btrig?, what is F?, ..
      hcr =rbind(hcr,data.frame(yearHcr=min(as.numeric(dimnames(hcrOutcome$hvt)$year)),
                                #yearAss=rep(range(bd)[2],dims(bd)$iter),
                                model.frame(           hcrPar,drop=T)[,-5],
                                tac    =as.data.frame(apply(hcrOutcome$tac,6,mean),drop=T)[,'data'],
                                harvest=as.data.frame(apply(hcrOutcome$hvt,6,mean),drop=T)[,'data'],
                                stock  =as.data.frame(hcrOutcome$stock,drop=T)[,2]))
      
      ## Assessment parameters and reference points
      mp =rbind(mp,cbind(cbind(year=iYr,model.frame(params(bd))),
                         model.frame(refpts(bd))[,-4],
                         hcr))
    }
    
  return(om)}

if (FALSE){
  library(FLCore)
  library(FLash)
  library(FLBRP)
  library(ggplotFL)
  library(FLXSA)
  library(FLife)
  library(mpb)
  library(plyr)
  
  source('~/Desktop/flr/mse/R/msy.R')
  source('~/Desktop/flr/mse/R/hcr.R')
  
  ##Use Life history to generate a stock
  data(teleost)
  
  alb=lhPar(teleost[,"Thunnus alalunga"])
  alb["sl"]=1
  
  nits=100
  mFn<-function(age,...) FLQuant(0.2,dimnames=dimnames(age))
  
  ##OM
  eql=lhEql(alb, range=c(min=0,max=10,plusgroup=10,minfbar=5,maxfbar=9))
  fbar(eql)=FLBRP:::refpts(eql)["msy","harvest"]*FLQuant(c(rep(.1,19),
                                                           seq(.1,2,length.out=40),
                                                           seq(2,.7,length.out=11)[-1],
                                                           rep(.7,61)))
  srDev=rlnorm(nits,fbar(eql)[,-1,,,,1]*0,.3)
  om=propagate(fwd(eql),nits)
  om=fwd(om,f=fbar(om)[,-1],sr.residuals=rlnorm(nits,fbar(om)[,-1,,,,1]*0,.3),sr=eql)

  mp=as(om,"biodyn")
  params(mp)["p"]=0.00000001
  
  plotProduction(mp)+theme(legend.position = "none")
  
  uDev=rlnorm(nits,stock.n(om)[,,,,,1]*0,.2)
  cpue=apply(stock.n(om)%*%uDev,c(2,6),sum)
 
  setParams( mp)<-cpue  
  setControl(mp)<-params(mp)  
  
  mp=fit(mp,cpue)
  
  }
  