
# Accompanies the book:
#   Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition: 
#   A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.

source("DBDA2E-utilities.R")

#some functions to analysis
logit<-function(x){log(x/(1-x))}
sigmoid<-function(x){exp(x)/(exp(x)+1)}


#===============================================================================

genMCMC = function( data , xName="x" , yName="y" , sName="s", uName="u", vName="v",
                    numSavedSteps=10000 , thinSteps=1 , saveName=NULL ,
                    runjagsMethod=runjagsMethodDefault , select_model, 
                    nChains=nChainsDefault) { 
  require(runjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  phi_sample = data[,yName]
  x = as.matrix(data[,xName],ncol=length(xName))
  s = as.matrix(data[,sName],ncol=length(sName))
  u = as.matrix(data[,uName],ncol=length(uName))
  v = as.matrix(data[,vName],ncol=length(vName))
  # Do some checking that data make sense:
  if ( any( y!=0 & y!=1 ) ) { stop("All y values must be 0 or 1.") }
  Ntotal = length(phi_sample)
  Nsubj = length(unique(s))
  # cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
  #show( round(cor(x),3) )
  #cat("\n")
  flush.console()
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    delta1 = x[,1] ,
    delta2 = x[,2],
    phi_sample = phi_sample,
    s = s,
    Ntotal = Ntotal ,
    Nsubj = Nsubj,
    v=v,
    u=u,
    R=100*diag(2)
  )
  
  
  #-----------------------------------------------------------------------------
  # THE MODEL.
  
  modelString = "
   model {

    for ( i in 1:Ntotal ) {
      phi_sample[i] ~ dnorm(phi[s[i,],1] * delta1[i] + phi[s[i,],2] * delta2[i],v[i,]^(-1)*delta1[i]+u[i,]^(-1)*delta2[i])
    }
      
    for ( sIdx in 1:Nsubj ) {
      phi[sIdx,1:2] ~ dmnorm(mu , sigma)
    }

    mu[1] ~ dnorm(0,0.01)
    mu[2] ~ dnorm(0,0.01)
    sigma ~ dwish(R,Nsubj)
    
    
  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="model3.txt" )
  
  #-----------------------------------------------------------------------------
  parameters = c( "phi")
  adaptSteps = 2000  # Number of steps to "tune" the samplers
  burnInSteps = 2000
  runJagsOut <- run.jags( method=runjagsMethod ,
                          model=select_model, 
                          monitor=parameters , 
                          data=dataList ,  
                          #inits=initsList , 
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps , 
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  codaSamples = as.mcmc.list( runJagsOut )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function
 
#===============================================================================
smryMCMC = function(  codaSamples , 
                      saveName=NULL ) {
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples)
  p1_hat = apply(mcmcMat[,1:30],1,function(x) mean(sigmoid(x)))
  p2_hat = apply(mcmcMat[,31:60],1,function(x) mean(sigmoid(x)))
  diff<-mean(mcmcMat[,1:30]-phi1_data)
  mcmcMat = cbind(mcmcMat,p1_hat,p2_hat,diff)
  
  paramName = colnames(mcmcMat)
  for ( pName in paramName ) {
    summaryInfo = rbind( summaryInfo , summarizePost( mcmcMat[,pName] ) )
  }
  rownames(summaryInfo) = paramName
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  return( summaryInfo )
}

#===============================================================================


#===============================================================================
