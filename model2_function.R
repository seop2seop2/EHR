
# Accompanies the book:
#   Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition: 
#   A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.

source("DBDA2E-utilities.R")

#some functions to analysis
logit<-function(x){log(x/(1-x))}
sigmoid<-function(x){exp(x)/(exp(x)+1)}


#===============================================================================

genMCMC = function( data , xName="x" , yName="y" , sName="s",
                    numSavedSteps=10000 , thinSteps=1 , saveName=NULL ,
                    runjagsMethod=runjagsMethodDefault , select_model, 
                    nChains=nChainsDefault) { 
  require(runjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  y = data[,yName]
  x = as.matrix(data[,xName],ncol=length(xName))
  s = as.matrix(data[,sName],ncol=length(sName))
  # Do some checking that data make sense:
  if ( any( y!=0 & y!=1 ) ) { stop("All y values must be 0 or 1.") }
  N_y1=sum(data[,xName[1]]==1)
  Ntotal = length(y)
  Nsubj = length(unique(s))
  # cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
  #show( round(cor(x),3) )
  #cat("\n")
  flush.console()
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    delta1 = x[,1] ,
    delta2 = x[,2],
    y = y,
    s = s,
    Ntotal = Ntotal ,
    Nsubj = Nsubj,
    R=100*diag(dim(x)[2])
  )
  
  
  #-----------------------------------------------------------------------------
  # THE MODEL.
  
  modelString = "
   model {

    for ( i in 1:Ntotal ) {
      y[i] ~ dbern( ilogit( phi[s[i,],1] * delta1[i] + phi[s[i,],2] * delta2[i] ) )
      
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
  writeLines( modelString , con="model2.txt" )
  
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
  mcmcMat = as.matrix(mcmcCoda)
  phi1_hat = apply(mcmcMat[,1:30],1,function(x) mean(sigmoid(x)))
  phi2_hat = apply(mcmcMat[,31:60],1,function(x) mean(sigmoid(x)))
  rho = apply(mcmcMat,1, function(x) cor(x[1:30],x[31:60]))
  mcmcMat = cbind(mcmcMat,phi1_hat,phi2_hat,rho)
  
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