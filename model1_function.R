# Jags-Ydich-XmetMulti-Mlogistic.R 
# Accompanies the book:
#   Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition: 
#   A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.

source("DBDA2E-utilities.R")





#some functions to analysis
logit<-function(x){log(x/(1-x))}
sigmoid<-function(x){exp(x)/(exp(x)+1)}




#===============================================================================

genMCMC = function( data , xName="x" , yName="y" , 
                    numSavedSteps=10000 , thinSteps=1 , saveName=NULL ,
                    runjagsMethod=runjagsMethodDefault , select_model, 
                    nChains=nChainsDefault, tau) { 
  require(runjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  y = data[,yName]
  x = as.matrix(data[,xName],ncol=length(xName))
  # Do some checking that data make sense:
  if ( any( !is.finite(y) ) ) { stop("All y values must be finite.") }
  if ( any( !is.finite(x) ) ) { stop("All x values must be finite.") }
 # cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
  #show( round(cor(x),3) )
  #cat("\n")
  flush.console()
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    x = x ,
    y = y,
    tau=tau ,
    Nx = dim(x)[2] ,
    Ntotal = dim(x)[1]
  )
  
 
  
  #-----------------------------------------------------------------------------
  # THE MODEL.
  
  modelString = "
  
 
  model {
  
  

  for ( i in 1:Ntotal ) {
  
  # In JAGS, ilogit is logistic:
  y[i] ~ dbern( ilogit(phi1+sum(theta[1:Nx] * x[i,1:Nx] )) )
  }
  
  # Priors vague on standardized scale:
  
  phi1 ~ dnorm( 0 ,10000^(-1) )  
  
  for ( j in 1:Nx ) {
  theta[j] ~ dnorm( 0 ,tau^(-1) )
  }
  
  
  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="model1.txt" )
  
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Let JAGS do it...
  
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "phi1" ,  "theta")
  adaptSteps = 500  # Number of steps to "tune" the samplers
  burnInSteps = 1000
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
  phi2 = apply(mcmcMat,1,function(x) sum(x))
  mcmcMat = cbind(mcmcMat,phi2)
  paramName = colnames(mcmcMat,phi2)
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
