  #------------------------------------------------------------------------------- 
  # Optional generic preliminaries:
  graphics.off() # This closes all of R's graphics windows.
  rm(list=ls())  # Careful! This clears all of R's memory!
  #------------------------------------------------------------------------------- 
  library(Deriv)
  library(binom)
  source("model1_function.R")
  source("simple sample.R")
  p1_0<-c(); p2_0<-c();
  
  fileNameRoot = "model1(model2_sample)_" 
  numSavedSteps=15000 ; thinSteps=2

  
  yName = "y" ;sName = "s" ; xName=c("delta2") # column name for subject ID
  #w1<-g1/sum(g1)
  #w2<-g2/sum(g2)
  N1=3000
  N2=30000
  
  myData.list<-list()
  for(i in 1:length(times_number)){
    myData.list[[i]]<-read.table(paste(p2_set[i], "simul_data.txt"),sep="@")
  }

  
p2<-NULL; p1<-NULL; p1_low<-NULL ; p1_high<-NULL; p2_low<-NULL; p2_high<-NULL
for(i in 1:length(p2_set)){
  myData<-myData.list[[i]]
    
  p1_0[i]<-mean(myData[1:N1,1])
  p2_0[i]<-mean(myData[(N1+1):(N1+N2),1])
  phi1_hat[i]=logit(p1_0[i])
  phi2_hat[i]=logit(p2_0[i])
    
    #v=sqrt(1/(n1*p1_0*(1-p1_0)))
    #u=sqrt(1/(n2*p2_0*(1-p2_0)))
    
    
    tau0.square[i]<-(phi1_hat[i]-phi2_hat[i])^2#-(u^2+v^2))
  
  #.............................................................................
  #graphFileType = "eps" 
  #------------------------------------------------------------------------------- 
  # Load the relevant model into R's working memory:
  
  #------------------------------------------------------------------------------- 
  # Generate the MCMC chain:
  #startTime = proc.time()
  mcmcCoda = genMCMC( data=myData , xName=xName , yName=yName , 
                      numSavedSteps=numSavedSteps , thinSteps=thinSteps , select_model="model1.txt",
                      saveName=paste(fileNameRoot),tau=tau0.square[i])
  #stopTime = proc.time()
  #duration = stopTime - startTime
  #show(duration)
  #------------------------------------------------------------------------------- 
# -------------------------------------------------------------------------- 
  # Get summary statistics of chain:
  summaryInfo = smryMCMC( mcmcCoda , 
                          saveName=fileNameRoot )
 #------------------------------------------------------------------------------- 
  
    
    p1[i]<-sigmoid(summaryInfo[1,1])
    p2[i]<-sigmoid(summaryInfo[3,1])
    p1_low[i]<-sigmoid(summaryInfo[1,6])
    p1_high[i]<-sigmoid(summaryInfo[1,7])
    p2_low[i]<-sigmoid(summaryInfo[3,6])
    p2_high[i]<-sigmoid(summaryInfo[3,7])
    print(i)
    
    #------------------------------------------------------------------------------- 
}
  
  
  df <- data.frame(x =p2_set, y=p1, L =p1_low, U =p1_high)
  df$p2 <- factor(df$x, levels=as.character(p2_set))
  
  label=paste(round(p1,3))
  
  ggplot(df,aes(x=p2, y =p1)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = U, ymin = L))+
    geom_text(aes(label=label),hjust=0, vjust=-1)
    
  
  save.image("archive_model1.Rdata")
  
  
  