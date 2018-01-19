#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
library(MASS)
library(Deriv)
library(binom)
library(ggplot2)
source("model2_function.R")
source("simple sample.R")
p1_0<-NULL; p2_0<-NULL;

fileNameRoot = "model2-" 
numSavedSteps=15000 ; thinSteps=2

z1<-logit(table(myData.list[[5]][1:3000,1:2])[2,]*0.01)
z2<-logit(table(myData.list[[7]][3001:33000,1:2])[2,]*0.001)
sd(z1);sd(z2)

yName = "y" ;sName = "s" ; xName=c("delta1","delta2") # column name for subject ID
#w1<-g1/sum(g1)
#w2<-g2/sum(g2)

myData.list<-list()
for(i in 1:length(times_number)){
  myData.list[[i]]<-read.table(paste(p2_set[i], "simul_data.txt"),sep="@")
}

p2<-NULL; p1<-NULL; p1_low<-NULL ; p1_high<-NULL; p2_low<-NULL; p2_high<-NULL
for(i in 1:length(p2_set)){
  myData<-myData.list[[i]]
  
  p1_0[i]<-mean(myData[1:sum(g1),1])
  p2_0[i]<-mean(myData[(sum(g1)+1):sum(g1+g2),1])

  #.............................................................................
  #graphFileType = "eps" 
  #------------------------------------------------------------------------------- 
  # Load the relevant model into R's working memory:
  
  #------------------------------------------------------------------------------- 
  # Generate the MCMC chain:
  #startTime = proc.time()
  mcmcCoda = genMCMC( data=myData , xName=xName , yName=yName , sName=sName,
                      numSavedSteps=numSavedSteps , thinSteps=thinSteps , select_model="model2.txt",
                      saveName=paste(fileNameRoot))


  #stopTime = proc.time()
  #duration = stopTime - startTime
  #show(duration)
  #------------------------------------------------------------------------------- 
  #------------------------------------------------------------------------------- 
  # Get summary statistics of chain:
  summaryInfo = smryMCMC( mcmcCoda , 
                          saveName=fileNameRoot )
  #show(summaryInfo)
  
  p2[i]<-summaryInfo[62,1]
  p1[i]<-summaryInfo[61,1]
  p1_low[i]<-summaryInfo[61,6]
  p1_high[i]<-summaryInfo[61,7]
  p2_low[i]<-summaryInfo[62,6]
  p2_high[i]<-summaryInfo[62,7]
  rho[i]<-summaryInfo[63]
  print(i)
  
  #------------------------------------------------------------------------------- 
}

  
save.image(file ="archive_model2.RData")

phi1_hat<-as.numeric(summaryInfo[1:30,1])
phi2_hat<-as.numeric(summaryInfo[31:60,1])
p1_hat<-sigmoid(as.numeric(summaryInfo[1:30,1]))
p2_hat<-sigmoid(as.numeric(summaryInfo[31:60,1]))


p1_0-summaryInfo[61,1]
p1_0-sum(w1*sigmoid(phi1_hat))

p1_weighted<-sum(p1_hat*g1)/sum(g1)



idx=0
p1_0_low<-c(); p1_0_high<-c(); 
for(i in g1){
  idx<-idx+i
  p1_0_low<-c(p1_0_low,as.numeric(binom.logit(sum(y1[(idx-i+1):idx]),i)[5]))
  p1_0_high<-c(p1_0_high,as.numeric(binom.logit(sum(y1[(idx-i+1):idx]),i)[6]))
}

idx=0
p2_0_low<-c(); p2_0_high<-c(); 
for(i in g2){
  idx<-idx+i
  p2_0_low<-c(p2_0_low,as.numeric(binom.logit(sum(y2[(idx-i+1):idx]),i)[5]))
  p2_0_high<-c(p2_0_high,as.numeric(binom.logit(sum(y2[(idx-i+1):idx]),i)[6]))
  
}




df_p1_data <- data.frame(x=1:30, y=p1, L =p1_0_low, U =p1_0_high)
df_p2_data <- data.frame(x=1:30, y=p2, L =p2_0_low, U =p2_0_high)


label_p1_data=paste(round(p1_data,3))
label_p2_data=paste(round(p2_data,3))


ggplot(df_p1_data,aes(x=x,y=y))+
  geom_point(aes(y=y, x=x+0.5),color="Red")+
  geom_errorbar(aes(x=1:30+0.5, ymax = p1_0_high, ymin = p1_0_low),color="pink")+
  geom_errorbar(aes(ymax = sigmoid(as.numeric(summaryInfo[1:30,7])), ymin = sigmoid(as.numeric(summaryInfo[1:30,6]))),color="blue", lwd=0.7)+
  geom_point(aes(y=p1_hat),size = 4) +
  geom_text(aes(label=label_p1_data),hjust=0, vjust=-1)



ggplot(df_p2_data,aes(x=x,y=y))+
  geom_point(aes(y=y, x=x+0.5),color="Red")+
  geom_errorbar(aes(x=1:30+0.5, ymax = p2_0_high, ymin = p2_0_low),color="pink")+
  geom_errorbar(aes(ymax = sigmoid(as.numeric(summaryInfo[31:60,7])), ymin = sigmoid(as.numeric(summaryInfo[31:60,6]))),color="skyblue", lwd=0.7)+
  geom_point(aes(y=p2_hat),size = 3, fill="white", shape=21) +
  geom_text(aes(label=label_p2_data),hjust=0, vjust=-1)





save.image(file ="archive_model2.RData")

load("archive_model2.RData")

