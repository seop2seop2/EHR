#------------------------------------------------------------------------------- 
# Optional generic preliminaries:

graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
library(MASS)
library(Deriv)
library(binom)
source("model3_function.R")
source("simple sample.R")
p1_0<-NULL; p2_0<-NULL;

fileNameRoot = "model3-" 
numSavedSteps=15000 ; thinSteps=2


yName = c("phi_data") ;sName = "s" ; xName=c("delta1","delta2"); uName="u"; vName="v"
#w1<-g1/sum(g1)
#w2<-g2/sum(g2)
n1=100
n2=1000

myData.list<-list()
for(i in 1:length(times_number)){
  myData.list[[i]]<-read.table(paste(p2_set[i], "simul_data.txt"),sep="@")
}

p2<-NULL; p1<-NULL; p1_low<-NULL ; p1_high<-NULL; p2_low<-NULL; p2_high<-NULL; diff<-NULL
for(i in 1:length(p2_set)){
  myData<-myData.list[[i]]
  
  
  y<-myData[,1]
  
  n1<-sum(myData[,3])
  n2<-nrow(myData)-n1
  
  y1<-myData[1:n1,1]
  y2<-myData[(n1+1):(n1+n2),1]
  
  p1_0[i]<-mean(myData[1:sum(g1),1])
  p2_0[i]<-mean(myData[(sum(g1)+1):sum(g1+g2),1])
  
  p1_data<-as.numeric(table(y1,s1)[2,])/g1
  p2_data<-as.numeric(table(y2,s2)[2,])/g2
  
  phi1_data<-logit(p1_data)
  phi2_data<-logit(p2_data)
  
  phi_data<-cbind(c(phi1_data,phi2_data))
  
  v.square=1/(100*p1_data*(1-p1_data))
  u.square=1/(1000*p2_data*(1-p2_data))
  
  model3_data=data.frame(phi_data=c(phi1_data,phi2_data), v=v.square, 
                         u=u.square,s=1:30,delta1=c(rep(1,30),rep(0,30)),delta2=c(rep(0,30),rep(1,30)))
  #.............................................................................
  #graphFileType = "eps" 
  #------------------------------------------------------------------------------- 
  # Load the relevant model into R's working memory:
  
  #------------------------------------------------------------------------------- 
  # Generate the MCMC chain:
  #startTime = proc.time()
  mcmcCoda = genMCMC( data=model3_data , yName=yName , uName=uName, vName=vName, xName=xName,
                      numSavedSteps=numSavedSteps , thinSteps=thinSteps , select_model="model3.txt",
                      saveName=paste(fileNameRoot))
  #stopTime = proc.time()
  #duration = stopTime - startTime
  #show(duration)
  #------------------------------------------------------------------------------- 
  # Get summary statistics of chain:
  summaryInfo = smryMCMC( mcmcCoda , 
                          saveName=fileNameRoot )
  #show(summaryInfo)
  
  #------------------------------------------------------------------------------- 
  
  p2[i]<-summaryInfo[62,1]
  p1[i]<-summaryInfo[61,1]
  p1_low[i]<-summaryInfo[61,6]
  p1_high[i]<-summaryInfo[61,7]
  p2_low[i]<-summaryInfo[62,6]
  p2_high[i]<-summaryInfo[62,7]
  diff[i]<-summaryInfo[63,1]
  print(i)

#------------------------------------------------------------------------------- 
}

phi1_hat<-as.numeric(summaryInfo[1:30,1])
phi2_hat<-as.numeric(summaryInfo[31:60,1])
p1_hat<-sigmoid(as.numeric(summaryInfo[1:30,1]))
p2_hat<-sigmoid(as.numeric(summaryInfo[31:60,1]))


save.image(file ="archive_model3.RData")



idx=0
p1_data<-c(); p1_0_low<-c(); p1_0_high<-c(); 
for(i in g1){
  idx<-idx+i
  p1_data<-c(p1_data,as.numeric(binom.logit(sum(y1[(idx-i+1):idx]),i)[4]))
  p1_0_low<-c(p1_0_low,as.numeric(binom.logit(sum(y1[(idx-i+1):idx]),i)[5]))
  p1_0_high<-c(p1_0_high,as.numeric(binom.logit(sum(y1[(idx-i+1):idx]),i)[6]))
}

idx=0
p2_data<-c(); p2_0_low<-c(); p2_0_high<-c(); 
for(i in g2){
  idx<-idx+i
  p2_data<-c(p2_data,as.numeric(binom.logit(sum(y2[(idx-i+1):idx]),i)[4]))
  p2_0_low<-c(p2_0_low,as.numeric(binom.logit(sum(y2[(idx-i+1):idx]),i)[5]))
  p2_0_high<-c(p2_0_high,as.numeric(binom.logit(sum(y2[(idx-i+1):idx]),i)[6]))
  
}





df_p1_data <- data.frame(x=1:30, y=p1_data, L =p1_0_low, U =p1_0_high)
df_p2_data <- data.frame(x=1:30, y=p2_data, L =p2_0_low, U =p2_0_high)


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


save.image(file ="archive_model3.RData")


load("archive model3.RData")
