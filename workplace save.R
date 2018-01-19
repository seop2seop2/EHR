
library(ggplot2)

load("archive_model1.Rdata")
model1<-data.frame(p1=p1, p2=p2, L1=p1_low, U1=p1_high, L2=p2_low, U2=p2_high, p1_0=p1_0, p2_0=p2_0)
load("archive_model2.RData")
model2<-data.frame(p1=p1, p2=p2, L1=p1_low, U1=p1_high, L2=p2_low, U2=p2_high, p1_0=p1_0, p2_0=p2_0)
model2_last<-summaryInfo
load("archive_model3.RData")
model3<-data.frame(p1=p1, p2=p2, L1=p1_low, U1=p1_high, L2=p2_low, U2=p2_high, p1_0=p1_0, p2_0=p2_0)
model3_last<-summaryInfo
#load("archive_model4.RData")
#model4<-data.frame(p1=p1, p2=p2, L1=p1_low, U1=p1_high, L2=p2_low, U2=p2_high, p1_0=p1_0, p2_0=p2_0)
#model4_last<-summaryInfo

p1_0_low<-0;p2_0_low<-0;p1_0_upper<-0;p2_0_upper<-0;
for(i in 1:length(p2_set)){
  myData<-myData.list[[i]]
  y=myData[,1]
  p1_0_low[i]<-as.numeric(binom.logit(sum(y[1:n1]),n1)[5])
  p1_0_upper[i]<-as.numeric(binom.logit(sum(y[1:n1]),n1)[6])
  p2_0_low[i]<-as.numeric(binom.logit(sum(y[(n1+1):(n1+n2)]),n2)[5])
  p2_0_upper[i]<-as.numeric(binom.logit(sum(y[(n1+1):(n1+n2)]),n2)[6])
}

ggplot(model1,aes(y=p1))+
  geom_point(aes(y=p1, x=1:7),color="Red")+
  #geom_point(aes(y=p2, x=1:7),color="orange")+
  geom_point(aes(y=model2$p1, x=1:7+0.1),color="blue")+
  geom_point(aes(y=model3$p1, x=1:7+0.2),color="green")+
  #geom_point(aes(y=model4$p1, x=1:7+0.3),color="purple")+
  geom_errorbar(aes(x=1:7, ymax = U1, ymin = L1),color="red")+
  geom_errorbar(aes(x=1:7+0.1, ymax = model2$U1, ymin = model2$L1),color="blue")+
  geom_errorbar(aes(x=1:7+0.2, ymax = model3$U1, ymin = model3$L1),color="green")+
  #geom_errorbar(aes(x=1:7, ymax = p1_0_upper, ymin = p1_0_low),color="black")+
  scale_x_continuous(breaks=1:7, labels=as.character(round(p2_0,3)))+
  xlab("p2")+
  geom_hline(yintercept = p1_0[1],lwd=1)+
  geom_hline(yintercept = p1_0_low)+
  geom_hline(yintercept = p1_0_upper)

  #geom_errorbar(aes(x=1:7, ymax = p2_0_upper, ymin = p2_0_low),color="orange")+
  #geom_errorbar(aes(x=1:7+0.3, ymax = model4$U1, ymin = model4$L1),color="purple")+

  #geom_hline(yintercept = p1_0[1],lwd=1)
  #geom_errorbar(aes(x=1:7+0.3, ymax = p1_0_upper, ymin = p1_0_low))+
  #geom_point(aes(y=p1_0, x=1:7+0.3))
  
 
 # geom_errorbar(aes(ymax = sigmoid(as.numeric(summaryInfo[1:30,7])), ymin = sigmoid(as.numeric(summaryInfo[1:30,6]))),color="blue", lwd=0.7)+
  #geom_point(aes(y=p1_hat),size = 4) +
  #geom_text(aes(label=label_p1_data),hjust=0, vjust=-1)

