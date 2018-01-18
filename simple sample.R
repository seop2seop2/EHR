library(MASS)
set.seed(2123)


#some functions to analysis
logit<-function(x){log(x/(1-x))}
sigmoid<-function(x){exp(x)/(exp(x)+1)}

#generating sumulation data
n1=100
n2=1000


#number of samples by group
#number of samples by group
#g1<-round(rnorm(30, n1, 5))
#g2<-round(rnorm(30, n2, 50))
g1<-rep(100,30)
g2<-rep(1000,30)

#assign group
s1<-rep(1:30,g1)
s2<-rep(1:30,g2)

rho=0.8

p1_0=0.2
sigma1<-sqrt(1/(30*p1_0*(1-p1_0)))

times_number<-c(1,1.05,1.1,1.15,1.25,1.5,2)
p2_set<-0.2*times_number

phi1_g<-rnorm(30,logit(p1_0),sigma1)

N1=sum(g1)
N2=sum(g2)

#assign type(nhis, ehr)
delta1<-c(rep(1,N1),rep(0,N2))
delta2<-c(rep(0,N1),rep(1,N2))


y1<-c(); zz<-c()
for(i in 1:30){
  y1<-c(y1,rbinom(g1[i],1,sigmoid(phi1_g)[i]))
}

for(k in 1:length(times_number)){
  
  sigma2<-sqrt(1/(30*p2_set[k]*(1-p2_set[k])))
  
  #Generate phi2
  phi2_g<-NULL
  for(i in 1:30){ phi2_g[i]<-rnorm(1,logit(p2_set[k])+rho*(sigma2/sigma1)*(phi1_g[i]-logit(p1_0)),sqrt(sigma2^2*(1-rho^2))) }
  
  
  y2<-c()
  for(i in 1:30){ y2<-c(y2,rbinom(g2[i],1,sigmoid(phi2_g)[i]))}
  zz[k]<-cor(phi1_g,phi2_g)  
  
  myData = data.frame(y=c(y1,y2), s=c(s1,s2), delta1=delta1, delta2=delta2)
  
  write.table(myData, file=paste(p2_set[k], "simul_data.txt"), sep="@")
  
}

myData<-list()
for(i in 1:length(times_number)){
  myData[[i]]<-read.table(paste(p2_set[i], "simul_data.txt"),sep="@")
}

#confirm simulation
#diff<-NULL; p1_0<-NULL; p2_0<-NULL; 
#for(i in 1:7){
#  diff[i]<-mean(myData[[i]][1:sum(g1),1])-mean(myData[[i]][(sum(g1)+1):sum(g1+g2),1])
#  p1_0[i]<-mean(myData[[i]][1:sum(g1),1])
#  p2_0[i]<-mean(myData[[i]][(sum(g1)+1):sum(g1+g2),1])
#}

#plot(abs(diff))
#p1_0
#p2_0
#p2_0/p1_0