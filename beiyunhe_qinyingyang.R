###################K
setwd("M:/myworkspace/RESULTS_beiyunhe")
p_K = read.table(file = "yulinzhuang_K.txt")
p_K = p_K[[1]]
K<-seq(0.018,0.8,by=0.001)
plot(K, p_K, type = 'l',xlim=c(0,0.8),ylim=c(0,0.0017))

#AcceptReject 抽取K
N = 200    ####抽取多少???
data=c()
for(i in 1:N){
  u1 = runif(1)
  u2 = runif(1)
  y = floor(length(K)*u1) + 1
  while(u2 > p_K[y]/max(p_K)){
    u1 = runif(1)
    u2 = runif(1)
    y = floor(length(K)*u1) + 1
  }
  data[i]=y
}
sample_K = K[data]
hist(sample_K, freq = FALSE)

###################Q
library(readxl)
flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/S18.xlsx", range = "C1:C102", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
library(moments)
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)

sample_Q = rgamma(100,shape=a, rate=b)+a0   ####抽取多少???
filter = which(sample_Q>0)
sample_Q = sample_Q[filter]

l1 = 38776.6
l2 = 16481.7
u_a = 0.0174
u_b = 0.6696

sample_T1 = l1/(u_a*sample_Q^u_b)/3600/24
sample_T2 = l2/(u_a*sample_Q^u_b)/3600/24

###################C
C_chushi <- read_excel("M:/myworkspace/COD_beiyunhe/qinyingyang.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
library(fitdistrplus)
fitdist(C_chushi,'gamma')

sample_C = rgamma(50,shape=2.16359114, rate=0.04023817)    ####抽取多少???
sample_C = rgamma(50,shape=0.7254547, rate=0.1305836)  #NH3N
sample_C = rgamma(50,shape=1.571684, rate=2.425172)  #TP


##############vvvvvvv W vvvvvv
Q_pai = 0.01832826
Cs = 40 #NH3N 2

W = seq(250,630,by=1)
W = seq(1.3,7.1,by=0.1)  #NH3N
W = seq(0.8,5.5,by=0.1)  #TP


#####决定sd取多少！！
#plot(W,pnorm(W,mean=453.8,sd=70),type = "l")
#points(453.8,0.5,col="red",pch=20,cex=c(2,4))
#points(443.9,0.4,col="red",pch=20,cex=c(2,4))
#points(409.7,0.3,col="red",pch=20,cex=c(2,4))
#points(386.8,0.2,col="red",pch=20,cex=c(2,4))
#points(370.5,0.1,col="red",pch=20,cex=c(2,4))

p_W_prior = dnorm(W,mean=453.8,sd=70)
p_W_prior = dnorm(W,mean=4.2,sd=1.5)   #NH3N
p_W_prior = dnorm(W,mean=3.15,sd=1.2)   #TP


#memory.limit(100000)
ERROR = array(dim = c(length(W), length(sample_K), length(sample_C), length(sample_Q)))   #4????

for (i1 in 1:length(W)) {
  for (i2 in 1:length(sample_K)) {
    for (i3 in 1:length(sample_C)) {
      for (i4 in 1:length(sample_Q)){
        ERROR[i1, i2, i3, i4] = 
          (((sample_Q[i4]*sample_C[i3]*exp(-sample_K[i2]*sample_T1[i4]) + W[i1])/(sample_Q[i4]+Q_pai)*exp(-sample_K[i2]*sample_T2[i4]))-Cs)^2
        
      }
    }
  }
}
#  *p_W_prior[i1]*p_k[i2]*p_C[i3]*p_Q[i4]
LIKELIHOOD = 1/apply(ERROR,1,sum)*p_W_prior
p_W = LIKELIHOOD/sum(LIKELIHOOD)
plot(W, p_W)
#write.table(p_W,file = "fenghe_W_2(10,160).txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)

##############^^^^^^^ W ^^^^^^^
