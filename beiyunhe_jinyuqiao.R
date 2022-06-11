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
flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/jinyuqiao.xlsx", range = "C1:C109", sheet = 'Sheet1')
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

l1 = 26406.3
l2 = 18693.7
u_a = 0.1142
u_b = 0.5601

sample_T1 = l1/(u_a*sample_Q^u_b)/3600/24
sample_T2 = l2/(u_a*sample_Q^u_b)/3600/24

###################C
#C_chushi <- read_excel("M:/myworkspace/COD_beiyunhe/jinyuqiao.xlsx", range = "B1:B169", sheet = 'Sheet1')
#C_chushi = C_chushi[[1]]
#library(fitdistrplus)
#fitdist(C_chushi,'gamma')

sample_C = rgamma(50,shape=2.18798363, rate=0.04558722)    ####抽取多少???
#sample_C = rgamma(50,shape=1.210509, rate=1.434117)   #NH3N
#sample_C = rgamma(50,shape=1.621610, rate=2.991477)   #TP



##############vvvvvvv W vvvvvv
Q_pai = 27.65759654
Cs = 40  #NH3N 2; TP 0.4
W = seq(1370,1550,by=1)  #COD
W = seq(58.5,60.7,by=0.1)  #NH3N
W = seq(13.5,15.5,by=0.01)  #TP



p_W_prior = dnorm(W,mean=1446,sd=30)
p_W_prior = dnorm(W,mean=59.6,sd=0.5)  #NH3N
p_W_prior = dnorm(W,mean=14.5,sd=0.4)  #TP


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
