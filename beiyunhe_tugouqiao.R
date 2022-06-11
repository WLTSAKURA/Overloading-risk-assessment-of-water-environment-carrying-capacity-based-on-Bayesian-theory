###################K
setwd("Downloads/myworkspace/RESULTS_beiyunhe")
p_K = read.table(file = "yulinzhuang_K(0.01-0.8).txt")
p_K = p_K[[1]]
K<-seq(0.01,0.8,by=0.001)
plot(K, p_K, type = 'l')

#AcceptReject 抽取K
N = 80    ####抽取多少???
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
flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/tugouqiao.xlsx", range = "C1:C37", sheet = 'Sheet1')
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

l1 = 9683.9
l2 = 15993.2
u_a = 0.1142
u_b = 0.5601

sample_T1 = l1/(u_a*sample_Q^u_b)/3600/24
sample_T2 = l2/(u_a*sample_Q^u_b)/3600/24

###################C
#C_chushi <- read_excel("Downloads/myworkspace/COD_beiyunhe/tugouqiao.xlsx", range = "B1:B169", sheet = 'Sheet1')
#C_chushi = C_chushi[[1]]
#library(fitdistrplus)
#fitdist(C_chushi,'gamma')

sample_C = rgamma(50,shape=2.2836536 ,rate=0.0541245)    ####抽取多少???
#sample_C = rgamma(50,shape=1.8157726, rate=0.1835928)  #NH3N
#sample_C = rgamma(50,shape=4.598363, rate=2.975761)  #TP

##############vvvvvvv W vvvvvv
Q_pai = 10.8876704
#Cs = 40 #COD
Cs=2  #NH3N   #TP 0.4
#W = seq(470,580,by=1)  #COD
W = seq(25,27.4,by=0.1)  #NH3N
W = seq(4.98,5.5,by=0.01)  #TP

#####决定sd取多少！！
#plot(W,pnorm(W,mean=453.8,sd=70),type = "l")
#points(453.8,0.5,col="red",pch=20,cex=c(2,4))
#points(443.9,0.4,col="red",pch=20,cex=c(2,4))
#points(409.7,0.3,col="red",pch=20,cex=c(2,4))
#points(386.8,0.2,col="red",pch=20,cex=c(2,4))
#points(370.5,0.1,col="red",pch=20,cex=c(2,4))


#p_W_prior = dnorm(W,mean=524.1,sd=13)  #COD
p_W_prior = dnorm(W,mean=26.2,sd=0.5)  #NH3N
p_W_prior = dnorm(W,mean=5.24,sd=0.09)  #TP

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
#sum(W*p_W)
#descdist(p_W)
#write.table(p_W,file = "tugouqiao_W.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)
#write.table(W,file = "tugouqiao_Wx.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)
write.table(p_W,file = "pastethis_p_W.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)

skewness(p_W)
kurtosis(p_W)
sum(W*p_W)

p_cdf=c()
for(i in 1:length(p_W)){
  p_cdf = c(p_cdf, sum(p_W[1:i]))
}
write.table(p_cdf,file = "pastethis_p_cdf.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)

plot(W,p_cdf,type='l')

'''
p2 = c()
for(i in 1:length(p_W)){
  p2 = c(p2, sum(p_W[1:i]))
}
plot(W,p2,type='l')
write.table(p2, file = "tugouqiao_cdf.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)
'''

##############^^^^^^^ W ^^^^^^^
