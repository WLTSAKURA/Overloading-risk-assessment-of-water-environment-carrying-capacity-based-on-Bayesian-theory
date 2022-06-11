###################K


###################Q
library(readxl)
flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/fenghe.xlsx", range = "C1:C97", sheet = 'Sheet1')
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

l1 = 30422
l2 = 9786.4
u_a = 0.1364
u_b = 0.5001

sample_T1 = l1/(u_a*sample_Q^u_b)/3600/24
sample_T2 = l2/(u_a*sample_Q^u_b)/3600/24

###################C
#C_chushi <- read_excel("Downloads/myworkspace/NH3N_beiyunhe/fenghe.xlsx", range = "B1:B25", sheet = 'Sheet1')
#C_chushi = C_chushi[[1]]
#library(fitdistrplus)
#fitdist(C_chushi,'gamma')

sample_C = rgamma(50,shape=1.024172, rate=1.247033) #NH3N   ####抽取多少???
sample_C = rgamma(50,shape=1.610366, rate=2.408685) #TP

#C<-seq(0,8,by=0.01)
#p_C=dgamma(C,shape=0.5670108, rate=0.3721197)
#plot(C,p_C)
#write.table(p_C,file = "pastethis_C.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)

##############vvvvvvv W vvvvvv
Q_pai = 0.346388255
Cs = 40  #NH3N 2; TP 0.4
#W = seq(270,440,by=1) #COD

#####决定sd取多少！！
#plot(W,pnorm(W,mean=453.8,sd=70),type = "l")
#points(453.8,0.5,col="red",pch=20,cex=c(2,4))
#points(443.9,0.4,col="red",pch=20,cex=c(2,4))
#points(409.7,0.3,col="red",pch=20,cex=c(2,4))
#points(386.8,0.2,col="red",pch=20,cex=c(2,4))
#points(370.5,0.1,col="red",pch=20,cex=c(2,4))

#p_W_prior = dnorm(W,mean=338.3,sd=25)  #COD
W = seq(0.25,2.55,by=0.01) #NH3N
p_W_prior = dnorm(W,mean=1.4,sd=0.3)

W = seq(0.6,1.22,by=0.01) #TP
p_W_prior = dnorm(W,mean=0.91,sd=0.12)

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
write.table(p_W,file = "pastethis_p_W.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)

skewness(p_W)
kurtosis(p_W)
sum(W*p_W)

p_cdf=c()
for(i in 1:length(p_W)){
  p_cdf = c(p_cdf, sum(p_W[1:i]))
}
write.table(p_cdf,file = "pastethis_p_cdf.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)

##############^^^^^^^ W ^^^^^^^
