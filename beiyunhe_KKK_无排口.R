

##############vvvvvvv K鍊肩巼???? vvvvvv
library(readxl)
library(infotheo)
k<-seq(0.01,0.80,by=0.001) #383涓猭

C_in <- read_excel("Downloads//myworkspace/K_parameter/yulinzhuang2.xlsx", range = "C1:C27", sheet = 'Sheet1')
C_out <- read_excel("Downloads/myworkspace/K_parameter/yulinzhuang2.xlsx", range = "D1:D27", sheet = 'Sheet1')
t <- read_excel("Downloads/myworkspace/K_parameter/yulinzhuang2.xlsx", range = "E1:E27", sheet = 'Sheet1')
q <- read_excel("Downloads/myworkspace/K_parameter/yulinzhuang2.xlsx", range = "G1:G27", sheet = 'Sheet1')
C_in = C_in[[1]]
C_out = C_out[[1]]
t = t[[1]]
q = q[[1]]

C_out_calculate <- matrix(nrow = length(k), ncol= length(C_out))
L <- matrix(nrow = length(k), ncol = length(C_out))

for(j in 1:length(C_out)){
  for (i in 1:length(k)) {
    C_out_calculate[i,j] <- C_in[j]*exp(-k[i]*t[j])
    L[i,j]<-(C_out_calculate[i,j]-C_out[j])^2
  }
} 
Likelihood = 1/rowSums(L)
p_k = Likelihood/sum(Likelihood)
plot(k, p_k, type = 'l', lwd=2)

#write.table(p_k,file = "yulinzhuang_K(0.01-0.8).txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)
