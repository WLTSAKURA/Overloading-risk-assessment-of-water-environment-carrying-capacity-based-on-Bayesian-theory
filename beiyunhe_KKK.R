

##############vvvvvvv K鍊肩巼瀹? vvvvvv
library(readxl)
library(infotheo)
k<-seq(0.018,0.4,by=0.001) #383涓猭

C_in <- read_excel("D:/RStudio/myworkspace/K_parameter/qinghezha.xlsx", range = "C1:C121", sheet = 'Sheet1')
C_out <- read_excel("D:/RStudio/myworkspace/K_parameter/qinghezha.xlsx", range = "D1:D121", sheet = 'Sheet1')
t1 <- read_excel("D:/RStudio/myworkspace/K_parameter/qinghezha.xlsx", range = "E1:E121", sheet = 'Sheet1')
t2 <- read_excel("D:/RStudio/myworkspace/K_parameter/qinghezha.xlsx", range = "F1:F121", sheet = 'Sheet1')
q <- read_excel("D:/RStudio/myworkspace/K_parameter/qinghezha.xlsx", range = "G1:G121", sheet = 'Sheet1')
C_in = C_in[[1]]
C_out = C_out[[1]]
t1 = t1[[1]]
t2 = t2[[1]]
q = q[[1]]
Q_pai = 5.75019
C_pai = 168.88

C_out_calculate <- matrix(nrow = length(k), ncol= length(C_out))
L <- matrix(nrow = length(k), ncol = length(C_out))

for(j in 1:length(C_out)){
  for (i in 1:length(k)) {
    C_out_calculate[i,j] <- ((q[j]*C_in[j]*exp(-k[i]*t1[j]) + Q_pai*C_pai)/(q[j]+Q_pai))*exp(-k[i]*t2[j])
    L[i,j]<-(C_out_calculate[i,j]-C_out[j])^2
  }
} 
Likelihood = 1/rowSums(L)
p_k = Likelihood/sum(Likelihood)
plot(k, p_k, type = 'l')

#write.table(p_k,file = "qinghezha_K.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)
