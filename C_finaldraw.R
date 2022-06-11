windows(width=30,height=4)
par(mfrow=c(2,5))
setwd("D:/RStudio/myworkspace/RESULTS_beiyunhe")
library(readxl)

C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/tugouqiao.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=2.2836536 ,rate=0.0541245),type='l',main="土沟桥",lwd=2)


C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/jinyuqiao.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=2.18798363, rate=0.04558722),type='l',main="温榆河顺义区",lwd=2)


C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/wangjiabai.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=5.599409, rate=0.119169),type='l',main="王家摆",lwd=2)


C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/qinyingyang.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=2.16359114, rate=0.04023817),type='l',main="秦营扬水站",lwd=2)


C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/kuangergang.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=4.0666892, rate=0.2115652),type='l',main="筐儿港",lwd=2)


C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/nanshahe.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=1.28275323, rate=0.02039273),type='l',main="南沙河入昌平",lwd=2)



C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/qinghezha.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=1.3947388,rate=0.0505176),type='l',main="清河闸",lwd=2)


C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/dahongmenzha.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=3.391585, rate=0.124711),type='l',main="大红门闸上",lwd=2)


C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/fenghe.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=3.9236899, rate=0.1791936),type='l',main="凤河",lwd=2)


C_chushi <- read_excel("D:/RStudio/myworkspace/COD_beiyunhe/dongditou.xlsx", range = "B1:B169", sheet = 'Sheet1')
C_chushi = C_chushi[[1]]
C = seq(0,max(C_chushi),by=1)
plot(C,dgamma(C,shape=4.2442044, rate=0.2166339),type='l',main="东堤头闸上",lwd=2)
