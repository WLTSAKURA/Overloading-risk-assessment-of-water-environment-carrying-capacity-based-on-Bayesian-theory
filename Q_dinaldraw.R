windows(width=30,height=4)
par(mfrow=c(2,5))

setwd("Downloads/myworkspace/RESULTS_beiyunhe")
library(readxl)
library(moments)

flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/tugouqiao.xlsx", range = "C1:C37", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="土沟桥", lwd=2)
#dg = dgamma(x-a0,shape=a, rate=b)
#write.table(dg,file = "tugouqiao_Q.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)


flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/jinyuqiao.xlsx", range = "C1:C109", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="jinyuqiao",lwd=2)
#actual_prob <-read_excel("Downloads/myworkspace/flow_beiyunhe/jinyuqiao.xlsx", sheet = 'actual_prob')
#x_actual = actual_prob[[1]]
#y_actual = actual_prob[[2]]
#plot(x,1-pgamma(x-a0,shape=a, rate=b),type='l', xlim=c(0,70), ylim = c(0,1))
#par(new=TRUE)
#plot(x_actual,y_actual,type = 'p',xlim=c(0,70), ylim = c(0,1))


flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/S2.xlsx", range = "C1:C49", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="wangjiabai",lwd=2)
#actual_prob <-read_excel("Downloads/myworkspace/flow_beiyunhe/S2.xlsx", sheet = 'actual_prob')
#x_actual = actual_prob[[1]]
#y_actual = actual_prob[[2]]
#plot(x,1-pgamma(x-a0,shape=a, rate=b),type='l', xlim=c(0,70), ylim = c(0,1))
#par(new=TRUE)
#plot(x_actual,y_actual,type = 'p',xlim=c(0,70), ylim = c(0,1))


flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/S18.xlsx", range = "C1:C102", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="??Ӫ??ˮվ",lwd=2)


flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/kuangergang.xlsx", range = "C1:C102", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="??????",lwd=2)


flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/nanshahe.xlsx", range = "C1:C34", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="??ɳ??????ƽ",lwd=2)



flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/qinghezha.xlsx", range = "C1:C121", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="????բ",lwd=2)



flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/S14.xlsx", range = "C1:C97", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="??????բ??",lwd=2)



flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/fenghe.xlsx", range = "C1:C97", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="????",lwd=2)


flow<-read_excel("Downloads/myworkspace/flow_beiyunhe/S17.xlsx", range = "C1:C121", sheet = 'Sheet1')
x_ba = mean(flow[[1]])
c_v = sd(flow[[1]])/x_ba
c_s = length(flow[[1]]) * skewness(flow[[1]])/(length(flow[[1]])-3)
a = 4/c_s^2
b = 2/(x_ba*c_v*c_s)
a0 = x_ba*(1-2*c_v/c_s)
x=seq(0,max(flow[[1]])+5,by=0.01)
plot(x,dgamma(x-a0,shape=a, rate=b),type='l',main="????ͷբ??",lwd=2)
