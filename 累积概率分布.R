windows(width=30,height=4)
par(mfrow=c(2,5))

setwd("D:/RStudio/myworkspace/RESULTS_beiyunhe")

p_tugouqiao = read.table(file = "tugouqiao_W(470,580).txt")
p_tugouqiao = p_tugouqiao[[1]]
W_tugouqiao = seq(470,580,by=1)
p2_tugouqiao = c()
for(i in 1:length(p_tugouqiao)){
  p2_tugouqiao = c(p2_tugouqiao, sum(p_tugouqiao[1:i]))
}
plot(W_tugouqiao,p2_tugouqiao,type='l', main="土沟桥-温榆河顺义区",lwd=2)
points(524.1,0.5,col="red",pch=20,cex=c(2,4))
points(522.3,0.4,col="red",pch=20,cex=c(2,4))
points(519.7,0.3,col="red",pch=20,cex=c(2,4))
points(512.2,0.2,col="red",pch=20,cex=c(2,4))
points(505.1,0.1,col="red",pch=20,cex=c(2,4))


p_jinyuqiao = read.table(file = "jinyuqiao_W(1370,1530).txt")
p_jinyuqiao = p_jinyuqiao[[1]]
W_jinyuqiao = seq(1370,1530,by=1)
p2_jinyuqiao = c()
for(i in 1:length(p_jinyuqiao)){
  p2_jinyuqiao = c(p2_jinyuqiao, sum(p_jinyuqiao[1:i]))
}
plot(W_jinyuqiao,p2_jinyuqiao,type='l',main="温榆河顺义区-榆林庄",lwd=2)
points(1445.9,0.5,col="red",pch=20,cex=c(2,4))
points(1445.9,0.4,col="red",pch=20,cex=c(2,4))
points(1433.2,0.3,col="red",pch=20,cex=c(2,4))
points(1419.9,0.2,col="red",pch=20,cex=c(2,4))
points(1407.2,0.1,col="red",pch=20,cex=c(2,4))


###ke
p_wangjiabai = read.table(file = "wangjiabai_W(455,575).txt")
p_wangjiabai = p_wangjiabai[[1]]
W_wangjiabai = seq(455,575,by=1)
p2_wangjiabai = c()
for(i in 1:length(p_wangjiabai)){
  p2_wangjiabai = c(p2_wangjiabai, sum(p_wangjiabai[1:i]))
}
plot(W_wangjiabai,p2_wangjiabai,type='l',main="王家摆-土门楼",lwd=2)
points(513.1,0.5,col="red",pch=20,cex=c(2,4))
points(510.9,0.4,col="red",pch=20,cex=c(2,4))
points(505.5,0.3,col="red",pch=20,cex=c(2,4))
points(496.2,0.2,col="red",pch=20,cex=c(2,4))
points(481.5,0.1,col="red",pch=20,cex=c(2,4))



p_qinyingyang = read.table(file = "qinyingyang_W(250,630).txt")
p_qinyingyang = p_qinyingyang[[1]]
W_qinyingyang = seq(250,630,by=1)
p2_qinyingyang = c()
for(i in 1:length(p_qinyingyang)){
  p2_qinyingyang = c(p2_qinyingyang, sum(p_qinyingyang[1:i]))
}
plot(W_qinyingyang,p2_qinyingyang,type='l',main="秦营扬水站-筐儿港",lwd=2)
points(453.8,0.5,col="red",pch=20,cex=c(2,4))
points(443.9,0.4,col="red",pch=20,cex=c(2,4))
points(409.7,0.3,col="red",pch=20,cex=c(2,4))
points(386.8,0.2,col="red",pch=20,cex=c(2,4))
points(370.5,0.1,col="red",pch=20,cex=c(2,4))



p_kuangergang = read.table(file = "kuangergang_W(110,310).txt")
p_kuangergang = p_kuangergang[[1]]
W_kuangergang = seq(110,310,by=1)
p2_kuangergang = c()
for(i in 1:length(p_kuangergang)){
  p2_kuangergang = c(p2_kuangergang, sum(p_kuangergang[1:i]))
}
plot(W_kuangergang,p2_kuangergang,type='l',main="筐儿港-新老米店闸",lwd=2)
points(213.4,0.5,col="red",pch=20,cex=c(2,4))
points(197.2,0.4,col="red",pch=20,cex=c(2,4))
points(182.3,0.3,col="red",pch=20,cex=c(2,4))
points(171.4,0.2,col="red",pch=20,cex=c(2,4))
points(163.9,0.1,col="red",pch=20,cex=c(2,4))



p_nanshahe = read.table(file = "nanshahe_W(0,60).txt")
p_nanshahe = p_nanshahe[[1]]
W_nanshahe = seq(0,60,by=1)
p2_nanshahe = c()
for(i in 1:length(p_nanshahe)){
  p2_nanshahe = c(p2_nanshahe, sum(p_nanshahe[1:i]))
}
plot(W_nanshahe,p2_nanshahe,type='l',main="南沙河入昌平-南沙河口",lwd=2)
points(24.6,0.5,col="red",pch=20,cex=c(2,4))
points(22.5,0.4,col="red",pch=20,cex=c(2,4))
points(20.9,0.3,col="red",pch=20,cex=c(2,4))
points(16.1,0.2,col="red",pch=20,cex=c(2,4))
points(12.7,0.1,col="red",pch=20,cex=c(2,4))



p_qinghezha = read.table(file = "qinghezha_W(270,440).txt")
p_qinghezha = p_qinghezha[[1]]
W_qinghezha = seq(270,440,by=1)
p2_qinghezha = c()
for(i in 1:length(p_qinghezha)){
  p2_qinghezha = c(p2_qinghezha, sum(p_qinghezha[1:i]))
}
plot(W_qinghezha,p2_qinghezha,type='l',main="清河闸-沙子营",lwd=2)
points(338.3,0.5,col="red",pch=20,cex=c(2,4))
points(329.8,0.4,col="red",pch=20,cex=c(2,4))
points(323.4,0.3,col="red",pch=20,cex=c(2,4))
points(318.4,0.2,col="red",pch=20,cex=c(2,4))
points(307.1,0.1,col="red",pch=20,cex=c(2,4))



p_dahongmenzha = read.table(file = "dahongmenzha_W(600,780).txt")
p_dahongmenzha = p_dahongmenzha[[1]]
W_dahongmenzha = seq(600,780,by=1)
p2_dahongmenzha = c()
for(i in 1:length(p_dahongmenzha)){
  p2_dahongmenzha = c(p2_dahongmenzha, sum(p_dahongmenzha[1:i]))
}
plot(W_dahongmenzha,p2_dahongmenzha,type='l',main="大红门闸上-凉水河口",lwd=2)
points(682.5,0.5,col="red",pch=20,cex=c(2,4))
points(673.7,0.4,col="red",pch=20,cex=c(2,4))
points(669.0,0.3,col="red",pch=20,cex=c(2,4))
points(661.7,0.2,col="red",pch=20,cex=c(2,4))
points(649.2,0.1,col="red",pch=20,cex=c(2,4))



p_fenghe = read.table(file = "fenghe_W_2(10,160).txt")
p_fenghe = p_fenghe[[1]]
W_fenghe = seq(10,160,by=1)
p2_fenghe = c()
for(i in 1:length(p_fenghe)){
  p2_fenghe = c(p2_fenghe, sum(p_fenghe[1:i]))
}
plot(W_fenghe,p2_fenghe,type='l',main="凤河-前侯尚村",lwd=2)
#sum(p_W*W)  #79.19791
#skewness(p_W)   #0.7548632
#kurtosis(p_W)   #1.981581
'''
points(80.7,0.5,col="red",pch=20,cex=c(2,4))
points(76.2,0.4,col="red",pch=20,cex=c(2,4))
points(71.3,0.3,col="red",pch=20,cex=c(2,4))
points(67.7,0.2,col="red",pch=20,cex=c(2,4))
points(60.1,0.1,col="red",pch=20,cex=c(2,4))
'''
points(97.3,0.5,col="red",pch=20,cex=c(2,4))
points(91.3,0.4,col="red",pch=20,cex=c(2,4))
points(88.1,0.3,col="red",pch=20,cex=c(2,4))
points(83.6,0.2,col="red",pch=20,cex=c(2,4))
points(80.7,0.1,col="red",pch=20,cex=c(2,4))

points(59.8,0.5,col="red",pch=20,cex=c(2,4))
points(54.8,0.4,col="red",pch=20,cex=c(2,4))
points(51.9,0.3,col="red",pch=20,cex=c(2,4))
points(47.3,0.2,col="red",pch=20,cex=c(2,4))
points(43.5,0.1,col="red",pch=20,cex=c(2,4))



p_dongditou = read.table(file = "dongditou_W(0,130).txt")
p_dongditou = p_dongditou[[1]]
W_dongditou = seq(0,130,by=1)
p2_dongditou = c()
for(i in 1:length(p_dongditou)){
  p2_dongditou = c(p2_dongditou, sum(p_dongditou[1:i]))
}
plot(W_dongditou,p2_dongditou,type='l',main="东堤头闸上-龙凤河口",lwd=2)
'''
points(64.4,0.5,col="red",pch=20,cex=c(2,4))
points(62.2,0.4,col="red",pch=20,cex=c(2,4))
points(58.1,0.3,col="red",pch=20,cex=c(2,4))
points(50.2,0.2,col="red",pch=20,cex=c(2,4))
points(44.1,0.1,col="red",pch=20,cex=c(2,4))
'''
points(68.7,0.5,col="red",pch=20,cex=c(2,4))
points(67.1,0.4,col="red",pch=20,cex=c(2,4))
points(65.1,0.3,col="red",pch=20,cex=c(2,4))
points(63.9,0.2,col="red",pch=20,cex=c(2,4))
points(63.4,0.1,col="red",pch=20,cex=c(2,4))






