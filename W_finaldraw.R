windows(width=30,height=4)
par(mfrow=c(2,5))

setwd("Downloads/myworkspace/RESULTS_beiyunhe")

p_tugouqiao = read.table(file = "tugouqiao_W(470,580).txt")
p_tugouqiao = p_tugouqiao[[1]]
W_tugouqiao = seq(470,580,by=1)
p2_tugouqiao = c()
for(i in 1:length(p_tugouqiao)){
  p2_tugouqiao = c(p2_tugouqiao, sum(p_tugouqiao[1:i]))
}
plot(W_tugouqiao,p_tugouqiao,type='l',main="土沟桥", lwd=2)
legend("topright",legend = c("expectation=525.82","skewness=0.36","kurtosis=1.61"),cex=0.7)
###expectation=sum(W_tugouqiao*p_tugouqiao)



p_jinyuqiao = read.table(file = "jinyuqiao_W(1370,1530).txt")
p_jinyuqiao = p_jinyuqiao[[1]]
W_jinyuqiao = seq(1370,1530,by=1)
plot(W_jinyuqiao,p_jinyuqiao,type='l',main="???ܺ?˳????-????ׯ",lwd=2)
legend("topright",legend = c("expectation=1446.2","skewness=0.87","kurtosis=2.21"),cex=0.7)


p_wangjiabai = read.table(file = "wangjiabai_W(455,575).txt")
p_wangjiabai = p_wangjiabai[[1]]
W_wangjiabai = seq(455,575,by=1)
plot(W_wangjiabai,p_wangjiabai,type='l',main="???Ұ?-????¥",lwd=2)
legend("topright",legend = c("expectation=513.2","skewness=0.47","kurtosis=1.69"),cex=0.7)


p_qinyingyang = read.table(file = "qinyingyang_W(250,630).txt")
p_qinyingyang = p_qinyingyang[[1]]
W_qinyingyang = seq(250,630,by=1)
plot(W_qinyingyang,p_qinyingyang,type='l',main="??Ӫ??ˮվ-??????",lwd=2)
legend("topright",legend = c("expectation=451.9","skewness=0.48","kurtosis=1.69"),cex=0.7)


p_kuangergang = read.table(file = "kuangergang_W(110,310).txt")
p_kuangergang = p_kuangergang[[1]]
W_kuangergang = seq(110,310,by=1)
plot(W_kuangergang,p_kuangergang,type='l',main="??????-?????׵?բ",lwd=2)
legend("topright",legend = c("expectation=207.3","skewness=0.57","kurtosis=1.78"),cex=0.7)


p_nanshahe = read.table(file = "nanshahe_W(0,60).txt")
p_nanshahe = p_nanshahe[[1]]
W_nanshahe = seq(0,60,by=1)
plot(W_nanshahe,p_nanshahe,type='l',main="??ɳ??????ƽ-??ɳ?ӿ?",lwd=2)
legend("topright",legend = c("expectation=25.5","skewness=-0.35","kurtosis=1.84"),cex=0.7)


p_qinghezha = read.table(file = "qinghezha_W(270,440).txt")
p_qinghezha = p_qinghezha[[1]]
W_qinghezha = seq(270,440,by=1)
plot(W_qinghezha,p_qinghezha,type='l',main="????բ-ɳ??Ӫ",lwd=2)
legend("topright",legend = c("expectation=340.1","skewness=0.98","kurtosis=2.42"),cex=0.7)


p_dahongmenzha = read.table(file = "dahongmenzha_W(600,780).txt")
p_dahongmenzha = p_dahongmenzha[[1]]
W_dahongmenzha = seq(600,780,by=1)
plot(W_dahongmenzha,p_dahongmenzha,type='l',main="??????բ??-��ˮ?ӿ?",lwd=2)
legend("topright",legend = c("expectation=684.5","skewness=1.04","kurtosis=2.54"),cex=0.7)


p_fenghe = read.table(file = "fenghe_W_2(10,160).txt")
p_fenghe = p_fenghe[[1]]
W_fenghe = seq(10,160,by=1)
plot(W_fenghe,p_fenghe,type='l',main="????-ǰ???д?",lwd=2)
#sum(p_W*W)  #79.19791
#skewness(p_W)   #0.7548632
#kurtosis(p_W)   #1.981581
legend("topright",legend = c("expectation=79.198","skewness=0.755","kurtosis=1.982"),cex=0.8)



p_dongditou = read.table(file = "dongditou_W(0,130).txt")
p_dongditou = p_dongditou[[1]]
W_dongditou = seq(0,130,by=1)
plot(W_dongditou,p_dongditou,type='l',main="????ͷբ??-?????ӿ?",lwd=2)
legend("topright",legend = c("expectation=69.6","skewness=0.59","kurtosis=1.81"),cex=0.7)







