library(readxl)
library(fitdistrplus)

dibiaoshui<-read_excel("Downloads/myworkspace/shuiziyuanliang/shuiziyuan.xlsx", range = "B1:B97", sheet = 'Sheet1')
dibiaoshui = dibiaoshui[[1]]
fitdist(dibiaoshui,'gamma')
fitdist(dibiaoshui,'gamma',method = "mme")
#hist(dibiaoshui)
#DBS <-seq(0,1300,by=1)
#p_DBS = dgamma(DBS,shape=1.367714080, rate=0.006494904)
#plot(DBS, p_DBS)

random_DBS = rgamma(500,shape=1.367714080, rate=0.006494904) ##抽多少个


dixiashui<-read_excel("Downloads/myworkspace/shuiziyuanliang/shuiziyuan.xlsx", range = "D1:D121", sheet = 'Sheet1')
dixiashui = dixiashui[[1]]
fitdist(dixiashui,'gamma')
#fitdist(dixiashui,'gamma',method = "mme")

random_DXS = rgamma(500,shape=6.29252599, rate=0.02126351) ##抽多少个

total_shuiziyuan = random_DBS + random_DXS
fitdist(total_shuiziyuan,'gamma')
hist(total_shuiziyuan)

###############!!!上面的不要了
shuiziyuan <-read_excel("Downloads/myworkspace/shuiziyuanliang/shuiziyuan.xlsx", range = "A1:A97", sheet = 'Sheet1')
shuiziyuan = shuiziyuan[[1]]
fitdist(shuiziyuan,'gamma')
hist(shuiziyuan,freq = FALSE, breaks = 20)

SZY <-seq(0,600,by=5)
p_SZY = dgamma(SZY,shape=2.02245126, rate=0.02212004)
plot(SZY, p_SZY)
write.table(p_SZY,file = "pastethis_SZY.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)

120
240
360
480
pgamma(120,shape=2.02245126, rate=0.02212004)
pgamma(240,shape=2.02245126, rate=0.02212004)-pgamma(120,shape=2.02245126, rate=0.02212004)
pgamma(360,shape=2.02245126, rate=0.02212004)-pgamma(240,shape=2.02245126, rate=0.02212004)
pgamma(480,shape=2.02245126, rate=0.02212004)-pgamma(360,shape=2.02245126, rate=0.02212004)










############ 用水量######

yongshui<-read_excel("Downloads/myworkspace/shuiziyuanliang/shuiziyuan.xlsx", range = "B1:B121", sheet = 'Sheet1')
yongshui = yongshui[[1]]
fitdist(yongshui,'gamma')
fitdist(yongshui,'gamma',method = "mme")
hist(yongshui)

YS <-seq(0,4000,by=10)
p_YS = dgamma(YS,shape=0.996345454, rate=0.001105212)
plot(YS, p_YS)
write.table(p_YS,file = "pastethis_YS.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)

1600
3200
4800
6400
pgamma(1600,shape=1.0234619406, rate=0.0006040869)
pgamma(3200,shape=1.0234619406, rate=0.0006040869)-pgamma(1600,shape=1.0234619406, rate=0.0006040869)
pgamma(4800,shape=1.0234619406, rate=0.0006040869)-pgamma(3200,shape=1.0234619406, rate=0.0006040869)
pgamma(6400,shape=1.0234619406, rate=0.0006040869)-pgamma(4800,shape=1.0234619406, rate=0.0006040869)









