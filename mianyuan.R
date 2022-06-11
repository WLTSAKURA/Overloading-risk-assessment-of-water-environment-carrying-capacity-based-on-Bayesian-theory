library(readxl)
library(fitdistrplus)

mianyuan<-read_excel("Downloads/myworkspace/mianyuan/TP/jinyuqiao.xlsx", range = "B1:B121", sheet = 'Sheet1')
mianyuan = mianyuan[[1]]
fitdist(mianyuan,'gamma')
fitdist(mianyuan,'gamma',method = "mme")
hist(mianyuan, freq = FALSE, breaks = 20)


M <-seq(0,8,by=0.1)
p_M = dgamma(M,shape=0.3730878, rate=2.2374054)
plot(M, p_M)
write.table(p_M,file = "pastethis_M.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)

#write.table(p_cdf_M,file = "pastethis_p_cdf.txt",sep='\n',quote = FALSE,row.names = FALSE,col.names = FALSE)
0.36
0.72
1.08
1.44
pgamma(0.36,shape=0.1629365, rate=0.7245838)
pgamma(0.72,shape=0.1629365, rate=0.7245838)-pgamma(0.36,shape=0.1629365, rate=0.7245838)
pgamma(1.08,shape=0.1629365, rate=0.7245838)-pgamma(0.72,shape=0.1629365, rate=0.7245838)
pgamma(1.44,shape=0.1629365, rate=0.7245838)-pgamma(1.08,shape=0.1629365, rate=0.7245838)






