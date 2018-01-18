filedata<- "korelasiV1_2"
lokasi <-"C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\Data Olahan\\49\\Data prasyarat dengan R spearman\\"

data <- read.csv(file=paste(lokasi,filedata,".csv",sep=""),row.names=NULL,head=TRUE,sep=",",fill=TRUE)
cor1<-data$KOM202
cor2<-data$KOM204
cor.test(cor1,cor2,method="spearman")

