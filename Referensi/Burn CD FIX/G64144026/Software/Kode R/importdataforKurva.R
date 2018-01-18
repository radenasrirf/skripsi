require(C50)
require(ROCR)
require(car)
require(caret)
require(Formula)
require(FSelector)
require(e1071)
require(entropy)
require(xlsx)
require(PRROC)
filedata<-"ds_3k_ALL_v1_balanced_1"
filedata1<- "testdata_ds_3k_ALL_v1_balanced_1"
filedata2<- "traindata_ds_3k_ALL_v1_balanced_1"

lokasi <-"C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\"

data1 <- read.csv(file=paste(lokasi,"Data Uji\\",filedata1,".csv",sep=""),row.names=NULL,head=TRUE,sep=",",fill=TRUE)
data1$X<-NULL
data2 <- read.csv(file=paste(lokasi,"Data Latih\\",filedata2,".csv",sep=""),row.names=NULL,head=TRUE,sep=",",fill=TRUE)
data2$X<-NULL

testData <-data1
trainData <-data2
#c50
trainData$CLASS<-as.factor(trainData$CLASS)
oneTree <- C5.0(CLASS~.,data=trainData)
View(oneTree)
summary(oneTree)
#prediksi model
oneTreePred <- predict(oneTree, testData[,-16])
View(oneTreePred)
oneTreePredProb <- predict(oneTree, testData[,-16],type='prob')
#multiclass.roc(ds_2k_ALL_balanced$CLASS, oneTreePredProb)
View(data1$CLASS)
#View(oneTreePred)
as.numeric(oneTreePred[1])
#write.table(oneTreePred, file = paste("C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\2Pred\\data_",filedata,".csv",sep=""),row.names = FALSE,col.names = FALSE)
write.table(data1$CLASS, file = paste("C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\1Test\\kelasdatauji_",filedata,".csv",sep=""),row.names = FALSE,col.names = FALSE)
dataPred<-matrix(0,nrow=nrow(oneTreePredProb),ncol=ncol(oneTreePredProb))
for(i in 1:nrow(oneTreePredProb))
{
  dataPred[i,as.numeric(oneTreePred[i])] = 1
}
View(dataPred)
kelasasli<-matrix(0,nrow=nrow(oneTreePredProb),ncol=ncol(oneTreePredProb))
for(i in 1:nrow(oneTreePredProb))
{
  kelasasli[i,as.numeric(data1$CLASS[i])] = 1
}
View(kelasasli)
View(dataPred)
write.table(dataPred, file = paste("C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\2PredAsli\\dataprediksi_",filedata,".csv",sep=""),row.names = FALSE,col.names = FALSE,sep=',')
write.table(kelasasli, file = paste("C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\2PredAsli\\kelasasli_",filedata,".csv",sep=""),row.names = FALSE,col.names = FALSE,sep=',')
write.table(oneTreePredProb, file = paste("C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\2PredAsli\\prob_",filedata1,".csv",sep=""),row.names = FALSE,col.names = FALSE,sep=',')


dconf_test <- table(oneTreePred,testData$CLASS)
View(data1$CLASS)
