filedata1<- "testdata_ds_2K_ALL_balanced_4"
filedata2<- "traindata_ds_2K_ALL_balanced_4"
lokasi <-"C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\"

data1 <- read.csv(file=paste(lokasi,"Data Uji\\",filedata1,".csv",sep=""),row.names=NULL,head=TRUE,sep=",",fill=TRUE)
data1$X<-NULL
data2 <- read.csv(file=paste(lokasi,"Data Latih\\",filedata2,".csv",sep=""),row.names=NULL,head=TRUE,sep=",",fill=TRUE)
data2$X<-NULL

testData <-data1
trainData <-data2
trainData$CLASS<-as.factor(trainData$CLASS)
oneTree <- C5.0(CLASS~.,data=trainData)
summary(oneTree)
oneTreePred <- predict(oneTree, testData)
conf_test <- table(oneTreePred,testData$CLASS)
akurasi <- formatC(100*mean(sum(diag(conf_test))/sum(conf_test)))
akurasi
roc.prediction = prediction(as.numeric(as.factor(oneTreePred)), as.numeric(as.factor(testData$CLASS)))

roc.prec.rec = performance(roc.prediction,"prec","rec")
plot(roc.prec.rec, col="red",lty=3)


roc.tpr.fpr = performance(roc.prediction,"tpr","fpr")
roc.auc = performance(roc.prediction,"auc")
plot(roc.tpr.fpr, col="red",lty=3)
abline(a=0, b= 1)
formatC(roc.auc@y.values[[1]],digits=2,format="f")
