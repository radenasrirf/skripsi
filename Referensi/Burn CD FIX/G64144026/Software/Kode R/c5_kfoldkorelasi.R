  require(C50)
  require(ROCR)
  require(car)
  require(caret)
  require(Formula)
  require(FSelector)
  require(e1071)
  require(entropy)
  require(xlsx)
  sumber<-"C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\Data Olahan\\49\\Data olahan Korelasi v1\\"
  filedata <- "dat2korv1_5"
  lokasi <-"C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\2 SIDANG\\Data Olahan\\49\\result\\"
  
  #Membaca dataset yang akan digunakan
  data <- read.csv(file=paste(sumber,filedata,".csv",sep=""),head=TRUE,sep=",",fill=TRUE)
  
  #Randomly shuffle the data / Acak data
  data<-data[sample(nrow(data)),]
  #Pembagian data dengan cross validation(ukurannya 10)
    folds<-cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
    dt <- (matrix('',nrow = 10,ncol = 2)) # '' removes NA from your final spreadsheet
    dtl<- (matrix('',nrow = 10,ncol = 7))
    dtakurasi<-(matrix('',nrow=10,ncol=2))
    dtauc<-(matrix('',nrow=10,ncol=2))
    
    dtakurasirule<-(matrix('',nrow=10,ncol=2))
    dtaucrule<-(matrix('',nrow=10,ncol=2))
    for(i in 1:10)
      {
        testIndexes <- which(folds==i, arr.ind=TRUE)
        testData <- data[testIndexes, ]
        trainData <- data[-testIndexes, ]
        testData
        #Export data uji to Excel
        write.csv(testData, file = paste(lokasi,"Data Uji\\testdata_",filedata,"_",i,".csv",sep=""))
        trainData
        #Export data latih to Excel
        write.csv(trainData, file = paste(lokasi,"Data Latih\\traindata_",filedata,"_",i,".csv",sep=""))
        
        #Pembuatan model pohon keputusan
        trainData$CLASS<-as.factor(trainData$CLASS)
        oneTree <- C5.0(CLASS~.,data=trainData)
        
        #Nilai akurasi dari model pohon keputusan dihitung menggunakan fungsi
        #predict. Data yang digunakan adalah data uji.
        oneTreePred <- predict(oneTree, testData)
        oneTreePred
        #Pembuatan model berbasis aturan
        rules<-C5.0(CLASS~.,data=trainData, rules=TRUE)
        rules
        summary
        predict_train<- predict(oneTree, newdata=trainData)
        conf_train <- table(trainData$CLASS, predict_train)
        predict <- predict(oneTree, newdata=testData)
        conf_test <- table(predict,testData$CLASS)
        conf_test
        
        predictRule <- predict(rules, newdata=testData)
        conf_testRule <- table(predictRule,testData$CLASS)
        ttest<-table(testData$CLASS)
        ttest
        trtest<-table(trainData$CLASS)
        trtest
        
        akurasi <- sum(diag(conf_test))/sum(conf_test)
        akurasiRule <- sum(diag(conf_testRule))/sum(conf_testRule)
        #rocr pohon keputusan
        tryCatch({
        rocTree<-(roc.prediction = prediction(as.numeric(as.factor(oneTreePred)), as.numeric(as.factor(testData$CLASS))))
        roctfTree<-(roc.tpr.fpr = performance(rocTree,"tpr","fpr"))
        rocaucTree<-(roc.auc = performance(rocTree,"auc"))
        png(filename=paste(lokasi,"Plot\\plotAUC_",filedata,"_",i,".png",sep=""),width = 800, height = 600)
        plot(roctfTree, col="red",lty=3)
        abline(a=0, b= 1)
        dev.off()
        
        rocprTree<-(roc.prec.rec = performance(rocTree,"prec","rec"))
        png(filename=paste(lokasi,"precrecall\\plotPR_",filedata,"_",i,".png",sep=""),width = 800, height = 600)
        plot(rocprTree, col="red",lty=3)
        dev.off()
        }, error=function(e){cat("ERROR gambar plot auc ke- ",i,": ",conditionMessage(e), "\n")})
        #precision recall curve (just for binary class)
        
        
        #rocr rule
        tryCatch({
          rocRules<-(roc.prediction = prediction(as.numeric(as.factor(predictRule)), as.numeric(as.factor(testData$CLASS))))
          roctfRule<-(roc.tpr.fpr = performance(rocRules,"tpr","fpr"))
          rocaucs<-(roc.auc = performance(rocRules,"auc"))
          png(filename=paste(lokasi,"rl\\Plot\\plotAUCrl_",filedata,"_",i,".png",sep=""),width = 800, height = 600)
          plot(roctfRule, col="red",lty=3)
          abline(a=0, b= 1)
          dev.off()
          dtaucrule[i,1]<-i
          dtaucrule[i,2]<-formatC(rocaucs@y.values[[1]],digits=2,format="f")
          rocprRules<-(roc.prec.rec = performance(rocRules,"prec","rec"))
          png(filename=paste(lokasi,"rl\\precrecall\\plotPR_",filedata,"_",i,".png",sep=""),width = 800, height = 600)
          plot(rocprRules, col="red",lty=3)
          dev.off()
        }, error=function(e){cat("ERROR gambar plot auc rule ke- ",i,": ",conditionMessage(e), "\n")})
        #precision recall curve (just for binary class)
        
        
        #All of result will export to txt 
        fileCOnn<-file(paste(lokasi,"R\\dataR_",filedata,"_",i,".txt",sep=""))
        write(paste("jumlah data uji ke- ",i," = ",nrow(testData),sep=""),fileCOnn)
        fileCOnn<-file(paste(lokasi,"R\\dataR_",filedata,"_",i,".txt",sep=""), open="a")
        write(paste("jumlah data latih ke- ",i," = ",nrow(trainData),sep=""),append=TRUE,fileCOnn)
        write(paste("jumlah setiap kelas pada data uji ke- ",i," = ",sep=""),append=TRUE,fileCOnn)
        write.table(table(testData$CLASS), fileCOnn, sep="\t") 
        write(paste("jumlah setiap kelas pada data latih ke- ",i," = ",sep=""),append=TRUE,fileCOnn)
        write.table(table(trainData$CLASS), fileCOnn, sep="\t") 
        write(paste("Pohon Keputusan ke- ",i,sep=""),append=TRUE,fileCOnn)
        capture.output(oneTree,file=fileCOnn,append =TRUE)
        write(paste(summary(oneTree),sep=""),append=TRUE,fileCOnn)
        write(paste("akurasi pada k ke- ",i, ": ", formatC(100*mean(akurasi),digits = 2, format = "f"),sep="\t"),append=TRUE,fileCOnn)
        write(paste("Confusion Matrix dan akurasi ke- ",i,sep=""),append=TRUE,fileCOnn)
        m<-conf_test
        m
        capture.output(m,file=fileCOnn, append=TRUE)
        
        #akurasi rule
        fileCOnnR<-file(paste(lokasi,"rl\\R\\dataRule_",filedata,"_",i,".txt",sep=""))
        write(paste("Pohon Keputusan ke- ",i,sep=""),fileCOnnR)
        fileCOnnR<-file(paste(lokasi,"rl\\R\\dataRule_",filedata,"_",i,".txt",sep=""), open="a")
        capture.output(rules,file=fileCOnnR,append =TRUE)
        write(paste(summary(rules),sep=""),append=TRUE,fileCOnnR)
        write(paste("akurasi pada k ke- ",i, ": ", formatC(100*mean(akurasiRule),digits = 2, format = "f"),sep="\t"),append=TRUE,fileCOnnR)
        write(paste("Confusion Matrix dan akurasi ke- ",i,sep=""),append=TRUE,fileCOnnR)
        cfrule<-conf_testRule
        cfrule
        capture.output(cfrule,file=fileCOnnR, append=TRUE)
        write(paste("PostResample TREE ke- ",i,sep=""),append=TRUE,fileCOnnR)
        postrrektree<-postResample(predict(oneTree,testData),testData $CLASS)
        capture.output(postrrektree,file=fileCOnnR,append =TRUE)
        write(paste("PostResample RULE ke- ",i,sep=""),append=TRUE,fileCOnnR)
        postrrekrule<-postResample(predict(rules,testData),testData $CLASS)
        capture.output(postrrekrule,file=fileCOnnR,append =TRUE)
        
        write(paste("Confusion Matrix TREE ke- ",i,sep=""),append=TRUE,fileCOnnR)
        cmtree<-confusionMatrix(oneTreePred,testData$CLASS)
        capture.output(cmtree,file=fileCOnnR,append =TRUE)
        write(paste("Confusion Matrix RULE ke- ",i,sep=""),append=TRUE,fileCOnnR)
        cmrule<-confusionMatrix(predictRule,testData $CLASS)
        capture.output(cmrule,file=fileCOnnR,append =TRUE)
        
        #pohon keputusan
        tryCatch({
        png(filename=paste(lokasi,"Tree\\tree_",filedata,"_",i,".png",sep=""),width = 1366, height = 768)
        plot(oneTree,type="simple")
        dev.off()
        }, error=function(e){cat("ERROR pohon ke-",i,": ",conditionMessage(e), "\n")})
        
        #Contain matrix to export excel
        #matrix export excel count of training and test data
        dt[i,1] <- nrow(trainData) 
        dt[i,2] <- nrow(testData)
        #matrix export excel table data training
        dtl[i,1] <- trtest["BAIK"] 
        dtl[i,2] <- trtest["CUKUP"]
        #matrix export excel table data test
        dtl[i,5] <- ttest["BAIK"] 
        dtl[i,6] <- ttest["CUKUP"]
        #matrix export excel akurasi pohon keputusan
        dtakurasi[i,1] <- i
        dtakurasi[i,2] <- formatC(100*mean(akurasi),digits = 2, format = "f")
        #matrix export excel auc
        dtauc[i,1]<-i
        dtauc[i,2]<-formatC(roc.auc@y.values[[1]],digits=2,format="f")
        
        #matrix export excel akurasi pohon keputusan
        dtakurasirule[i,1] <- i
        dtakurasirule[i,2] <- formatC(100*mean(akurasiRule),digits = 2, format = "f")
        
      }
    write.xlsx(x = dt, file = paste(lokasi,"Jumlah_Uji_latih\\ujilatih_",filedata,".xlsx",sep=""),sheetName = "test", row.names = FALSE, col.names=FALSE)
    write.xlsx(x = dtl, file = paste(lokasi,"Jumlah_Uji_latih\\kelaslatih_",filedata,".xlsx",sep=""),sheetName = "test", row.names = FALSE, col.names=FALSE)
    write.xlsx(x = dtakurasi, file = paste(lokasi,"Akurasi\\Akurasi_",filedata,".xlsx",sep=""),sheetName = "test", row.names = FALSE, col.names=FALSE)
    write.xlsx(x = dtakurasirule, file = paste(lokasi,"rl\\Akurasi\\Akurasirule_",filedata,".xlsx",sep=""),sheetName = "test", row.names = FALSE, col.names=FALSE)
    tryCatch({
    write.xlsx(x = dtauc, file = paste(lokasi,"Luas AUC\\Auc_",filedata,".xlsx",sep=""),sheetName = "test", row.names = FALSE, col.names=FALSE)
    write.xlsx(x = dtaucrule, file = paste(lokasi,"rl\\Luas AUC\\Auc_",filedata,".xlsx",sep=""),sheetName = "test", row.names = FALSE, col.names=FALSE)
      
    }, error=function(e){cat("ERROR luas auc ke-",i,": ",conditionMessage(e), "\n")}) 
    