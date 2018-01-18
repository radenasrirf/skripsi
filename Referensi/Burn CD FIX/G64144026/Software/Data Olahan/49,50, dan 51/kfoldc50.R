#get the data from somewhere and specify number of folds
setwd("C:\\Users\\ASUS K43TA\\Desktop\\SIDANG SEMINAR\\1 SEMINAR\\Hasil Olahan\\49,50, dan 51")
data <- read.csv(file="ds2kall.csv",head=TRUE,sep=",",fill=TRUE)
nrFolds <- 10
# generate array containing fold-number for each sample (row)
model_all <- list()
p_all <- list()
akurasi_all <- 0
conf_mat_all <- list()
trainX_all <- list()
trainY_all <- list()
testX_all <- list()
testY_all <- list()
# actual cross validation
for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- data[-fold,]
  data.test <- data[fold,]
  trainX <- data.train[,1:15]
  testX <- data.test[,1:15]
  trainY <-data.train[,16]
  testY <- data.test[,16]
  model <- C5.0( trainX, trainY )
  p <- predict( model, testX, type="CLASS" )
  akurasi<- sum( p == testY ) / length( p )
  model_all[[k]] <- model
  p_all[[k]] <- p
  akurasi_all[k] <- akurasi
  conf_mat_all[[k]] <- table(testY, p )
  trainX_all[[k]] <- trainX
  trainY_all[[k]] <- trainY
  testX_all[[k]] <- testX
  testY_all[[k]] <- testY
  #plot(model_all[[k]])
  # train and test your model with data.train and data.test
}
#untuk melihat jumlah data test untuk setiap fold gunakan ini
for(k in 1:nrFolds){
  print(paste0("fold",k))
  print(table(testY_all[[k]]))
}
#untuk melihat jumlah data train untuk setiap fold gunakan ini
for(k in 1:nrFolds){
  print(paste0("fold",k))
  print(table(trainY_all[[k]]))
}