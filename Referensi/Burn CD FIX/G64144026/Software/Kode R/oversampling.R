
table(ds_3k_ALL_v2_filter1$CLASS);

data1<-ds_3k_ALL_v2_filter1
#data2<-ds_3k_ALL_v2_filter1
g1 <- runif(nrow(data1));
g1;
g2 <- runif(nrow(data2));
g2;
data1_olah<- data1[order(g1),];
data2_olah<- data2[order(g2),];
str(data1_olah);
str(data2_olah);
datatraining_1 <- data1_olah
datatraining_2 <- data2_olah
data1_olah_oversampling <- ovun.sample(CLASS~., data=datatraining_1,
                                         p=0.5, 
                                         seed=1, method="over")$data
data2_olah_oversampling <- ovun.sample(CLASS~., data=datatraining_2,
                                          p=0.5, 
                                          seed=1, method="over")$data

write.csv(data1_olah_oversampling, "D:hasilover1.csv")
write.csv(data2_olah_oversampling, "D:hasilover2.csv")


