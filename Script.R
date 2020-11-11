library(tidyr)
library(dplyr)
library(ggplot2)
library(caret)
library(here)
library(corrplot)


#mengimport file csv
df <- read.csv(here("creditcard.csv"))

#melihat ringkasan dataframe
str(df)
summary(df)

#cek jumlah data kosong dan data na
sum(is.null(df))
sum(is.na(df))

#melihat perbandingan data fraud dan non frauds
p_f <- round(sum(df$Class==1)/nrow(df)*100,2)
p_nf <- round(sum(df$Class==0)/nrow(df)*100,2)
sprintf("Persentase data yang bersifat frauds adalah %g persen",p_f)
sprintf("Persentase data yang bersifat non-frauds adalah %g persen",p_nf)

#memplot distribusi waktu dan jumlah per transaksi
ggplot(df,aes(x=Time))+
  geom_histogram()+
  labs(title = "Distribusi Waktu Transaksi")

ggplot(df,aes(x=Amount))+
  geom_histogram()+
  labs(title ="Distribusi Jumlah Transaksi")

#membuat dataset baru tanpa kolom time dan amount untuk sampling
data<- subset(df,select = -c(Time,Amount))

#membuat skala kolom time dan amount agar ternormalisasi di dataset baru
data$scale_time <- scale(df$Time)
data$scale_amount <- scale(df$Amount)

#membuat dataset baru tanpa kolom time dan amount untuk sampling
data<- subset(df,select = -c(Time,Amount))

#membuat skala kolom time dan amount agar ternormalisasi di dataset baru
data$scale_time <- scale(df$Time)
data$scale_amount <- scale(df$Amount)

#membuat dataset baru untuk yang fraud dan non fraud
fraud_df <- filter(data,Class==1)
nfraud_df <- filter(data,Class==0)  

#Melakukan undersampling
set.seed(50)
sample_num <- sample(nrow(nfraud_df),nrow(fraud_df))
under_sampling <- nfraud_df[sample_num,]

#membuat dataset baru hasil undersampling
new_df <- rbind(fraud_df,under_sampling)

#Plot hasil sampling 
ggplot(new_df,aes(x=Class))+
  geom_bar()+
  scale_x_discrete(labels=c('non-fraud','fraud'))


#melihat korelasi matrix antar variabel
cor_mat <- cor(new_df)
corrplot(cor_mat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Melihat mana yang korelasi paling berpengaruh ke Class
matrix <- cor_mat[,"Class"]
# 5 korelasi positif tertinggi terhadap variabel Class
print(tail(sort(matrix),5,decreasing=TRUE))
# 5 korelasi negatif tertinggi terhadap variabel Class
print(head(sort(matrix),5,decreasing=TRUE))

#menghapus data outlier berdasarkan v14,v4,v11,v12,v10

##Mendapatkan nilai yg fraud di V14
v14_fraud <- new_df%>%
  filter(Class==1)


v14_q25 <- quantile(v14_fraud$V14,0.25)
sprintf("kuartil 25 v14 adalah %g",v14_q25)
v14_q75 <- quantile(v14_fraud$V14,0.75)
sprintf("kuartil 75 v14 adalah %g",v14_q75)
v14_iqr <- v14_q75 - v14_q25
sprintf("Jarak Interkuartil v14 adalah %g",v14_iqr)

v14_upper <- v14_q75 + v14_iqr*1.5
sprintf("V14 Upper : %g",v14_upper)
v14_lower <- v14_q25 - v14_iqr*1.5
sprintf("V14 Lower : %g",v14_lower)

outlier_v14 <- which(v14_fraud$V14<v14_lower|v14_fraud$V14>v14_upper) 
data_outlier_v14 <- v14_fraud[outlier_v14,"V14"]
print(data_outlier_v14)
length(data_outlier_v14)

#menghilangkan data outlier dari dataset 
if(length(outlier_v14)>0){
  new_df <- new_df[-outlier_v14,]
} else print("Tidak ada data outlier")


##Mendapatkan nilai yg fraud di V4
v4_fraud <- new_df%>%
  filter(Class==1)


v4_q25 <- quantile(v4_fraud$V4,0.25)
sprintf("kuartil 25 v4 adalah %g",v4_q25)
v4_q75 <- quantile(v4_fraud$V4,0.75)
sprintf("kuartil 75 v4 adalah %g",v4_q75)
v4_iqr <- v4_q75 - v4_q25
sprintf("Jarak Interkuartil v4 adalah %g",v4_iqr)

v4_upper <- v4_q75 + v4_iqr*1.5
sprintf("V4 Upper : %g",v4_upper)
v4_lower <- v4_q25 - v4_iqr*1.5
sprintf("V4 Lower : %g",v4_lower)

outlier_v4 <- which(v4_fraud$V4<v4_lower|v4_fraud$V4>v4_upper) 
data_outlier_v4 <- v4_fraud[outlier_v4,"V4"]
print(data_outlier_v4)
length(data_outlier_v4)

#menghilangkan data outlier dari dataset
if(length(outlier_v4)>0){
  new_df <- new_df[-outlier_v4,]
} else print("Tidak ada data outlier")


##Mendapatkan nilai yg fraud di V11
v11_fraud <- new_df%>%
  filter(Class==1)


v11_q25 <- quantile(v11_fraud$V11,0.25)
sprintf("kuartil 25 v11 adalah %g",v11_q25)
v11_q75 <- quantile(v11_fraud$V11,0.75)
sprintf("kuartil 75 v11 adalah %g",v11_q75)
v11_iqr <- v11_q75 - v11_q25
sprintf("Jarak Interkuartil v11 adalah %g",v11_iqr)

v11_upper <- v11_q75 + v11_iqr*1.5
sprintf("V11 Upper : %g",v11_upper)
v11_lower <- v11_q25 - v11_iqr*1.5
sprintf("V11 Lower : %g",v11_lower)

outlier_v11 <- which(v11_fraud$V11<v11_lower|v11_fraud$V11>v11_upper) 
data_outlier_v11 <- v11_fraud[outlier_v11,"V11"]
print(data_outlier_v11)
length(data_outlier_v11)

#menghilangkan data outlier dari dataset 
if(length(outlier_v11)>0){
  new_df <- new_df[-outlier_v11,]
} else print("Tidak ada data outlier")


##Mendapatkan nilai yg fraud di V12
v12_fraud <- new_df%>%
  filter(Class==1)


v12_q25 <- quantile(v12_fraud$V12,0.25)
sprintf("kuartil 25 v12 adalah %g",v12_q25)
v12_q75 <- quantile(v12_fraud$V12,0.75)
sprintf("kuartil 75 v12 adalah %g",v12_q75)
v12_iqr <- v12_q75 - v12_q25
sprintf("Jarak Interkuartil v12 adalah %g",v12_iqr)

v12_upper <- v12_q75 + v12_iqr*1.5
sprintf("V12 Upper : %g",v12_upper)
v12_lower <- v12_q25 - v12_iqr*1.5
sprintf("V12 Lower : %g",v12_lower)

outlier_v12 <- which(v12_fraud$V12<v12_lower|v12_fraud$V12>v12_upper) 
data_outlier_v12 <- v12_fraud[outlier_v12,"V12"]
print(data_outlier_v12)
length(data_outlier_v12)
#menghilangkan data outlier dari dataset 
if(length(outlier_v12)>0){
  new_df <- new_df[-outlier_v12,]
} else print("Tidak ada data outlier")


##Mendapatkan nilai yg fraud di V10
v10_fraud <- new_df%>%
  filter(Class==1)


v10_q25 <- quantile(v10_fraud$V10,0.25)
sprintf("kuartil 25 v10 adalah %g",v10_q25)
v10_q75 <- quantile(v10_fraud$V10,0.75)
sprintf("kuartil 75 v10 adalah %g",v10_q75)
v10_iqr <- v10_q75 - v10_q25
sprintf("Jarak Interkuartil v10 adalah %g",v10_iqr)

v10_upper <- v10_q75 + v10_iqr*1.5
sprintf("V10 Upper : %g",v10_upper)
v10_lower <- v10_q25 - v10_iqr*1.5
sprintf("V10 Lower : %g",v10_lower)

outlier_v10 <- which(v10_fraud$V10<v10_lower|v10_fraud$V10>v10_upper) 
data_outlier_v10 <- v10_fraud[outlier_v10,"V10"]
print(data_outlier_v10)
length(data_outlier_v10)

#menghilangkan data outlier dari dataset
if(length(outlier_v10)>0){
  new_df <- new_df[-outlier_v10,]
} else print("Tidak ada data outlier")


#mengubah variabel Class menjadi factor N= Non-fraud dan F= Frauds
new_df$Class <- factor(new_df$Class,levels = c(0,1),labels = c("N","F"))

#Membuat model dengan caret beserta cv

##membuat object hypertuning parameter
set.seed(50)
mycontrol <- trainControl(method = "cv",
                          number = 10,
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          verboseIter = TRUE)

##membuat model klasifikasi dengan random forest
model_rf <- train( Class ~ .,
                    data = new_df,
                    tuneLength=3,
                    metric = "ROC",
                    method = "ranger",
                    trControl = mycontrol)
print(model_rf)
max(model_rf[["results"]][["ROC"]])


##membuat model klasifikasi dengan glmnet
model_glmnet <- train( Class ~ .,
                   data = new_df,
                   tuneLength=3,
                   metric = "ROC",
                   method = "glmnet",
                   trControl = mycontrol)
print(model_glmnet)
max(model_glmnet[["results"]][["ROC"]])

##membuat model klasifikasi dengan xgbDart
model_xgbdart <- train( Class ~ .,
                       data = new_df,
                       tuneLength=3,
                       metric = "ROC",
                       method = "xgbDART",
                       trControl = mycontrol)
print(model_xgbdart)
max(model_xgbdart[["results"]][["ROC"]])

##membuat model klasifikasi dengan SVM
model_svm <- train( Class ~ .,
                        data = new_df,
                        tuneLength=3,
                        metric = "ROC",
                        method = "svmRadial",
                        trControl = mycontrol)
print(model_svm)
max(model_svm[["results"]][["ROC"]])

##membuat model klasifikasi dengan KNN
model_knn <- train( Class ~ .,
                    data = new_df,
                    tuneLength=3,
                    metric = "ROC",
                    method = "knn",
                    trControl = mycontrol)
print(model_knn)
max(model_knn[["results"]][["ROC"]])


#Membandingkan performa model 
list_model <- list(RF=model_rf,GLMNET=model_glmnet,XGBDART=model_xgbdart,SVM=model_svm,KNN=model_knn)
model_compare <- resamples(list_model)
summary(model_compare)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(model_compare, scales=scales,metric = "ROC")


