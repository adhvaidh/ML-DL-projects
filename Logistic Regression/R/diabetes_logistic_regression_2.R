dia<-read.csv(choose.files(), header = T)
head(dia)
dia<-dia[,-4]
str(dia)

dia
library(caTools)
set.seed(238)

split<- sample.split(dia$Is_Diabetic,SplitRatio = 0.75)
train<-subset(dia,split==T)
nrow(test)
test<-subset(dia,split==F)
names(dia)
reg<-glm(Is_Diabetic~.-Age, data=train, family=binomial)
summary(reg)
rpred<-predict(reg,newdata=test, type = "response")
summary(rpred)
dia_with_p<-cbind(dia,rpred)
View(dia_with_p)
Dia_with_binary=transform(dia_with_p,rpred_bin=ifelse(rpred>0.5,"YES","NO"))
View(Dia_with_binary)
table(Dia_with_binary$Is_Diabetic)
freq_table<-table(Dia_with_binary$Is_Diabetic,Dia_with_binary$rpred_bin)
freq_table
library(caret)
library(e1071)
confusionMatrix(freq_table)


Dia_with_bin=transform(dia_with_p,rpred_bin_6=ifelse(rpred>0.6,"YES","NO"))
View(Dia_with_bin)
table(Dia_with_bin$Is_Diabetic)
freq_table_1<-table(Dia_with_bin$Is_Diabetic,Dia_with_bin$rpred_bin)
library(caret)

confusionMatrix(freq_table_1)


Dia_with_bin=transform(dia_with_p,rpred_bin_6=ifelse(rpred>0.7,"YES","NO"))
View(Dia_with_bin)
table(Dia_with_bin$Is_Diabetic)
freq_table_2<-table(Dia_with_bin$Is_Diabetic,Dia_with_bin$rpred_bin)
library(caret)

confusionMatrix(freq_table_2)


Dia_with_bin=transform(dia_with_p,rpred_bin_6=ifelse(rpred>0.4,"YES","NO"))
View(Dia_with_bin)
table(Dia_with_bin$Is_Diabetic)
freq_table_3<-table(Dia_with_bin$Is_Diabetic,Dia_with_bin$rpred_bin)
library(caret)

confusionMatrix(freq_table_3)


Dia_with_bin=transform(dia_with_p,rpred_bin_6=ifelse(rpred>0.3,"YES","NO"))
View(Dia_with_bin)
table(Dia_with_bin$Is_Diabetic)
freq_table_4<-table(Dia_with_bin$Is_Diabetic,Dia_with_bin$rpred_bin)
library(caret)

confusionMatrix(freq_table_4)


Dia_with_bin=transform(dia_with_p,rpred_bin_6=ifelse(rpred>0.65,"YES","NO"))
View(Dia_with_bin)
table(Dia_with_bin$Is_Diabetic)
freq_table_5<-table(Dia_with_bin$Is_Diabetic,Dia_with_bin$rpred_bin)
library(caret)

confusionMatrix(freq_table_5)
