dia<-read.csv(choose.files(), header = T)
head(dia)
dia<-dia[,-4]
str(dia)

dia
library(caTools)
set.seed(123)

split<- sample.split(dia$Is_Diabetic,SplitRatio = 0.75)
train<-subset(dia,split==T)
nrow(test)
test<-subset(dia,split==F)
library(rpart)
d_rp<-rpart(Is_Diabetic~.,data=train)
plot(d_rp)
text(d_rp)
pred<-predict(d_rp,newdata=test,type="class")
cm <- table(test$Is_Diabetic, pred)
library(caret)
confusionMatrix(cm)
#########################
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
########################################################
############Bayes naive model#############
library(e1071)
NB_Dia=naiveBayes(Is_Diabetic~.,data=train)
Predicted_Diabetic=predict(NB_Dia,newdata = test,type = "class")
Test_Final=cbind(test,Predicted_Diabetic)
cm=table(Test_Final$Is_Diabetic,Test_Final$Predicted_Diabetic)
cm
confusionMatrix(cm)
########################################################