ti<-read.csv(choose.files(),header=T)
head(ti)
ti<-ti[,c(2:5,13,22,25,28)]
str(ti)
ti$X2urvived<-factor(ti$X2urvived)
library(caTools)
set.seed(123)
split<-sample.split(ti$X2urvived,SplitRatio = 0.75)
train<-subset(ti,split==T)
test<-subset(ti,split==F)
t_reg<-glm(X2urvived~.,data=train,family=binomial)
t_pred<-predict(t_reg,newdata = test,type="response")
twpv<-cbind(test,t_pred)
twbpv<-transform(twpv,t_pred_t=ifelse(t_pred>0.5,1,0))
freq1<-table(twbpv$X2urvived,twbpv$t_pred_t)
library(caret)
library(ggplot2)
library(lattice)
confusionMatrix(freq1)
##########   Accuracy = 0.7927 ######### 
########################################################
#################  0.65       ##########

twpv_2<-cbind(test,t_pred)
twbpv_2<-transform(twpv_2,t_pred_t2=ifelse(t_pred>=0.65,1,0))
freq_2<-table(twbpv_2$X2urvived,twbpv_2$t_pred_t2)
confusionMatrix(freq_2)
###########  Accuracy = 0.7652 #########
########################################################
###########             0.8               ##############
twpv_3<-cbind(test,t_pred)
twbpv_3<-transform(twpv_3,t_pred_t3=ifelse(t_pred>=0.7,1,0))
freq_3<-table(twbpv_3$X2urvived,twbpv_3$t_pred_t3)
confusionMatrix(freq_3)
###########  Accuracy = 0.7561 #########
########################################################
###########             0.4               ##############
twpv_4<-cbind(test,t_pred)
twbpv_4<-transform(twpv_4,t_pred_t4=ifelse(t_pred>=0.4,1,0))
freq_4<-table(twbpv_4$X2urvived,twbpv_4$t_pred_t4)
confusionMatrix(freq_4)
###########  Accuracy = 0.7866 #########
########################################################
###########             0.3               ##############
twpv_5<-cbind(test,t_pred)
twbpv_5<-transform(twpv_5,t_pred_t5=ifelse(t_pred>=0.3,1,0))
freq_5<-table(twbpv_5$X2urvived,twbpv_5$t_pred_t5)
confusionMatrix(freq_5)
###########  Accuracy = 0.7439 #########
##########  Final cut off = 0.5 ########
########################################################
library(ggplot2)
library(ROCR)
ROCPred = prediction(twpv$t_pred,twpv$X2urvived)
ROCPred
ROCperformance = performance(ROCPred,"tpr","fpr")
plot(ROCperformance,col="blue",print.cutoffs.at=seq(.1,by=.1))
abline(a=0, b=1)                     
########################################################
#################   Naive Bayes Theorem   ##############
library(e1071)
nbt=naiveBayes(X2urvived~.,data=train)
t_pred=predict(nbt,newdata = test,type = "class")
fin_tst=cbind(test,t_pred)
cm=table(fin_tst$X2urvived,fin_tst$t_pred)
cm
confusionMatrix(cm)
###############  Accuracy = 0.7591  ###################
#######################################################
##################  Decission Tree  ###################
library(rpart)
Dtree<-rpart(X2urvived~.,data=train)
pred<-predict(Dtree,newdata=test,type="class")
cm <- table(test$X2urvived, pred)
library(caret)
confusionMatrix(cm)
plot(Dtree)
text(Dtree)
##############   Accuracy = 0.8018  ##################