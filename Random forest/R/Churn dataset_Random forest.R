#DecissionTree
install.packages("C50")
library(C50)
data(churn)
str(churnTrain)
View(churnTrain)
table(churnTrain$churn)
# % of customers have churned  
table(churnTrain$churn)/nrow(churnTrain)
#To keep only plan and usage type variable in the data
churnTrain<-churnTrain[,4:20]
#for breaking the dataset into training and test past
library(caTools)
set.seed(2)
split<-sample.split(churnTrain,SplitRatio = 0.7)
train<-subset(churnTrain,split==T)
test<-subset(churnTrain,split==F)
#to build decission tree
library(randomForest)
churn_rf<-randomForest(churn~)



library(rpart)
ch_rp<-rpart(churn~.,data=train)
plot(ch_rp)
text(ch_rp)
#From line # 25 to 29 not required
##############################################################
install.packages("rattle")#for a fancy table plotting, we use rattle package
library(rattle)
fancyRpartPlot(ch_rp)
split_1<-subset(train,train$total_day_minutes>=264)
table(split_1$churn)/nrow(split_1)
###############################################################

#prediction
pred<-predict(ch_rp,test,type="class")
cm<-table(test$churn,pred)
library(caret)
confusionMatrix(cm)
#The accuracy is 94!!!
