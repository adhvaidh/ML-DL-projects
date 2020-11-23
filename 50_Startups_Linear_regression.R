strtup <- read.csv(choose.files(), header = T)
colnames(strtup)
strtup <- strtup[,-4]
colnames(strtup)
colSums(is.na(strtup))
library(caTools)
set.seed(123)
split<-sample.split(strtup$Profit, SplitRatio = 0.75)
training <- subset(strtup, split==T)
training
test <- subset(strtup, split==F)
test
regmod <- lm(Profit~.,data=training)
summary(regmod)
regpred <- predict(regmod, newdata=test)
regpred
regPred_1 <- cbind(test$Profit, regpred)
View(regPred_1)
