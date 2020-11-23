comp<-read.csv(choose.files(), header = TRUE)
View(comp)
colSums(is.na(comp))
summary(comp)
library(caTools)
set.seed(1231)
spl<-sample.split(comp$price, SplitRatio = 0.75)
spl
training <- subset(comp, spl==TRUE)
training
nrow(training)
test <- subset(comp, spl==FALSE)
test
nrow(test)
colnames(comp)
rmod_1 <- lm(formula = price~., data = training)
rmod_1
summary(rmod_1)
regPred <- predict(rmod_1, newdata = test)
regPred
regPred_comp <- cbind(test$price,regPred)
regPred_comp
View(regPred_comp)

