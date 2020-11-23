USA1 <- read.csv(choose.files(), header = TRUE)
View(USA1)
colnames(USA1)
USA1 <- USA1[,-7]
head(USA1)
colSums(is.na(USA1))
library(caTools)
set.seed(123)
split <- sample.split(USA1$Price, SplitRatio = 0.7)
table(split)
train <- subset(USA1, split==T)
head(train)
train
test <- subset(USA1, split==F)
test
colnames(USA1)
hmod_1=lm(formula = Price~.-Avg..Area.Number.of.Bedrooms, data=train)
hmod_1
summary(hmod_1)
rpred <- predict(hmod_1, newdata=test)
rpred
rpred_1 <- cbind(test$Price,rpred)
View(rpred_1)
