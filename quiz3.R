# Question 1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
q1data = segmentationOriginal
library(caret)
set.seed(125)
train_crit <- createDataPartition(y=q1data$Case, p=0.75, list = FALSE)
train_set <- q1data[train_crit,]
test_set <- q1data[-train_crit,]
set.seed(125)
model1 <- train(q1data$Class ~ ., data=q1data, method='rpart')
require(rattle)
rattle::fancyRpartPlot(model1$finalModel)
print(model1$finalModel)
fancyRpartPlot(model1$finalModel)

# Question 3
library(pgmm)
data(olive)
olive = olive[,-1]
model2 <- train(olive$Area ~ ., data=olive, method='rpart')
newdata = as.data.frame(t(colMeans(olive)))
preddata <- predict(model2, newdata)


# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
model3 <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
                data=trainSA, method='glm', family='binomial')
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
train_pred <- predict(model3)
test_pred <- predict(model3, testSA)
train_mc <- missClass(trainSA$chd, train_pred)
test_mc <- missClass(testSA$chd, test_pred)


# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
model5 <- train(y ~ ., data=vowel.train,method="rf",prox=TRUE)
model5$finalModel
varImp(model5)
